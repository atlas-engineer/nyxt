;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'define-parenscript)
(defmacro define-parenscript (script-name args &body script-body)
  "Define parenscript function SCRIPT-NAME.
SCRIPT-BODY must be a valid parenscript and will be wrapped in (PS:PS ...).
Any Lisp expression must be wrapped in (PS:LISP ...).

The returned function sends the compiled Javascript to the current buffer webview.
The function can be passed ARGS."
  `(defun ,script-name ,args
     (ffi-buffer-evaluate-javascript (current-buffer)
                                     (ps:ps ,@script-body))))

(export-always 'pflet)
(defmacro pflet (functions &body body)
  (flet ((transform-definition (name args body)
           `(,name ,args
                   (ffi-buffer-evaluate-javascript (current-buffer) (ps:ps . ,body)))))
    `(flet ,(loop for (name lambda-list . body) in functions
                  collect (transform-definition name lambda-list body))
       ,@body)))

(export-always 'peval)
(defmacro peval (&body body)
  `(ffi-buffer-evaluate-javascript (current-buffer) (ps:ps ,@body)))

(define-parenscript %document-scroll-position (&optional (y 0 y-provided-p) (x 0 x-provided-p))
  (let ((x (ps:lisp x))
        (y (ps:lisp y)))
    (if (or (ps:lisp x-provided-p) (ps:lisp y-provided-p))
        (ps:chain window (scroll-to x y))
        (list (ps:chain window page-y-offset)
              (ps:chain window page-x-offset)))))

(export-always 'document-scroll-position)
(defmethod document-scroll-position (&optional (buffer (current-buffer)))
  "Get current scroll position or set it.
If passed no arguments, return a list of two elements: vertical (Y) and
horizontal (X) offset.
If `setf'-d to a single value (or a single list) -- set Y to it.
If `setf'-d to a list of two values -- set Y to `first' and X to `second' element."
  (with-current-buffer buffer
    (let ((position (%document-scroll-position)))
      (when (listp position)
        position ))))

(defmethod (setf document-scroll-position) (value &optional (buffer (current-buffer)))
  (when value
    (with-current-buffer buffer
      (destructuring-bind (y &optional x)
          (uiop:ensure-list value)
        (%document-scroll-position y x)))))

(export-always 'document-get-paragraph-contents)
(define-parenscript document-get-paragraph-contents (&key (limit 100000))
  (let ((result ""))
    (loop for element in (nyxt/ps:qsa document (list "p"))
          do (setf result (+ result
                             (ps:chain element text-content))))
    (ps:chain result (slice 0 (ps:lisp limit)))))

(export-always '%paste)
(define-parenscript %paste (&key (input-text (ring-insert-clipboard (clipboard-ring *browser*))))
  (let ((active-element (ps:chain document active-element))
        (tag (ps:chain document active-element tag-name)))
    (when (nyxt/ps:element-editable-p active-element)
      (nyxt/ps:insert-at active-element (ps:lisp input-text)))))

(export-always '%copy)
(define-parenscript %copy ()
  "Return selected text from javascript."
  (ps:chain window (get-selection) (to-string)))

(define-parenscript %cut ()
  (let ((active-element (ps:chain document active-element)))
    (when (nyxt/ps:element-editable-p active-element)
      (let ((selection-text (ps:chain window (get-selection) (to-string))))
        (nyxt/ps:insert-at active-element "")
        selection-text))))

(define-parenscript %select-all ()
  (let ((active-element (ps:chain document active-element)))
    (when (nyxt/ps:element-editable-p active-element)
      (ps:chain active-element (set-selection-range 0 (ps:@ active-element value length))))))

(defun html-write (content &optional (buffer (current-buffer)))
  (ffi-buffer-evaluate-javascript-async
   buffer
   (ps:ps (ps:chain document
                    (write (ps:lisp content))))))

(defun html-set (content &optional (buffer (current-buffer)))
  (ffi-buffer-evaluate-javascript-async
   buffer
   (ps:ps (setf (ps:@ document body |innerHTML|)
                (ps:lisp content)))))

(defun html-set-style (style-string &optional (buffer (current-buffer)))
  (let ((style (spinneret:with-html-string (:style style-string))))
    (ffi-buffer-evaluate-javascript-async
     buffer
     (ps:ps (ps:chain document body
                      (|insertAdjacentHTML| "afterbegin"
                                            (ps:lisp style)))))))

(sera:eval-always
  (defvar *nyxt-url-commands* (make-hash-table) ; TODO: Rename to `*internal-pages-command-list*'.
    "A map from allowed nyxt: URLs symbols to the functions that generate code of
  the pages related to these commands."))

(defun internal-page-symbol-p (sym)
  (gethash sym *nyxt-url-commands*))

(deftype internal-page-symbol ()
  `(and symbol (satisfies internal-page-symbol-p)))

(define-class internal-page (command)
  ((dynamic-title ; Not `title' so that it does not clash with other `title' methods.
    ""
    :initarg :title
    :accessor nil
    :type (or string function)
    :documentation "If a function, it is called with the internal page arguments
and must return a string.")
   (page-mode
    nil
    :export t
    :type symbol
    :documentation "The mode that's specific to a nyxt:// page.
It's automatically enabled when the page is loaded and disabled when another URL
is loaded.")
   (form
    nil
    :initarg nil
    :writer nil
    :reader t
    :type (maybe function)
    :documentation "Function that returns HTML content when a nyxt:// URL is
invoked.
The nyxt:// URL query arguments are passed to this function as keyword arguments."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Each instance is a unique internal page generator for the
nyxt:// URL scheme.

Register a new nyxt:// URL under NAME.
When loaded, BODY is run to populate the page content.

WARNING: Don't run anything sensitive in the BODY as any third-party page can
load nyxt:// URLs.

BODY should end with a form returning the HTML body as a string.

ARGLIST is arguments for the underlying page-generating
function. Any argument from it is safe to use in the body of this macro.
Beware: the ARGLIST should have nothing but keyword arguments because it's
mapped to the URL query parameters.
Only Lisp values that can be converted to JavaScript with
`webkit:lisp-to-jsc-value' are accepted.

See `find-internal-page-buffer'."))

(defmethod (setf form) (lambda-expression (page internal-page))
  (let ((arglist (second lambda-expression)))
    (multiple-value-bind (required optional rest keyword allow-other-keys-p aux key-p)
        (alex:parse-ordinary-lambda-list arglist)
      (declare (ignore keyword allow-other-keys-p key-p))
      (when (or required optional rest aux)
        (error "Only keyword parameters are allowed in an internal-page definition."))
      (setf (slot-value page 'form)
            (lambda (&rest args)
              (declare (ignorable args))
              (let ((*print-pretty* nil))
                (values (spinneret:with-html-string
                          (:head
                           (:title (progn
                                     (apply #'dynamic-title (gethash (name page) *nyxt-url-commands*)
                                            args)))
                           (:style (style (current-buffer))))
                          (:body
                           (:raw (apply (compile nil lambda-expression) args))))
                        "text/html;charset=utf8")))))))

(defmethod set-internal-page-method ((page internal-page) form)
  (when form
    (let* ((arglist (second form))
           (keywords (nth-value 3 (alex:parse-ordinary-lambda-list arglist)))
           (body (cddr form))
           (documentation (nth-value 2 (alex:parse-body body :documentation t))))
      (closer-mop:ensure-method
       page
       `(lambda (,@arglist)
          ,@(when documentation (list documentation))
          (declare (ignorable ,@(mappend #'cdar keywords)))
          (set-current-buffer
           (buffer-load (nyxt-url (name ,page) ,@(mappend #'first keywords))
                        :buffer (ensure-internal-page-buffer (name ,page)))))))))

(defmethod initialize-instance :after ((page internal-page) &key form &allow-other-keys)
  "Register PAGE into the globally known nyxt:// URLs."
  (when form
    (set-internal-page-method page form)
    (setf (form page) form))
  (setf (gethash (name page) *nyxt-url-commands*) page))

(defmethod reinitialize-instance :after ((page internal-page) &key form &allow-other-keys)
  "Register PAGE into the globally known nyxt:// URLs."
  (when form
    (set-internal-page-method page form)
    (setf (form page) form))
  (setf (gethash (name page) *nyxt-url-commands*) page))

(defmethod dynamic-title ((page internal-page) &rest args)
  (with-slots ((title dynamic-title)) page
    (cond
      ((stringp title)
       title)
      ((functionp title)
       (apply title args))
      (t
       (format nil "*~a*" (string-downcase (name page)))))))

(defun internal-page-name (url)
  (read-from-string (str:upcase (quri:uri-path url))))

;; (-> find-internal-page-buffer (internal-page-symbol) (maybe buffer))
(defun find-internal-page-buffer (name) ; TODO: Test if CCL can catch bad calls at compile-time.
  "Return first buffer which URL is a NAME `internal-page'."
  (find name (buffer-list) :key (compose #'internal-page-name #'url)))

(defun find-url-internal-page (url)
  "Return the `internal-page' to which URL corresponds."
  (and (equal "nyxt" (quri:uri-scheme url))
       (gethash
        (internal-page-name url)
        *nyxt-url-commands*)))

(export-always 'ensure-internal-page-buffer)
(defun ensure-internal-page-buffer (name)
  "Return first buffer which URL is a NAME internal page, or create it if it does not exist."
  (or (find-internal-page-buffer name)
      (make-instance 'web-buffer)))

(export-always 'define-internal-page)
(defmacro define-internal-page (name (&rest form-args) (&rest initargs) &body body)
  "Define an `internal-page'.
FORM-ARGS or the `internal-page' `form' keyword arguments.
`INITARGS' are passed to the `internal-page' initialization arguments

Example:

\(define-internal-page my-page (&key arg1 arg2)
  (:title \"My beautiful page\")
  ...)"
  `(apply #'make-instance 'internal-page
          :name ',name
          :visibility :anonymous
          :lambda-list ',form-args
          :form (quote (lambda (,@form-args) ,@body))
          (list ,@initargs)))

(export-always 'define-internal-page-command)
(defmacro define-internal-page-command (name (&rest arglist)
                                        (buffer-var title &optional mode)
                                        &body body)
  "Define a command called NAME creating an `internal-page'.

Only keyword arguments are accepted."
  (multiple-value-bind (stripped-body declarations documentation)
      (alex:parse-body body :documentation t)
    `(progn
       (export-always ',name (symbol-package ',name))
       (sera:lret ((gf (defgeneric ,name (,@(generalize-lambda-list arglist))
                         (:documentation ,documentation)
                         (:generic-function-class internal-page))))
         (let ((wrapped-body '(lambda (,@arglist)
                               ,@(when documentation (list documentation))
                               ,@declarations
                               (let ((,buffer-var (current-buffer)))
                                 (declare (ignorable ,buffer-var))
                                 ,@stripped-body))))
           (set-internal-page-method gf wrapped-body)
           (setf (slot-value #',name 'visibility) :mode)
           (setf (slot-value #',name 'page-mode) ,mode)
           (setf (slot-value #',name 'dynamic-title)
                 ,(if (stringp title)
                      title
                      (let ((keywords (nth-value 3 (alex:parse-ordinary-lambda-list arglist))))
                        `(lambda (,@arglist)
                           (declare (ignorable ,@(mappend #'cdar keywords)))
                           ,title))))
           (setf (form gf) wrapped-body))))))

(export-always 'define-internal-page-command-global)
(defmacro define-internal-page-command-global (name (&rest arglist)
                                               ;; TODO: Move `buffer-var' (and `title'?) to a keyword arg.
                                               (buffer-var title &optional mode)
                                               &body body)
  "Define a global command called NAME creating an `internal-page'.

Only keyword arguments are accepted."
  `(prog1 (define-internal-page-command ,name (,@arglist) (,buffer-var ,title ,mode) ,@body)
     (setf (slot-value #',name 'visibility) :global)))

(defvar *json-object-accumulator* (make-hash-table :test 'equal)
  "Our own object accumulator to override the default `cl-json:decode-json' object->alist behavior.
Objects are transformed to the hash-tables instead.")

(defvar *json-last-object-key* nil
  "The last key used in `*json-object-accumulator*'.")

(defun json-object-init ()
  (setf *json-object-accumulator* (make-hash-table :test 'equal)))

(defun json-object-add-key (key)
  (setf (gethash key *json-object-accumulator*) nil
        *json-last-object-key* key))

(defun json-object-add-value (value)
  (setf (gethash *json-last-object-key* *json-object-accumulator*) value))

(defun json-object-get ()
  *json-object-accumulator*)

;; TODO: Decode arrays as vectors?
(sera:-> decode-json (string) t)
(export-always 'decode-json)
(defun decode-json (string)
  "An overridden version of `cl-json:decode-json-from-string'.
Distinguishes between null/false and arrays/objects.
Decodes:
- null as :NULL,
- undefined as :UNDEFINED,
- false as nil,
- true as t,
- objects as hash-tables.

Otherwise behaves like plain `cl-json:decode-json-from-string'."
  (let ((json::+json-lisp-symbol-tokens+
          '(("true" . t)
            ("false" . nil)
            ("null" . :null)
            ("undefined" . :undefined)))
        (json:*object-scope-variables* '(json:*internal-decoder* *json-object-accumulator* *json-last-object-key*))
        (json:*beginning-of-object-handler* #'json-object-init)
        (json:*object-key-handler* #'json-object-add-key)
        (json:*object-value-handler* #'json-object-add-value)
        (json:*end-of-object-handler* #'json-object-get))
    (json:decode-json-from-string string)))

(sera:-> encode-json (t) (values string &optional))
(export-always 'encode-json)
(defun encode-json (data)
  "Overridden version of `cl-json:encode-json-to-string'.
Distinguishes between null and false.
Encodes:
- :NULL as null,
- :UNDEFINED as undefined,
- nil as false.

Otherwise behaves the same as `cl-json:encode-json-to-string'."
  (let ((json::+json-lisp-symbol-tokens+
          '(("true" . t)
            ("false" . nil)
            ("null" . :null)
            ("undefined" . :undefined))))
    (json:encode-json-to-string data)))
