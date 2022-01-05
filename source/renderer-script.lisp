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

(defvar *nyxt-url-commands* (make-hash-table)
  "A map from allowed nyxt: URLs symbols to the functions that generate code of
  the pages related to these commands.")

(defmacro define-internal-page-command (name (&rest arglist)
                                        (buffer-var title mode)
                                        &body body)
  "Define a command called NAME creating an internal interface page.

Should end with a form returning HTML as a string.

Create a buffer (and bind it to BUFFER-VAR) in case there's no buffer with TITLE
and MODE. If there is one, bind BUFFER-VAR to it. Either way, BUFFER-VAR is
always a buffer that the generated code is loaded into.

ARGLIST is arguments for the command and for the underlying page-generating
function. Any argument from it is safe to use in the body of this macro.
Beware: the ARGLIST should have keyword arguments only because it's mapped to
query parameters."
  (let* ((internal-name (gensym (symbol-name name)))
         (args (alex:mappend #'first (nth-value 3 (alex:parse-ordinary-lambda-list arglist)))))
    (multiple-value-bind (body declarations documentation)
        (alex:parse-body body :documentation t)
      `(progn
         (flet ((,internal-name (,@arglist)
                  ,@(when documentation (list documentation))
                  ,@declarations
                  ;; We need to ignore those to avoid warnings, as the same arglist
                  ;; is used in both internal function and a command.
                  (declare (ignorable ,@(loop for arg in (rest args) by #'cddr
                                              collect arg)))
                  ;; TODO: Maybe create buffer here too?
                  ;; This way it won't fail when called from a URL.
                  (let ((,buffer-var (or (find (quri:uri
                                                (nyxt-url
                                                 (quote ,name)
                                                 ,@args))
                                               (buffer-list) :key #'url
                                               :test #'quri:uri=)
                                         (current-buffer))))
                    ;; We need to ignore those to avoid warnings, as the same arglist
                    ;; is used in both internal function and a command.
                    (declare (ignorable ,buffer-var))
                    ,@body)))
           (setf (gethash (quote ,name) *nyxt-url-commands*)
                 (function ,internal-name)))
         (define-command-global ,name (,@arglist)
           ,@(when documentation (list documentation))
           (let* ((,buffer-var (or (find-if (lambda (b)
                                              (and (string= (title b) ,title)
                                                   (find-mode b ,mode)))
                                            (buffer-list))
                                   (funcall (symbol-function ,mode)
                                            :activate t
                                            :buffer (make-internal-buffer
                                                     :title ,title
                                                     :url (quri:uri
                                                           (nyxt-url
                                                            (quote ,name)
                                                            ,@args)))))))
             (set-current-buffer ,buffer-var)
             ,buffer-var))))))

(defmacro with-current-panel ((buffer-var title &key (side :left))
                              &body body)
  "Display a panel buffer displaying CONTENT.
If a panel with TITLE exists, reuse it, otherwise create a new panel.
BUFFER-VAR is bound to the new buffer in BODY.
BODY must return the HTML markup as a string."
  (alex:once-only (title)
    `(let* ((window (current-window))
            (,buffer-var (or (find ,title
                                   (panel-buffers window)
                                   :key #'title
                                   :test #'string=)
                             (make-instance 'user-panel-buffer
                                            :title ,title))))
       (window-add-panel-buffer window ,buffer-var ,side)
       (html-set
        (progn
          ,@body)
        ,buffer-var)
       ,buffer-var)))

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
