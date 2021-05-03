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
  `(progn
     (defun ,script-name ,args
       (ffi-buffer-evaluate-javascript (current-buffer)
                                       (ps:ps ,@script-body)))))

(export-always 'pflet)
(defmacro pflet (functions &body body)
  (flet ((transform-definition (name args body)
           `(,name ,args
                   (ffi-buffer-evaluate-javascript (current-buffer) (ps:ps . ,body)))))
    `(flet ,(loop for (name lambda-list . body) in functions
                  collect (transform-definition name lambda-list body))
       ,@body)))

(export-always 'document-get-paragraph-contents)
(define-parenscript document-get-paragraph-contents (&key (limit 100000))
  (let ((result ""))
    (loop for element in (qsa document (list "p"))
          do (setf result (+ result
                             (ps:chain element text-content))))
    (ps:chain result (slice 0 (ps:lisp limit)))))

(export-always '%paste)
(define-parenscript %paste (&key (input-text (ring-insert-clipboard (clipboard-ring *browser*))))
  (let ((active-element (ps:chain document active-element))
        (tag (ps:chain document active-element tag-name)))
    (when (or (string= tag "INPUT")
              (string= tag "TEXTAREA"))
      (nyxt/ps:insert-at active-element (ps:lisp input-text)))))

(export-always '%copy)
(define-parenscript %copy ()
  "Return selected text from javascript."
  (ps:chain window (get-selection) (to-string)))

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
  (let ((style (markup:markup (:style style-string))))
    (ffi-buffer-evaluate-javascript-async
     buffer
     (ps:ps (ps:chain document body
                      (|insertAdjacentHTML| "afterbegin"
                                            (ps:lisp style)))))))

(export-always 'with-current-html-buffer)
(defmacro with-current-html-buffer ((buffer-var title mode
                                     &key no-history-p)
                                    &body body)
  "Switch to a buffer in MODE displaying CONTENT.
If a buffer in MODE with TITLE exists, reuse it, otherwise create a new buffer.
BUFFER-VAR is bound to the new buffer in BODY.
MODE is a mode symbol.
BODY must return the HTML markup as a string."
  `(let* ((,buffer-var (or (find-if (lambda (b)
                                      (and (string= (title b) ,title)
                                           (find-mode b ,mode)))
                                    (buffer-list))
                           (funcall (symbol-function ,mode)
                                    :activate t
                                    :buffer (make-internal-buffer
                                             :title ,title
                                             :no-history-p ,no-history-p)))))
     (html-set
      (progn
        ,@body)
      ,buffer-var)
     (set-current-buffer ,buffer-var)
     ,buffer-var))

(in-package :nyxt/parenscript)

(defpsmacro qs (context selector)
  "Alias of document.querySelector"
  `(chain ,context (query-selector ,selector)))

(defpsmacro qsa (context selector)
  "Alias of document.querySelectorAll"
  `(chain ,context (query-selector-all ,selector)))

(defpsmacro insert-at (tag input-text)
  "Insert text at a tag."
  `(let ((start (chain tag selection-start))
         (end (chain ,tag selection-end)))
     (setf (chain ,tag value)
           (+ (chain ,tag value (substring 0 start))
              ,input-text
              (chain ,tag value
                     (substring end
                                (chain ,tag value length)))))
     (if (= start end)
         (progn
           (setf (chain ,tag selection-start) (+ start (ps:chain ,input-text length)))
           (setf (chain ,tag selection-end) (ps:chain ,tag selection-start)))
         (progn
           (setf (chain ,tag selection-start) start)
           (setf (chain ,tag selection-end) (+ start (ps:chain ,input-text length)))))))

(defpsmacro element-drawable-p (element)
  "Is the element drawable?"
  `(if (or (ps:chain ,element offset-width)
           (ps:chain ,element offset-height)
           (ps:chain ,element (get-client-rects) length))
       t nil))

(defpsmacro element-in-view-port-p (element)
  "Is the element in the view port?"
  `(ps:let* ((rect (ps:chain ,element (get-bounding-client-rect))))
     (if (and (>= (ps:chain rect top) 0)
              (>= (ps:chain rect left) 0)
              (<= (ps:chain rect right) (ps:chain window inner-width))
              (<= (ps:chain rect bottom) (ps:chain window inner-height)))
         t nil)))
