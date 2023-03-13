;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; TODO: Quite some code could be factored with `internal-page'.

(define-class panel-buffer-source (prompter:source)
  ((prompter:name "Panel buffers")
   (window :accessor window :initarg :window)
   (prompter:enable-marks-p t)
   (prompter:constructor (lambda (source)
                           (panel-buffers (window source))))))

(define-command-global delete-panel-buffer (&key (window (current-window))
                                            (panels (prompt
                                                     :prompt "Delete a panel buffer"
                                                     :sources (make-instance 'panel-buffer-source
                                                                             :window window))))
  "Prompt for panel buffer(s) to be deleted.
When provided, PANELS are deleted instead."
  (mapc (curry #'window-delete-panel-buffer window) (uiop:ensure-list panels)))

(define-command-global delete-all-panel-buffers (&key (window (current-window)))
  "Delete all the open panel buffers in WINDOW."
  (delete-panel-buffer :panels (panel-buffers window)))

(define-class panel-page (internal-page)
  ((side
    :left
    :type (member :left :right)
    :documentation "The side of the window where the panel is displayed."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Internal page for `panel-buffers'.
The main difference is that their command toggles the panel."))

(defun find-panel-buffer (name)
  "Return first panel buffer which URL is a NAME `panel-page'."
  (find name (panel-buffers (current-window))
        :key (alex:compose #'internal-page-name #'url)))

(defmethod set-internal-page-method ((page panel-page) form)
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
          (alex:if-let ((panel-buffer (find-panel-buffer (name ,page))))
            (window-delete-panel-buffer (current-window) panel-buffer)
            (window-add-panel-buffer
             (current-window)
             (buffer-load (nyxt-url (name ,page) ,@(mappend #'first keywords))
                          :buffer (make-instance 'panel-buffer))
             (side ,page))))))))

;; FIXME: Better way to compose HTML wrappers?
(defmethod (setf form) :after (lambda-expression (page panel-page))
  (declare (ignore lambda-expression))
  (let ((original-form (slot-value page 'form)))
    (setf (slot-value page 'form)
          (lambda (&rest args)
            (destructuring-bind (contents &optional (type "text/html;charset=utf8") (status 200)
                                            headers reason)
                (multiple-value-list (funcall original-form) args)
              (when (str:starts-with-p "text/html" type)
                (setf contents
                      (spinneret:with-html-string
                        (:raw contents)
                        (let ((buffer (find-panel-buffer (name page))))
                          (:button.button
                           :style "position: absolute; top: 1em; right: 1em;"
                           :title "Close this panel buffer"
                           :onclick (ps:ps (nyxt/ps:lisp-eval
                                            (:title "panel close button"
                                             :buffer buffer)
                                            (window-delete-panel-buffer
                                             (current-window) buffer)))
                           "×")))))
              (values contents type status headers reason))))))

;; TODO: Add define-panel?

(export-always 'define-panel-command)
(defmacro define-panel-command (name (&rest arglist)
                                (buffer-var title &optional (side :left))
                                &body body)
  "Define a panel buffer and:
- A local command called NAME, creating this panel-buffer or closing it if it's shown already.
- A nyxt:NAME URL for the content of this panel buffer.

Should end with a form returning HTML as a string.

BUFFER-VAR is the variable the created panel will be bound to in the BODY. SIDE
is either :LEFT (default) or :RIGHT.

ARGLIST is arguments for the command and for the underlying page-generating
function. Any argument from it is safe to use in the body of this macro.
Beware: the ARGLIST should have nothing but keyword arguments because it's
mapped to query parameters."
  (multiple-value-bind (stripped-body declarations documentation)
      (alex:parse-body body :documentation t)
    `(progn
       (export-always ',name (symbol-package ',name))
       (sera:lret ((gf (defgeneric ,name (,@(generalize-lambda-list arglist))
                         ,@(when documentation
                             `((:documentation ,documentation)))
                         (:generic-function-class panel-page))))
         (let ((wrapped-body '(lambda (,@arglist)
                               ,@(when documentation (list documentation))
                               ,@declarations
                               (let ((,buffer-var (find-panel-buffer ',name)))
                                 (declare (ignorable ,buffer-var))
                                 ,@stripped-body))))
           (set-internal-page-method gf wrapped-body)
           (setf (slot-value #',name 'visibility) :mode)
           (setf (slot-value #',name 'dynamic-title)
                 ,(if (stringp title)
                      title
                      (let ((keywords (nth-value 3 (alex:parse-ordinary-lambda-list arglist))))
                        `(lambda (,@arglist)
                           (declare (ignorable ,@(mappend #'cdar keywords)))
                           ,title))))
           (setf (slot-value #',name 'side) ,side)
           (setf (form gf) wrapped-body))))))

(export-always 'define-panel-command-global)
(defmacro define-panel-command-global (name (&rest arglist)
                                       (buffer-var title &optional (side :left))
                                       &body body)
  "Define a panel buffer with a global command showing it.

See `define-panel-command' for the description of the arguments."
  `(prog1 (define-panel-command ,name (,@arglist) (,buffer-var ,title ,side) ,@body)
     (setf (slot-value #',name 'visibility) :global)))
