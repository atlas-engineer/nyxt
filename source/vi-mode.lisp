(uiop:define-package :next/vi-mode
  (:use :common-lisp :trivia :next)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "VI-style bindings."))
(in-package :next/vi-mode)

(define-mode vi-normal-mode ()
  "Enable VI-style modal bindings (normal mode)"
  ((keymap-schemes
    :initform
    (define-scheme "vi"
      scheme:vi-normal
      (list
        "i" #'vi-insert-mode
        "button1" #'vi-button1)))
   (destructor
    :initform
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (get-default 'buffer 'keymap-scheme-name))
      (setf (forward-input-events-p (buffer mode))
            (get-default 'buffer 'keymap-scheme-name))))
   (constructor
    :initform
    (lambda (mode)
      (let ((active-buffer (buffer mode)))
        (vi-insert-mode :activate nil :buffer active-buffer)
        (setf (keymap-scheme-name active-buffer) scheme:vi-normal)
        (setf (forward-input-events-p active-buffer) nil))))))

;; TODO: Move ESCAPE binding to the override map?
(define-mode vi-insert-mode ()
  "Enable VI-style modal bindings (insert mode)"
  ((keymap-schemes
    :initform
    (define-scheme "vi"
      scheme:vi-insert
      (list
       ;; TODO: Forwarding C-v crashes cl-webkit.  See
       ;; https://github.com/atlas-engineer/next/issues/593#issuecomment-599051350
       "C-v" #'next/web-mode:paste
       "escape" #'vi-normal-mode
       "button1" #'vi-button1)))
   (destructor
    :initform
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (get-default 'buffer 'keymap-scheme-name))))
   (constructor
    :initform
    (lambda (mode)
      (let ((active-buffer (buffer mode)))
        (vi-normal-mode :activate nil :buffer active-buffer)
        (setf (keymap-scheme-name active-buffer) scheme:vi-insert))))))

(define-parenscript %clicked-in-input? ()
  (ps:chain document active-element tag-name))

(declaim (ftype (function (string)) input-tag-p))
(defun input-tag-p (tag)
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(define-command vi-button1 (&optional (buffer (current-buffer)))
  "Enable VI insert mode when focus is on an input element on the web page."
  (let ((root-mode (find-mode buffer 'root-mode)))
    ;; First we generate a button1 event so that the web view element is clicked
    ;; (e.g. a text field gets focus).
    (ffi-generate-input-event
     (ffi-window-active *browser*)
     (last-event (buffer root-mode)))
    (%clicked-in-input?
     :callback (lambda (response)
                 (cond
                   ((and (input-tag-p response)
                         (find-submode (buffer root-mode) 'vi-normal-mode))
                    (vi-insert-mode))
                   ((and (not (input-tag-p response))
                         (find-submode (buffer root-mode) 'vi-insert-mode))
                    (vi-normal-mode)))))))

(defmethod did-finish-navigation ((mode vi-insert-mode) url)
  (declare (ignore url))
  (vi-normal-mode))
