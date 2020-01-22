(uiop:define-package :next/vi-mode
  (:use :common-lisp :trivia :next)
  (:documentation "VI-style bindings."))
(in-package :next/vi-mode)

(define-mode vi-normal-mode ()
  "Enable VI-style modal bindings (normal mode)"
  ((keymap-schemes
    :initform
    (let ((map (make-keymap)))
      (define-key :keymap map
        "i" #'vi-insert-mode
        "button1" #'vi-button1)
      (list :vi-normal map)))
   (destructor
    :initform
    (lambda (mode)
      (setf (current-keymap-scheme (buffer mode))
            (get-default 'buffer 'current-keymap-scheme))
      (setf (forward-input-events-p (buffer mode))
            (get-default 'buffer 'current-keymap-scheme))))
   (constructor
    :initform
    (lambda (mode)
      (let ((active-buffer (buffer mode)))
        (vi-insert-mode :activate nil :buffer active-buffer)
        (setf (current-keymap-scheme active-buffer) :vi-normal)
        (setf (forward-input-events-p active-buffer) nil))))))

;; TODO: Move ESCAPE binding to the override map?
(define-mode vi-insert-mode ()
  "Enable VI-style modal bindings (insert mode)"
  ((keymap-schemes
    :initform
    (let ((map (make-keymap)))
      (define-key :keymap map
        "ESCAPE" #'vi-normal-mode
        "button1" #'vi-button1)
      (list :vi-insert map)))
   (destructor
    :initform
    (lambda (mode)
      (setf (current-keymap-scheme (buffer mode))
            (get-default 'buffer 'current-keymap-scheme))))
   (constructor
    :initform
    (lambda (mode)
      (let ((active-buffer (buffer mode)))
        (vi-normal-mode :activate nil :buffer active-buffer)
        (setf (current-keymap-scheme active-buffer) :vi-insert))))))

(define-parenscript %clicked-in-input? ()
  (ps:chain document active-element tag-name))

(declaim (ftype (function (string)) input-tag-p))
(defun input-tag-p (tag)
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(define-command vi-button1 (&optional (buffer (current-buffer)))
  "Enable VI insert mode when focus is on an input element on the web page."
  (let ((root-mode (find-mode buffer 'root-mode)))
    (rpc-generate-input-event
     (ipc-window-active *interface*)
     (first (last-key-chords
             (buffer root-mode))))
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
