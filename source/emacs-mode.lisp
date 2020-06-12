(uiop:define-package :nyxt/emacs-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Emacs-style bindings."))
(in-package :nyxt/emacs-mode)

(define-mode emacs-mode ()
  "Enable Emacs-style bindings.
To enable these bindings by default, add the mode to the list of default modes
in your configuration file.

Example:

\(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-default))))"
  ((previous-keymap-scheme-name
    :accessor previous-keymap-scheme-name
    :type keymap:scheme-name
    :documentation "The previous keymap scheme that will be used when ending
this mode.")
   (destructor
    :initform
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (previous-keymap-scheme-name mode))))
   (constructor
    :initform
    (lambda (mode)
      (with-accessors ((buffer buffer)) mode
        (setf (previous-keymap-scheme-name mode) (keymap-scheme-name buffer))
        (setf (keymap-scheme-name buffer) scheme:emacs))))))
