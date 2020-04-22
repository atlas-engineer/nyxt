(in-package :password)

(defvar *sleep-timer* 15)

(defclass password-interface ()
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'list-passwords))
(defgeneric list-passwords (password-interface)
  (:documentation "Retrieve all available passwords."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'clip-password))
(defgeneric clip-password (password-interface &key password-name service)
  (:documentation "Retrieve specific password by name."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'save-password))
(defgeneric save-password (password-interface &key password-name password service)
  (:documentation "Save password to database.
If PASSWORD-NAME is empty, then generate a new password."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'password-correct-p))
(defgeneric password-correct-p (password-interface)
  (:documentation "Return T if set password is correct, NIL otherwise."))

;;; Prerequisite Functions
(defun clip-password-string (pass)
  (let ((original-clipboard (trivial-clipboard:text)))
    (trivial-clipboard:text pass)
    (bt:make-thread
     (lambda ()
       (sleep *sleep-timer*)
       (when (string= (trivial-clipboard:text) pass)
         (trivial-clipboard:text original-clipboard))))))


;;; Commands to wrap together.

(defun executable-find (command)
  "Search for COMMAND in the PATH and return the absolute file name.
Return nil if COMMAND is not found anywhere."
  (multiple-value-bind (path)
      (ignore-errors
       (uiop:run-program (format nil "command -v ~A" command)
                         :output '(:string :stripped t)))
    path))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'make))
(defun make ()
  (cond ((executable-find "pass")
         (make-instance 'password-store-interface))
        ((executable-find "keepassxc-cli")
         (make-instance 'keepassxc-interface))
        ((executable-find "security")
         (make-instance 'security-interface))
        (t nil)))
