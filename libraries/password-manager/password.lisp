(in-package :password)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '*sleep-timer*))
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

(defun safe-clipboard-text ()
  "Return clipboard content, or \"\" if the content is not textual."
  ;; xclip errors out when the clipboard contains non-text:
  ;; https://github.com/astrand/xclip/issues/38#issuecomment-466625564.
  (ignore-errors (trivial-clipboard:text)))

;;; Prerequisite Functions
(defun clip-password-string (pass)
  (trivial-clipboard:text pass)
  (bt:make-thread
   (lambda ()
     (sleep *sleep-timer*)
     (when (string= (safe-clipboard-text) pass)
       ;; Reset the clipboard so that the user does not accidentally paste
       ;; something else.
       (trivial-clipboard:text "")))))


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
  (unless *password-store-program*
    (setf *password-store-program* (executable-find "pass")))
  (unless *keepassxc-cli-program*
    (setf *keepassxc-cli-program* (executable-find "keepassxc-cli")))
  (unless *security-cli-program*
    (setf *security-cli-program* (executable-find "security")))
  (cond (*password-store-program*
         (make-instance 'password-store-interface))
        (*keepassxc-cli-program*
         (make-instance 'keepassxc-interface))
        (*security-cli-program*
         (make-instance 'security-interface))
        (t nil)))
