(in-package :password)

(annot:enable-annot-syntax)

(defvar *sleep-timer* 15)

(defclass password-interface ()
  ())

(defclass password-store-interface (password-interface)
  ((password-directory :reader password-directory
                       :initarg :directory
                       :initform (or (uiop:getenv "PASSWORD_STORE_DIR")
                                     (namestring (format nil "~a/.password-store"
                                                         (uiop:getenv "HOME")))))))

(defclass keepassxc-interface (password-interface)
  ((password-file :accessor password-file
                  :initarg :file)
   (master-password :accessor master-password
                    :initarg :master-password
                    :initform nil)))

@export
(defgeneric list-passwords (password-interface)
  (:documentation "Retrieve all available passwords."))

@export
(defgeneric clip-password (password-interface password-name)
  (:documentation "Retrieve specific password by name."))

@export
(defgeneric save-password (password-interface password-name password)
  (:documentation "Save password to database.
If PASSWORD-NAME is empty, then generate a new password."))

@export
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

@export
(defun make ()
  (cond ((executable-find "pass")
         (make-instance 'password-store-interface))
        ((executable-find "keepassxc-cli")
         (make-instance 'keepassxc-interface))
        (t nil)))
