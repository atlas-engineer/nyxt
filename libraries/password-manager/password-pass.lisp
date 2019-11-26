(in-package :password)

(defvar *password-store-program* (executable-find "pass"))

(defmethod list-passwords ((password-interface password-store-interface))
  (let ((raw-list (directory (format nil "~a/**/*.gpg"
                                     (password-directory password-interface))))
        (dir-length (length (namestring
                             (truename (password-directory password-interface))))))
    (mapcar #'(lambda (x)
                (subseq (namestring x) dir-length (- (length (namestring x)) 4)))
            raw-list)))

(defmethod clip-password ((password-interface password-store-interface) password-name)
  (clip-password-string
   ;; The common way to store secret in password-store is to use the first line
   ;; for the secret; there's no standard for how to encode anything else.
   ;; Because there's no support in `password` for having additional fields
   ;; (e.g. username or email address for autofilling input fields), we'll keep
   ;; it simple and just return the first line of the password file.
   (first
    (cl-ppcre:split #\newline
                    (uiop:run-program (list *password-store-program* "show"
                                            password-name)
                                      :output '(:string :stripped t))))))

(defmethod save-password ((password-interface password-store-interface)
                          password-name password)
  (if (str:emptyp password)
      (uiop:run-program (list *password-store-program* "generate" password-name))
      (with-open-stream (st (make-string-input-stream password))
        (uiop:run-program (list *password-store-program* "insert" "--echo" password-name)
                          :input st))))

(defmethod password-correct-p ((password-interface password-store-interface))
  t)
