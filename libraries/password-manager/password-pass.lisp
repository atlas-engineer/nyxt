(in-package :password)

(annot:enable-annot-syntax)

(defmethod list-passwords ((password-interface password-store-interface))
  (let ((raw-list (directory (format nil "~a/**/*.gpg"
                                     (password-directory password-interface))))
        (dir-length (length (namestring
                             (truename (password-directory password-interface))))))
    (mapcar #'(lambda (x)
                (subseq (namestring x) dir-length (- (length (namestring x)) 4)))
            raw-list)))

(defmethod clip-password ((password-interface password-store-interface) password-name)
  (clip-password-string (uiop:run-program `("pass" "show" ,password-name)
                                          :output '(:string :stripped t))))

(defmethod save-password ((password-interface password-store-interface) password-name password)
  (with-open-stream (st (make-string-input-stream password))
    (uiop:run-program `("pass" "insert" "--echo" ,password-name) :input st)))

(defmethod password-correct-p ((password-interface password-store-interface))
  t)
