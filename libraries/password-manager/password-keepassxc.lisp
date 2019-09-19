(in-package :password)

(annot:enable-annot-syntax)

(defmethod list-passwords ((password-interface keepassxc-interface))
  (let* ((st (make-string-input-stream (master-password password-interface)))
         (output (uiop:run-program `("keepassxc-cli"
                                     "ls" ,(password-file password-interface))
                                   :input st :output '(:string :stripped t))))
    (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal)))

(defmethod clip-password ((password-interface keepassxc-interface) password-name)
  (let* ((st (make-string-input-stream (master-password password-interface)))
         (output (uiop:run-program `("keepassxc-cli"
                                     "show" ,(password-file password-interface)
                                     ,password-name)
                                   :input st :output '(:string :stripped t))))
    (clip-password-string
     (cl-ppcre:regex-replace "[\\S\\s]*Password: \(.*\)[\\S\\s]*" output "\\1"))))

(defmethod save-password ((password-interface keepassxc-interface) password-name password)
  (with-input-from-string (st (make-string-input-stream
                               (format nil "~a~C~a"
                                       (master-password password-interface)
                                       #\newline password)))
    (uiop:run-program `("keepassxc-cli"
                        "add" "-p" ,(password-file password-interface) ,password-name)
                      :input st)))

(defmethod password-correct-p ((password-interface keepassxc-interface))
  (when (master-password password-interface)
    (handler-case
        (let* ((st (make-string-input-stream (master-password password-interface)))
               (output (uiop:run-program `("keepassxc-cli"
                                           "ls" ,(password-file password-interface))
                                         :input st :output '(:string :stripped t))))
          (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal))
      (uiop/run-program:subprocess-error ()
        nil))))
