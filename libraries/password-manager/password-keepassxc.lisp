(in-package :password)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '*keepassxc-cli-program*))
(defvar *keepassxc-cli-program* (executable-find "keepassxc-cli"))

(defclass keepassxc-interface (password-interface)
  ((password-file :accessor password-file
                  :initarg :file)
   (master-password :accessor master-password
                    :initarg :master-password
                    :initform nil)))

(defmethod list-passwords ((password-interface keepassxc-interface))
  (let* ((st (make-string-input-stream (master-password password-interface)))
         (output (uiop:run-program (list *keepassxc-cli-program*
                                         "ls" (password-file password-interface))
                                   :input st :output '(:string :stripped t))))
    (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal)))

(defmethod clip-password ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (let* ((st (make-string-input-stream (master-password password-interface)))
         (output (uiop:run-program (list *keepassxc-cli-program*
                                         "show" (password-file password-interface)
                                         password-name)
                                   :input st :output '(:string :stripped t))))
    (clip-password-string
     (cl-ppcre:regex-replace "[\\S\\s]*Password: \(.*\)[\\S\\s]*" output "\\1"))))

(defmethod save-password ((password-interface keepassxc-interface) &key password-name password service)
  (declare (ignore service))
  (with-input-from-string (st (format nil "~a~C~a"
                                      (master-password password-interface)
                                      #\newline password))
    (uiop:run-program (list *keepassxc-cli-program*
                            "add" "--password-prompt" (password-file password-interface)
                            (if (str:emptyp password-name)
                                "--generate"
                                password-name))
                      :input st)))

(defmethod password-correct-p ((password-interface keepassxc-interface))
  (when (master-password password-interface)
    (handler-case
        (let* ((st (make-string-input-stream (master-password password-interface)))
               (output (uiop:run-program (list *keepassxc-cli-program*
                                               "ls" (password-file password-interface))
                                         :input st :output '(:string :stripped t))))
          (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal))
      (uiop/run-program:subprocess-error ()
        nil))))
