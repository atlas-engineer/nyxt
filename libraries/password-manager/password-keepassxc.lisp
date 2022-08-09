;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class keepassxc-interface (password-interface)
  ((executable (pathname->string (sera:resolve-executable "keepassxc-cli")))
   (password-file)
   (master-password nil
                    :type (or null string))
   (entries-cache nil
                  :type list
                  :documentation "The cache to speed the entry listing up."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(push 'keepassxc-interface *interfaces*)

(defmethod list-passwords ((password-interface keepassxc-interface))
  (or (entries-cache password-interface)
      (let* ((st (make-string-input-stream (master-password password-interface)))
             (output (execute password-interface
                              (list "ls" "-Rf" ; Recursive flattened.
                                    (password-file password-interface))
                       :input st :output '(:string :stripped t))))
        (setf (entries-cache password-interface)
              (remove-if (alexandria:curry #'str:ends-with-p "/") (sera:lines output))))))

(defmethod clip-password ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (with-input-from-string (st (master-password password-interface))
    (execute password-interface (list "clip"
                                      (password-file password-interface)
                                      password-name)
      :input st
      :wait-p nil)))

(defmethod clip-username ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (with-input-from-string (st (master-password password-interface))
    (execute password-interface (list "clip"
                                      "--attribute" "username"
                                      (password-file password-interface)
                                      password-name)
             :input st
             :wait-p nil)))

(defmethod save-password ((password-interface keepassxc-interface)
                          &key password-name username password service)
  (declare (ignore service))
  ;; This is to force entries re-fetching the next time we need passwords.
  (setf (entries-cache password-interface) nil)
  (with-input-from-string (st (format nil "~a~C~a"
                                      (master-password password-interface)
                                      #\newline password))
    (execute password-interface (list "add"
                                      "--username" username
                                      "--password-prompt" (password-file password-interface)
                                      (if (str:emptyp password-name)
                                          "--generate"
                                          password-name))
             :input st)))

(defmethod password-correct-p ((password-interface keepassxc-interface))
  (when (master-password password-interface)
    (handler-case
        (list-passwords password-interface)
      (uiop/run-program:subprocess-error ()
        nil))))
