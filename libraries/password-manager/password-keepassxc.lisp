;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class keepassxc-interface (password-interface)
  ((executable (pathname->string (sera:resolve-executable "keepassxc-cli")))
   (password-file
    :documentation "The path to the KeePass password database.")
   (key-file
    nil
    :type (or null string pathname)
    :documentation "The key file for `password-file'.")
   (master-password
    ""
    :type string
    :documentation "The password to the `password-file'.")
   (yubikey-slot
    nil
    :documentation "Yubikey slot to unlock the `password-file'.")
   (entries-cache
    nil
    :type list
    :export nil
    :documentation "The cache to speed the entry listing up."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(push 'keepassxc-interface *interfaces*)

(defmethod list-passwords ((password-interface keepassxc-interface))
  (or (entries-cache password-interface)
      (let* ((st (make-string-input-stream (master-password password-interface)))
             (output (execute password-interface
                              (append (list "ls" "-Rf") ; Recursive flattened.
                                      (when (key-file password-interface)
                                        (list "-k" (uiop:native-namestring (key-file password-interface))))
                                      (when (yubikey-slot password-interface)
                                        (list "-y" (yubikey-slot password-interface)))
                                      (list (password-file password-interface)))
                              :input st :output '(:string :stripped t))))
        (setf (entries-cache password-interface)
              (remove-if (alexandria:curry #'str:ends-with-p "/") (sera:lines output))))))

(defmethod clip-password ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (with-input-from-string (st (master-password password-interface))
    (execute password-interface
             (append
              (list "clip")
              (when (key-file password-interface)
                (list "-k" (uiop:native-namestring (key-file password-interface))))
              (when (yubikey-slot password-interface)
                (list "-y" (yubikey-slot password-interface)))
              (list (password-file password-interface) password-name))
             :input st
             :wait-p nil)))

(defmethod clip-username ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (with-input-from-string (st (master-password password-interface))
    (execute password-interface
             (append (list "clip" "--attribute" "username")
                     (when (key-file password-interface)
                       (list "-k" (uiop:native-namestring (key-file password-interface))))
                     (when (yubikey-slot password-interface)
                       (list "-y" (yubikey-slot password-interface)))
                     (list (password-file password-interface) password-name))
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
    (execute password-interface
             (append (list "add" "--username" username
                           "--password-prompt" (password-file password-interface))
                     (when (key-file password-interface)
                       (list "-k" (uiop:native-namestring (key-file password-interface))))
                     (when (yubikey-slot password-interface)
                       (list "-y" (yubikey-slot password-interface)))
                     (list (if (str:emptyp password-name)
                               "--generate"
                               password-name)))
             :input st)))

(defmethod password-correct-p ((password-interface keepassxc-interface))
  (handler-case
      (list-passwords password-interface)
    (uiop/run-program:subprocess-error ()
      nil)))
