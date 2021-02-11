;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class keepassxc-interface (password-interface)
  ((executable (executable-find "keepassxc-cli"))
   (password-file)
   (master-password :initform nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(push 'keepassxc-interface *interfaces*)

(defmethod list-passwords ((password-interface keepassxc-interface))
  (let* ((st (make-string-input-stream (master-password password-interface)))
         (output (execute password-interface (list "ls" (password-file password-interface))
                          :input st :output '(:string :stripped t))))
    (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal)))

(defmethod clip-password ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (with-input-from-string (st (master-password password-interface))
    (execute password-interface (list "clip"
                                      (password-file password-interface)
                                      password-name
                                      ;; Timeout for password
                                      (format nil "~a" (sleep-timer password-interface)))
             :input st :output '(:string :stripped t))))

(defmethod save-password ((password-interface keepassxc-interface) &key password-name password service)
  (declare (ignore service))
  (with-input-from-string (st (format nil "~a~C~a"
                                      (master-password password-interface)
                                      #\newline password))
    (execute password-interface (list "add" "--password-prompt" (password-file password-interface)
                                      (if (str:emptyp password-name)
                                          "--generate"
                                          password-name))
             :input st)))

(defmethod password-correct-p ((password-interface keepassxc-interface))
  (when (master-password password-interface)
    (handler-case
        (let* ((st (make-string-input-stream (master-password password-interface)))
               (output (execute password-interface (list "ls" (password-file password-interface))
                                :input st :output '(:string :stripped t))))
          (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal))
      (uiop/run-program:subprocess-error ()
        nil))))
