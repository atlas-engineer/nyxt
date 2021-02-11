;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class keepassxc-interface (password-interface)
  ((executable :initform (executable-find "keepassxc-cli"))
   (password-file :accessor password-file
                  :initarg :file)
   (master-password :accessor master-password
                    :initarg :master-password
                    :initform nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'make-keepassxc-interface))
(defun make-keepassxc-interface ()
  (make-instance 'keepassxc-interface))
(push #'make-keepassxc-interface interface-list)

(defmethod list-passwords ((password-interface keepassxc-interface))
  (let* ((st (make-string-input-stream (master-password password-interface)))
         (output (uiop:run-program (list (executable password-interface)
                                         "ls" (password-file password-interface))
                                   :input st :output '(:string :stripped t))))
    (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal)))

(defmethod clip-password ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (with-input-from-string (st (master-password password-interface))
    (uiop:run-program (list (executable password-interface)
                            "clip"
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
    (uiop:run-program (list (executable password-interface)
                            "add" "--password-prompt" (password-file password-interface)
                            (if (str:emptyp password-name)
                                "--generate"
                                password-name))
                      :input st)))

(defmethod password-correct-p ((password-interface keepassxc-interface))
  (when (master-password password-interface)
    (handler-case
        (let* ((st (make-string-input-stream (master-password password-interface)))
               (output (uiop:run-program (list (executable password-interface)
                                               "ls" (password-file password-interface))
                                         :input st :output '(:string :stripped t))))
          (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal))
      (uiop/run-program:subprocess-error ()
        nil))))
