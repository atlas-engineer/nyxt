;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

;;; Provide an interface to the command line "security" program used
;;; on BSD and Darwin systems to interface with the system keychain

(defclass security-interface (password-interface) ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'make-security-interface))
(defun make-security-interface ()
  (make-instance 'security-interface))

(push #'make-security-interface interface-list)

(defmethod initialize-instance :after ((password-interface security-interface) &key)
  (unless (slot-boundp password-interface 'executable)
    (setf (executable password-interface) (executable-find "security"))))

(defmethod list-passwords ((password-interface security-interface))
  (error "Listing passwords not supported by security interface."))

(defmethod clip-password ((password-interface security-interface) &key password-name service)
  (clip-password-string
   (str:replace-all
    "\"" ""
    (str:replace-first
     "password: " ""
     (nth-value 1
                (uiop:run-program
                 (list (executable password-interface) "find-internet-password"
                       "-a" password-name "-s" service "-g")
                 :error-output '(:string :stripped t)))))))

(defmethod password-correct-p ((password-interface security-interface)) t)
