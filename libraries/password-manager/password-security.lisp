;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

;;; Provide an interface to the command line "security" program used
;;; on BSD and Darwin systems to interface with the system keychain

(define-class security-interface (password-interface)
  ((executable (pathname->string (sera:resolve-executable "security"))))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(push 'security-interface *interfaces*)

(defmethod list-passwords ((password-interface security-interface))
  (error "Listing passwords not supported by the 'security' interface."))

(defmethod clip-password ((password-interface security-interface) &key password-name service)
  (clip-password-string password-interface
   (str:replace-all
    "\"" ""
    (str:replace-first
     "password: " ""
     (nth-value 1
                (execute password-interface
                  (list "find-internet-password"
                        "-a" password-name "-s" service "-g")
                  :error-output '(:string :stripped t)))))))

(defmethod clip-username ((password-interface security-interface) &key password-name service)
  (declare (ignore password-name service))
  (error "Username clipping is not supported by security interface."))

(defmethod password-correct-p ((password-interface security-interface)) t)
