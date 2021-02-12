;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun make-password-interface-user-classes ()
  "Define user classes so that users may apply define-configuration
macro to change slot values."
  (loop for interface in password:*interfaces* do
           (eval `(define-user-class ,(intern (symbol-name interface)
                                              (package-name (symbol-package interface)))))))

(make-password-interface-user-classes)

(defun make-password-interface ()
  "Make a password interface for all user classes, the first one with
a non-nil value of executable is considered to be the interface to
make/return."
  (find-if #'password:executable
           (mapcar (alex:compose #'make-instance #'user-class-name)
                   password:*interfaces*)))

(defun password-suggestion-filter (password-instance)
  (let ((password-list (password:list-passwords password-instance))
        (domain (quri:uri-domain (url (current-buffer)))))
    (when domain
      (let ((closest-match (find-if (alex:curry #'str:containsp domain) password-list)))
        (setf password-list (cons closest-match (delete closest-match password-list)))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) password-list))))

(defun password-debug-info ()
  (when (password-interface *browser*)
    (log:debug "Password interface ~a uses executable ~s."
               (class-name (class-of (password-interface *browser*)))
               (password:executable (password-interface *browser*)))))

(defun has-method-p (object generic-function)
  "Return non-nil if OBJECT is a specializer of a method of GENERIC-FUNCTION."
  (find-if (alex:curry #'typep object)
           (alex:mappend #'closer-mop:method-specializers
                         (closer-mop:generic-function-methods generic-function))))

(define-command save-new-password ()
  "Save password to password interface."
  (password-debug-info)
  (cond
    ((and (password-interface *browser*)
          (has-method-p (password-interface *browser*)
                        #'password:save-password))
     (let* ((password-name (prompt-minibuffer
                            :input-prompt "Name for new password"
                            :input-buffer (or (quri:uri-domain (url (current-buffer)))
                                              "")))
            (new-password (prompt-minibuffer
                           :invisible-input-p t
                           :input-prompt "New password (leave empty to generate)")))
       (password:save-password (password-interface *browser*)
                               :password-name password-name
                               :password new-password)))
    ((null (password-interface *browser*))
     (echo-warning "No password manager found."))
    (t (echo-warning "Password manager ~s does not support saving passwords."
                     (string-downcase
                      (class-name (class-of (password-interface *browser*))))))))

(defmacro with-password (password-interface &body body)
  `(if (password:password-correct-p ,password-interface)
       ,@body))

(define-command copy-password-prompt-details ()
  "Copy password prompting for all the details without suggestion."
  (password-debug-info)
  (if (password-interface *browser*)
      (let* ((password-name (prompt-minibuffer
                             :input-prompt "Name of password"))
             (service (prompt-minibuffer
                       :input-prompt "Service")))
        (handler-case
            (password:clip-password (password-interface *browser*)
                                    :password-name password-name
                                    :service service)
          (error (c)
            (echo-warning "Error retrieving password: ~a" c))))
      (echo-warning "No password manager found.")))

(define-command copy-password ()
  "Copy chosen password from minibuffer."
  (password-debug-info)
  (if (password-interface *browser*)
      (with-password (password-interface *browser*)
        (let ((password-name (prompt-minibuffer
                              :suggestion-function
                              (password-suggestion-filter
                               (password-interface *browser*)))))
          (password:clip-password (password-interface *browser*) :password-name password-name)
          (echo "Password saved to clipboard for ~a seconds." (password:sleep-timer (password-interface *browser*)))))
      (echo-warning "No password manager found.")))
