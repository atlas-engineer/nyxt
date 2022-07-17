;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/password-mode
    (:documentation "Interface with third-party password managers."))
(in-package :nyxt/password-mode)

(define-mode password-mode ()
  "Enable interface with third-party password managers.
You can customize the default interface with the mode slot `password-interface'.
To interact with the password manager, see commands like `copy-password' or
`save-new-password'."
  ((visible-in-status-p nil)
   (password-interface
    (make-password-interface)
    :type (or null password::password-interface)
    :documentation "The current password interface.
See `password:*interfaces*' for the list of all currently registered interfaces.
To use, say, KeepassXC, set this slot to

  (make-instance 'password:keepassxc-interface)

Password interfaces are configurable through a `customize-instance' method.")))

(defmethod password-interface ((buffer buffer))
  (password-interface (find-submode 'password-mode buffer)))

(defun make-password-interface-user-classes ()
  "Define classes with the `user-class' metaclass so that users may use `customize-instance'."
  (dolist (interface password:*interfaces*)
    (closer-mop:ensure-class (intern (symbol-name interface))
                             :direct-superclasses (list interface)
                             :metaclass 'user-class)))

(make-password-interface-user-classes)

(defun make-password-interface ()
  "Return the instance of the first password interface among `password:*interfaces*'
for which the `executable' slot is non-nil."
  (some (lambda (interface)
          (let ((instance (make-instance interface)))
            (when (password:executable instance)
              instance)))
        password:*interfaces*))

(defvar password-source-actions
  (list (lambda-command clip-password (password-name)
          (let ((buffer (buffer (current-source)))
                (password-name (first password-name)))
            (password:clip-password (password-interface buffer) :password-name password-name)
            (echo "Password saved to clipboard for ~a seconds." (password:sleep-timer (password-interface buffer)))))
        (lambda-command clip-username (password-name)
          (let ((buffer (buffer (current-source)))
                (password-name (first password-name)))
            (if (password:clip-username (password-interface buffer) :password-name password-name)
                (echo "Username saved to clipboard.")
                (echo "No username found."))))))

(define-class password-source (prompter:source)
  ((prompter:name "Passwords")
   (buffer :accessor buffer :initarg :buffer)
   (password-instance :accessor password-instance :initarg :password-instance)
   (prompter:hide-attribute-header-p :single)
   (prompter:constructor
    (lambda (source)
      (password:list-passwords (password-instance source))))
   (prompter:return-actions password-source-actions)))

(defun password-debug-info ()
  (alex:when-let ((interface (password-interface (current-buffer))))
    (log:debug "Password interface ~a uses executable ~s."
               (class-name (class-of interface))
               (password:executable interface))))

(define-command save-new-password (&optional (buffer (current-buffer)))
  "Save password to password interface."
  (password-debug-info)
  (cond
    ((and (password-interface buffer)
          (nyxt:has-method-p (password-interface (find-submode 'password-mode buffer))
                             #'password:save-password))
     (let* ((password-name (prompt1
                             :prompt "Name for new password"
                             :input (or (quri:uri-domain (url (current-buffer))) "")
                             :sources 'prompter:raw-source))
            (new-password (prompt1
                            :invisible-input-p t
                            :prompt "New password (leave empty to generate)"
                            :sources 'prompter:raw-source))
            (username (prompt1
                        :prompt "Username (can be empty)"
                        :sources 'prompter:raw-source)))
       (password:save-password (password-interface buffer)
                               :username username
                               :password-name password-name
                               :password new-password)))
    ((null (password-interface buffer))
     (echo-warning "No password manager found."))
    (t (echo-warning "Password manager ~s does not support saving passwords."
                     (string-downcase
                      (class-name (class-of (password-interface buffer))))))))

(defmethod password:complete-interface ((password-interface password:keepassxc-interface))
  (loop :until (and (password::password-file password-interface)
                    (string-equal "kdbx"
                                  (pathname-type (pathname (password::password-file
                                                            password-interface)))))
        :do (setf (password::password-file password-interface)
                  (uiop:native-namestring
                   (prompt1
                    :prompt "Password database file"
                    :extra-modes 'nyxt/file-manager-mode:file-manager-mode
                    :sources (make-instance 'nyxt/file-manager-mode:file-source
                                            :extensions '("kdbx"))))))
  (loop :until (password:password-correct-p password-interface)
        :do (setf (password::master-password password-interface)
                  (prompt1 :prompt "Database password"
                           :sources 'prompter:raw-source
                           :invisible-input-p t))))

(defmacro with-password (password-interface &body body)
  `(if (password:password-correct-p ,password-interface)
       ,@body
       (progn
         (password:complete-interface ,password-interface)
         ,@body)))

(define-command copy-password-prompt-details (&optional (buffer (current-buffer)))
  "Copy password prompting for all the details without suggestions."
  (password-debug-info)
  (if (password-interface buffer)
      (let* ((password-name (prompt1
                              :prompt "Name of password"
                              :sources 'prompter:raw-source))
             (service (prompt1
                        :prompt "Service"
                        :sources 'prompter:raw-source)))
        (handler-case
            (password:clip-password (password-interface buffer)
                                    :password-name password-name
                                    :service service)
          (error (c)
            (echo-warning "Error retrieving password: ~a" c))))
      (echo-warning "No password manager found.")))

(define-command copy-password (&optional (buffer (current-buffer)))
  "Query password and save to clipboard.
See also `copy-password-prompt-details'."
  (password-debug-info)
  (if (password-interface buffer)
      (with-password (password-interface buffer)
        (prompt1 :prompt "Password"
                 :input (quri:uri-domain (url buffer))
                 :sources (make-instance
                           'password-source
                           :buffer buffer
                           :password-instance (password-interface buffer))))
      (echo-warning "No password manager found.")))

(define-command copy-username (&optional (buffer (current-buffer)))
  "Query username and save to clipboard."
  (password-debug-info)
  (if (password-interface buffer)
      (with-password (password-interface buffer)
        (prompt :prompt "Username"
                :input (quri:uri-domain (url buffer))
                :sources (make-instance
                          'password-source
                          :buffer buffer
                          :password-instance (password-interface buffer)
                          :return-actions (sera:filter (sera:eqs 'clip-username)
                                                       password-source-actions
                                                       :key #'name))))
      (echo-warning "No password manager found.")))
