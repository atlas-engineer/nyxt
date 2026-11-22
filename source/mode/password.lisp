;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/password
  (:documentation "Package for `password-mode', mode to interface with password
managers.

Relies on the `password' library for most package manager interactions. In
particular:
- Specifies `password::execute' for KeePassXC to prompt for Yubikey tap.
- Specifies `password:complete-interface' to prompt for details for interfaces
  that need it.
- Adds a `with-password' macro relying on `password:password-correct-p' to
  decide whether the interface is properly connected and complete, and calling
  `password:complete-interface' if it's not.

Also note the internal `make-password-interface-user-classes' function to force
password interfaces to become `user-class'es and thus
`define-configuration'-friendly.

See the `password-mode' for the external user-facing APIs."))
(in-package :nyxt/mode/password)

(define-mode password-mode ()
  "Enable interface with third-party password managers.
You can customize the default interface with the mode slot `password-interface'.

See `nyxt/mode/password' package documentation for implementation details and
internal programming APIs."
  ((visible-in-status-p nil)
   (password-interface
    (make-password-interface)
    :type (or null password::password-interface)
    :documentation "The current password interface.
See `password:*interfaces*' for the list of all currently registered interfaces.
To use, say, KeepassXC, set this slot to

  (make-instance 'password:keepassxc-interface)

Password interfaces are configurable through a `customize-instance' method.")))

(define-configuration context-buffer
  ((default-modes (cons 'password-mode %slot-value%))))

(defmethod password-interface ((buffer buffer))
  (when-let ((mode (find-mode buffer 'password-mode)))
    (password-interface mode)))

(defun make-password-interface-user-classes ()
  "Define classes with the `user-class' metaclass so that users may use
`customize-instance'."
  (dolist (interface password:*interfaces*)
    (closer-mop:ensure-class (intern (symbol-name interface))
                             :direct-superclasses (list interface)
                             :metaclass 'user-class)))

(make-password-interface-user-classes)

(defun make-password-interface ()
  "Return the instance of the first password interface among
`password:*interfaces*'
for which the `executable' slot is non-nil."
  (some (lambda (interface)
          (let ((instance (make-instance interface)))
            (when (password:executable instance)
              instance)))
        password:*interfaces*))

(defun clip-password-action (source)
  (lambda-command clip-password (password-name)
    "Save password to clipboard."
    (let ((buffer (buffer source))
          (password-name (first password-name)))
      (password:clip-password (password-interface buffer)
                              :password-name password-name)
      (echo "Password saved to clipboard for ~a seconds."
            (password:sleep-timer (password-interface buffer))))))

(defun clip-username-action (source)
  (lambda-command clip-username (password-name)
    "Save username to clipboard."
    (let ((buffer (buffer source))
          (password-name (first password-name)))
      (if (password:clip-username (password-interface buffer)
                                  :password-name password-name)
          (echo "Username saved to clipboard.")
          (echo "No username found.")))))

(define-class password-source (prompter:source)
  ((prompter:name "Passwords")
   (buffer :accessor buffer :initarg :buffer)
   (password-instance :accessor password-instance :initarg :password-instance)
   (prompter:constructor
    (lambda (source)
      (password:list-passwords (password-instance source))))))

(defmethod initialize-instance :after ((source password-source)
                                       &key (actions-on-return nil actions-supplied-p)
                                       &allow-other-keys)
  (declare (ignore actions-on-return))
  (unless actions-supplied-p
    (setf (slot-value source 'prompter:actions-on-return)
          (list (clip-password-action source)
                (clip-username-action source)))))

(defun password-debug-info (buffer)
  (when-let ((interface (password-interface buffer)))
    (log:debug "Password interface ~a uses executable ~s."
               (class-name (class-of interface))
               (password:executable interface))))

(defmacro with-password (password-interface &body body)
  `(if (password:password-correct-p ,password-interface)
       ,@body
       (progn
         (password:complete-interface ,password-interface)
         ,@body)))

(define-command save-new-password ((mode password-mode))
  "Save password to password interface."
  (let ((buffer (buffer mode))
        (interface (password-interface mode)))
    (password-debug-info buffer)
    (cond
      ((and interface
            (nyxt:has-method-p interface #'password:save-password))
       (with-password interface
         (let* ((password-name
                  (prompt1 :prompt "Name for new password"
                           :input (or (quri:uri-domain (url buffer))
                                      "")
                           :sources 'prompter:raw-source))
                (new-password
                  (prompt1 :prompt "New password (leave empty to generate)"
                           :sources 'prompter:raw-source
                           :invisible-input-p t))
                (username (prompt1 :prompt "Username (can be empty)"
                                   :sources 'prompter:raw-source)))
           (password:save-password interface
                                   :username username
                                   :password-name password-name
                                   :password new-password))))
      ((null interface)
       (echo-warning "No password manager found."))
      (t (echo-warning "Password manager ~s does not support saving passwords."
                       (string-downcase
                        (class-name (class-of interface))))))))

(defmethod password::execute :before
    ((password-interface password:keepassxc-interface) (arguments list)
     &rest run-program-args &key &allow-other-keys)
  (declare (ignore arguments run-program-args))
  (when (password::yubikey-slot password-interface)
    (echo "Tap your Yubikey to prove KeePassXC database access")))

(defmethod password:complete-interface
    ((password-interface password:keepassxc-interface))
  (loop until (password:password-correct-p password-interface)
        unless (and (password::password-file password-interface)
                    (string-equal "kdbx"
                                  (pathname-type
                                   (pathname (password::password-file
                                              password-interface)))))
          do (setf (password::password-file password-interface)
                   (uiop:native-namestring
                    (prompt1
                     :prompt "Password database file (.kdbx)"
                     :extra-modes 'nyxt/mode/file-manager:file-manager-mode
                     :sources (make-instance 'nyxt/mode/file-manager:file-source
                                             :extensions '("kdbx")))))
        unless (password::key-file password-interface)
          do (if-confirm ("Do you use key file for password database locking?")
                 (setf (password::key-file password-interface)
                       (uiop:native-namestring
                        (prompt1
                         :prompt "Password database key file"
                         :extra-modes 'nyxt/mode/file-manager:file-manager-mode
                         :sources (make-instance
                                   'nyxt/mode/file-manager:file-source)))))
        unless (password::yubikey-slot password-interface)
          do (if-confirm ("Do you use Yubikey for password database locking")
                 (setf (password::yubikey-slot password-interface)
                       (prompt1 :prompt "Yubikey slot[:port]"
                                :sources (make-instance 'prompter:raw-source))))
        do (setf (password::master-password password-interface)
                 (prompt1
                  :prompt
                  (format nil "Database password for ~a (leave empty if none)"
                          (password::password-file password-interface))
                          :sources 'prompter:raw-source
                          :height :fit-to-prompt
                          :invisible-input-p t))))

(define-command copy-password ((mode password-mode))
  "Query password and save to clipboard.
See also `copy-password-prompt-details'."
  (let ((buffer (buffer mode))
        (interface (password-interface mode)))
    (password-debug-info buffer)
    (if interface
        (with-password interface
          (prompt1 :prompt "Password"
                   :input (quri:uri-domain (url buffer))
                   :sources (make-instance
                             'password-source
                             :buffer buffer
                             :password-instance interface)))
        (echo-warning "No password manager found."))))

(define-command copy-username ((mode password-mode))
  "Query username and save to clipboard."
  (let ((buffer (buffer mode))
        (interface (password-interface mode)))
    (password-debug-info buffer)
    (if interface
        (with-password interface
          (let ((source (make-instance
                         'password-source
                         :buffer buffer
                         :password-instance interface)))
            (setf (slot-value source 'prompter:actions-on-return)
                  (list (clip-username-action source)))
            (prompt :prompt "Username"
                    :input (quri:uri-domain (url buffer))
                    :sources source)))
        (echo-warning "No password manager found."))))
