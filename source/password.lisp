(in-package :next)

(defun password-completion-filter (password-instance)
  (let ((password-list (password:list-passwords password-instance))
        (domain (domain (url (current-buffer)))))
    (when domain
      (let ((closest-match (find-if (alex:curry #'str:containsp domain) password-list)))
        (setf password-list (cons closest-match (delete closest-match password-list)))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) password-list))))

(defun password-debug-info ()
  (log:debug "Password interface ~a uses executable ~s."
             (when (password-interface *browser*)
               (class-name (class-of (password-interface *browser*))))
             (match (symbol-name (class-name (class-of (password-interface *browser*))))
               ;; With `symbol-name' we remove the package prefix.
               ("KEEPASSXC-INTERFACE" password:*keepassxc-cli-program*)
               ("SECURITY-INTERFACE" password:*security-cli-program*)
               ("PASSWORD-STORE-INTERFACE" password:*password-store-program*))))

(define-command save-new-password ()
  "Save password to password interface."
  (password-debug-info)
  (if (password-interface *browser*)
      (with-result* ((password-name (read-from-minibuffer
                                     (make-minibuffer
                                      :input-prompt "Name for new password"
                                      :input-buffer (or (domain (url (current-buffer)))
                                                        ""))))
                     (service (funcall (lambda (&key callback)
                                         (if (closer-mop:subclassp
                                              (class-of (password-interface *browser*))
                                              'password::security-interface)
                                             (read-from-minibuffer
                                              (make-minibuffer
                                               :input-prompt "Service"
                                               :callback callback))
                                             (funcall-safely callback nil)))))
                     (new-password (read-from-minibuffer
                                    (make-minibuffer
                                     :invisible-input-p t
                                     :input-prompt "New password (leave empty to generate)"))))
        (password:save-password (password-interface *browser*)
                                :password-name password-name
                                :password new-password
                                :service service))
      (echo-warning "No password manager found.")))

(defmacro with-password (password-interface &body body)
  `(if (password:password-correct-p ,password-interface)
       ,@body))

(define-command copy-password-prompt-details ()
  "Copy password prompting for all the details without completion."
  (password-debug-info)
  (if (password-interface *browser*)
      (with-result* ((password-name (read-from-minibuffer
                                     (make-minibuffer
                                      :input-prompt "Name of password")))
                     (service (read-from-minibuffer
                               (make-minibuffer
                                :input-prompt "Service"))))
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
        (with-result (password-name
                      (read-from-minibuffer
                       (make-minibuffer
                        :completion-function
                        (password-completion-filter
                         (password-interface *browser*)))))
          (password:clip-password (password-interface *browser*) :password-name password-name)
          (echo "Password saved to clipboard for ~a seconds." password:*sleep-timer*)))
      (echo-warning "No password manager found.")))
