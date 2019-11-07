(in-package :next)

(defun copy-password-completion-filter (password-instance)
  (let ((password-list (password:list-passwords password-instance)))
    (lambda (input)
      (fuzzy-match input password-list))))

(define-command save-new-password ()
  "Save password to password interface."
  (if (password-interface *interface*)
      (with-result* ((password-name (read-from-minibuffer
                                     (make-minibuffer
                                      :input-prompt "Name for new password:")))
                     (new-password (read-from-minibuffer
                                    (make-minibuffer
                                     :invisible-input-p t
                                     :input-prompt "New password (leave empty to generate):"))))
        (password:save-password (password-interface *interface*)
                                password-name
                                new-password))
      (echo-warning "No password manager found.")))

(defmacro with-password (password-interface &body body)
  `(if (password:password-correct-p ,password-interface)
       ,@body))

(define-command copy-password ()
  "Copy chosen password from minibuffer."
  (if (password-interface *interface*)
      (with-password (password-interface *interface*)
        (with-result (password-name
                      (read-from-minibuffer
                       (make-minibuffer
                        :completion-function
                        (copy-password-completion-filter
                         (password-interface *interface*)))))
          (password:clip-password (password-interface *interface*) password-name)))
      (echo-warning "No password manager found.")))
