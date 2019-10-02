(in-package :next)

(defun copy-password-completion-filter (password-instance)
  (let ((password-list (password:list-passwords password-instance)))
    (lambda (input)
      (fuzzy-match input password-list))))

(define-command save-new-password ()
  "Save password to password interface."
  (with-result* ((password-name (read-from-minibuffer
                                 (make-instance 'minibuffer
                                                :input-prompt "Name for new password:")))
                 (new-password (read-from-minibuffer
                                (make-instance 'minibuffer
                                               :invisible-input-p t
                                               :input-prompt "New password:"))))
    (password:save-password (password-interface *interface*)
                            password-name
                            new-password)))

(defmacro with-password (password-interface &body body)
  `(if (password:password-correct-p ,password-interface)
       ,@body))

(define-command copy-password ()
  "Copy chosen password from minibuffer."
  (with-password (password-interface *interface*)
    (with-result (password-name
                  (read-from-minibuffer
                   (make-instance 'minibuffer
                                  :completion-function
                                  (copy-password-completion-filter
                                   (password-interface *interface*)))))
      (password:clip-password (password-interface *interface*) password-name))))
