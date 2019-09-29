;;; recent-buffers.lisp --- Manage list of recent buffers.

(in-package :next)
(annot:enable-annot-syntax)

@export
(defmethod buffer-match-predicate ((buffer buffer))
  (lambda (other-buffer)
    (when other-buffer
      (and (string= (url buffer) (url other-buffer))
           (string= (title buffer) (title other-buffer))))))

(defun recent-buffer-completion-filter ()
  (let ((buffers (ring:recent-list (recent-buffers *interface*))))
    (lambda (input)
      (fuzzy-match input buffers))))

(define-command reopen-buffer ()
  "Reopen a deleted buffer via minibuffer input."
  (with-result (buffers (read-from-minibuffer
                         (make-instance 'minibuffer
                                        :input-prompt "Reopen buffer:"
                                        :multi-selection-p t
                                        :completion-function (recent-buffer-completion-filter))))
    (dolist (buffer buffers)
      (ring:delete-match (recent-buffers *interface*) (buffer-match-predicate buffer))
      (reload-current-buffer (rpc-buffer-make :dead-buffer buffer))
      (when (and (eq buffer (first buffers))
                 (focus-on-reopened-buffer-p *interface*))
        (set-current-buffer buffer)))))

(define-command reopen-last-buffer ()
  "Open a new buffer with the URL of the most recently deleted buffer."
  (if (plusp (ring:item-count (recent-buffers *interface*)))
      (let ((buffer (rpc-buffer-make
                     :dead-buffer (ring:pop-most-recent (recent-buffers *interface*)))))
        (reload-current-buffer buffer)
        (when (focus-on-reopened-buffer-p *interface*)
          (set-current-buffer buffer)))
      (echo "There are no recently-deleted buffers.")))

(define-deprecated-command undo-buffer-deletion ()
  "Deprecated by `reopen-last-buffer'."
  (reopen-last-buffer))
