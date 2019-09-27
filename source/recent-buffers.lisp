;;; recent-buffers.lisp --- Manage list of recent buffers.

(in-package :next)
(annot:enable-annot-syntax)

@export
(defmethod buffer-match-predicate ((buffer buffer))
  (lambda (other-buffer)
    (when other-buffer
      (and (string= (url buffer) (url other-buffer))
           (string= (title buffer) (title other-buffer))))))

(defun recent-buffer-completion-fn ()
  (let ((buffers (ring:recent-list (recent-buffers *interface*))))
    (lambda (input)
      (fuzzy-match input buffers))))

(define-command reopen-buffer ()
  "Reopen a deleted buffer via minibuffer input."
  (with-result (buffer (read-from-minibuffer
                        (make-instance 'minibuffer
                                       :input-prompt "Reopen buffer:"
                                       :completion-function (recent-buffer-completion-fn))))
    (ring:delete-match (recent-buffers *interface*) (buffer-match-predicate buffer))
    (reload-current-buffer (rpc-buffer-make :dead-buffer buffer))))

(define-command undo-buffer-deletion ()
  "Open a new buffer with the URL of the most recently deleted buffer."
  (if (plusp (ring:item-count (recent-buffers *interface*)))
      (reload-current-buffer (rpc-buffer-make
                              :dead-buffer (ring:pop-most-recent (recent-buffers *interface*))))
      (echo "There are no recently-deleted buffers.")))
