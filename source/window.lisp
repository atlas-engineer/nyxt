;;; window.lisp --- commands for managing windows

(in-package :next)

(define-command delete-window ()
  "Delete the currently active window."
  (let ((active-window (window-active *interface*)))
    (cond ((and active-window (> (hash-table-count (windows *interface*)) 1))
           (window-delete *interface* active-window))
          (active-window (echo *minibuffer* "Can't delete sole window.")))))

(define-command make-window ()
  "Create a new window."
  (let ((window (window-make *interface*))
        (buffer (make-buffer)))
    (window-set-active-buffer *interface* window buffer)
    (values window buffer)))
