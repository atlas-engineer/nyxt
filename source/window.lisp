;;; window.lisp --- commands for managing windows

(in-package :next)

(define-command delete-window ()
  "Delete the currently active window."
  (let ((active-window (rpc-window-active *interface*))
        (window-count (hash-table-count (windows *interface*))))
    (cond ((and active-window (> window-count 1))
           (rpc-window-delete *interface* active-window))
          (active-window
           (echo "Can't delete sole window.")))))

(defun make-window (&optional buffer)
  "Create a new window."
  (let ((window (rpc-window-make *interface*))
        (buffer (or buffer (make-buffer))))
    (window-set-active-buffer *interface* window buffer)
    (values window buffer)))

(define-command new-window ()           ; TODO: This function is not needed anymore, turn `make-window' to a command instead.
  "Create a new window.
This command is meant to be used interactively.
For Lisp code, see `make-window'."
  (let ((window (rpc-window-make *interface*))
        (buffer (make-buffer)))
    (window-set-active-buffer *interface* window buffer)
    (values window buffer)))
