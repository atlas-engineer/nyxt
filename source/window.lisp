;;; window.lisp --- commands for managing windows

(in-package :next)

(define-command delete-window ()
  "Delete the currently active window."
  (let ((active-window (window-active *interface*)))
    (when active-window
      (window-delete *interface* active-window))))

(define-command make-window ()
  "Create a new window."
  (window-make *interface*))
