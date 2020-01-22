;;; window.lisp --- commands for managing windows

(in-package :next)

(defmethod object-string ((window window))
  (match (active-buffer window)
    ((guard b b)
     (object-string b))
    (_ (format nil "<#WINDOW ~a>" (id window)))))

(defun window-completion-filter ()
  (let ((windows (alexandria:hash-table-values (windows *interface*))))
    (lambda (input)
      (fuzzy-match input windows))))

(define-command delete-window ()
  "Delete the queried window(s)."
  (with-result (windows (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete window(s)"
                          :multi-selection-p t
                          :completion-function (window-completion-filter))))
    (mapcar #'delete-current-window windows)))

(define-command delete-current-window (&optional (window (ipc-window-active *interface*)))
  "Delete WINDOW, or the currently active window if unspecified."
  (let ((window-count (hash-table-count (windows *interface*))))
    (cond ((and window (> window-count 1))
           (ipc-window-delete window))
          (window
           (echo "Can't delete sole window.")))))

(define-command make-window (&optional buffer)
  "Create a new window."
  (let ((window (ipc-window-make *interface*))
        (buffer (or buffer (make-buffer))))
    (window-set-active-buffer window buffer)
    (values window buffer)))

(define-deprecated-command new-window ()
  "Deprecated by `make-window'."
  (make-window))
