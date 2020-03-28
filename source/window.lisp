;;; window.lisp --- commands for managing windows

(in-package :next)

(defmethod object-string ((window window))
  (match (active-buffer window)
    ((guard b b)
     (object-string b))
    (_ (format nil "<#WINDOW ~a>" (id window)))))

(defun window-completion-filter ()
  (let ((windows (window-list)))
    (lambda (input)
      (fuzzy-match input windows))))

(declaim (ftype (function (browser)) window-make))
(defun window-make (browser)
  (let* ((window (ffi-window-make browser)))
    (setf (gethash (id window) (windows browser)) window)
    (unless (slot-value browser 'last-active-window)
      (setf (slot-value browser 'last-active-window) window))
    (hooks:run-hook (window-make-hook browser) window)
    window))

(declaim (ftype (function (window)) window-delete))
(defun window-delete (window)
  "This function must be called by the renderer when a window is deleted."
  (ffi-window-delete window)
  (hooks:run-hook (window-delete-hook window) window)
  (remhash (id window) (windows *browser*))
  (when (zerop (hash-table-count (windows *browser*)))
    (quit)))

(define-command delete-window ()
  "Delete the queried window(s)."
  (with-result (windows (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete window(s)"
                          :multi-selection-p t
                          :completion-function (window-completion-filter))))
    (mapcar #'delete-current-window windows)))

(define-command delete-current-window (&optional (window (current-window)))
  "Delete WINDOW, or the currently active window if unspecified."
  (let ((window-count (hash-table-count (windows *browser*))))
    (cond ((and window (> window-count 1))
           (ffi-window-delete window))
          (window
           (echo "Can't delete sole window.")))))

(define-command make-window (&optional buffer)
  "Create a new window."
  (let ((window (window-make *browser*))
        (buffer (or buffer (make-buffer))))
    (window-set-active-buffer window buffer)
    (values window buffer)))

(define-command fullscreen-current-window (&optional (window (current-window)))
  "Fullscreen WINDOW, or the currently active window if unspecified."
  (ffi-window-fullscreen window))

(define-command unfullscreen-current-window (&optional (window (current-window)))
  "Unfullscreen WINDOW, or the currently active window if unspecified."
  (ffi-window-unfullscreen window))

