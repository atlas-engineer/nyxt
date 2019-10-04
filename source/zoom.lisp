;;; zoom.lisp --- functions for zooming in and out of the page

(in-package :next)

(defun ensure-zoom-ratio-range (zoom &optional (buffer (current-buffer)))
  (let* ((ratio (funcall zoom (current-zoom-ratio buffer) (zoom-ratio-step buffer))))
    (setf ratio (max ratio (zoom-ratio-min buffer)))
    (setf ratio (min ratio (zoom-ratio-max buffer)))
    (setf (current-zoom-ratio buffer) ratio)))

(define-parenscript %zoom-in-page ()
  (ps:lisp (ensure-zoom-ratio-range #'+ %buffer))
  (ps:let ((style (ps:chain document body style)))
    (setf (ps:@ style zoom) (ps:lisp (current-zoom-ratio %buffer)))))

(define-parenscript %zoom-out-page ()
  (ps:lisp (ensure-zoom-ratio-range #'- %buffer))
  (ps:let ((style (ps:chain document body style)))
    (setf (ps:@ style zoom) (ps:lisp (current-zoom-ratio %buffer)))))

(define-parenscript %unzoom-page ()
  (ps:lisp (setf (current-zoom-ratio %buffer) (zoom-ratio-default %buffer)))
  (setf (ps:chain document body style zoom) (ps:lisp (zoom-ratio-default %buffer))))

(define-command zoom-in-page (&key (buffer (current-buffer)))
  "Zoom in the current page."
  (%zoom-in-page :buffer buffer))

(define-command zoom-out-page (&key (buffer (current-buffer)))
  "Zoom out the current page."
  (%zoom-out-page :buffer buffer))

(define-command unzoom-page (&key (buffer (current-buffer)))
  "Unzoom the page."
  (%unzoom-page :buffer buffer))
