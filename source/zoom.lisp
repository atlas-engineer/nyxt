;;; zoom.lisp --- functions for zooming in and out of the page

(in-package :next)

(defun ensure-zoom-ratio-range (zoom)
  (let ((ratio (funcall zoom *current-zoom-ratio* *zoom-ratio-step*)))
    (setf ratio (max ratio *zoom-ratio-min*))
    (setf ratio (min ratio *zoom-ratio-max*))
    (setf *current-zoom-ratio* ratio)))

(define-parenscript %zoom-in-page ()
  (ps:lisp (ensure-zoom-ratio-range #'+))
  (ps:let ((style (ps:chain document body style)))
    (setf (ps:@ style zoom) (ps:lisp *current-zoom-ratio*))))

(define-parenscript %zoom-out-page ()
  (ps:lisp (ensure-zoom-ratio-range #'-))
  (ps:let ((style (ps:chain document body style)))
    (setf (ps:@ style zoom) (ps:lisp *current-zoom-ratio*))))

(define-parenscript %unzoom-page ()
  (ps:lisp (setf *current-zoom-ratio* *zoom-ratio-default*))
  (setf (ps:chain document body style zoom) (ps:lisp *zoom-ratio-default*)))

(define-command zoom-in-page ()
  "Zoom in the current page."
  (buffer-evaluate-javascript *interface* (active-buffer *interface*) (%zoom-in-page)))

(define-command zoom-out-page ()
  "Zoom out the current page."
  (buffer-evaluate-javascript *interface* (active-buffer *interface*) (%zoom-out-page)))

(define-command unzoom-page ()
  "Unzoom the page."
  (buffer-evaluate-javascript *interface* (active-buffer *interface*) (%unzoom-page)))
