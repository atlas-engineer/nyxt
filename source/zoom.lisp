;;; zoom.lisp --- functions for zooming in and out of the page

(in-package :next)

(defun ensure-zoom-ratio-range (zoom)
  (let* ((buffer (active-buffer *interface*))
         (ratio (funcall zoom (current-zoom-ratio buffer) (zoom-ratio-step buffer))))
    (setf ratio (max ratio (zoom-ratio-min buffer)))
    (setf ratio (min ratio (zoom-ratio-max buffer)))
    (setf (current-zoom-ratio buffer) ratio)))

(define-parenscript %zoom-in-page ()
  (ps:lisp (ensure-zoom-ratio-range #'+))
  (ps:let ((style (ps:chain document body style)))
    (setf (ps:@ style zoom) (ps:lisp (current-zoom-ratio (active-buffer *interface*))))))

(define-parenscript %zoom-out-page ()
  (ps:lisp (ensure-zoom-ratio-range #'-))
  (ps:let ((style (ps:chain document body style)))
    (setf (ps:@ style zoom) (ps:lisp (current-zoom-ratio (active-buffer *interface*))))))

(defun %unzoom-page ()
  (let ((buffer (ps:lisp (active-buffer *interface*))))
    (ps:ps
      (ps:lisp (setf (current-zoom-ratio buffer) (zoom-ratio-default buffer)))
      (setf (ps:chain document body style zoom) (ps:lisp (zoom-ratio-default buffer))))))

(define-command zoom-in-page ()
  "Zoom in the current page."
  (%%buffer-evaluate-javascript *interface* (active-buffer *interface*) (%zoom-in-page)))

(define-command zoom-out-page ()
  "Zoom out the current page."
  (%%buffer-evaluate-javascript *interface* (active-buffer *interface*) (%zoom-out-page)))

(define-command unzoom-page ()
  "Unzoom the page."
  (%%buffer-evaluate-javascript *interface* (active-buffer *interface*) (%unzoom-page)))
