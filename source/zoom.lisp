(in-package :nyxt/web-mode)

(defun ensure-zoom-ratio-range (zoom &optional (buffer (current-buffer)))
  (let* ((ratio (funcall zoom (current-zoom-ratio buffer) (zoom-ratio-step buffer))))
    (setf ratio (max ratio (zoom-ratio-min buffer)))
    (setf ratio (min ratio (zoom-ratio-max buffer)))
    (setf (current-zoom-ratio buffer) ratio)))

(define-command zoom-in-page (&key (buffer (current-buffer)))
  "Zoom in the current page."
  (pflet ((zoom ()
            (ps:lisp (ensure-zoom-ratio-range #'+ (current-buffer)))
            (ps:let ((style (ps:chain document body style)))
              (setf (ps:@ style zoom) (ps:lisp (current-zoom-ratio (current-buffer)))))))
    (with-current-buffer buffer
      (zoom))))

(define-command zoom-out-page (&key (buffer (current-buffer)))
  "Zoom out the current page."
  (pflet ((zoom-out ()
            (ps:lisp (ensure-zoom-ratio-range #'- (current-buffer)))
            (ps:let ((style (ps:chain document body style)))
              (setf (ps:@ style zoom) (ps:lisp (current-zoom-ratio (current-buffer)))))))
    (with-current-buffer buffer
      (zoom-out))))

(define-command unzoom-page (&key (buffer (current-buffer))
                             (ratio (zoom-ratio-default (current-buffer))))
  "Unzoom the page."
  (pflet ((unzoom ()
            (ps:lisp (setf (current-zoom-ratio (current-buffer)) ratio))
            (setf (ps:chain document body style zoom) (ps:lisp ratio))))
    (with-current-buffer buffer
      (unzoom))))
