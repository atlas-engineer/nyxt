;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(defun ensure-zoom-ratio-range (zoom &optional (buffer (current-buffer)))
  (let* ((ratio (funcall zoom (current-zoom-ratio buffer) (zoom-ratio-step buffer))))
    (setf ratio (max ratio (zoom-ratio-min buffer)))
    (setf ratio (min ratio (zoom-ratio-max buffer)))
    (setf (current-zoom-ratio buffer) ratio)))

(define-command zoom-page (&key (buffer (current-buffer)))
  "Zoom in the current page."
  (ensure-zoom-ratio-range #'+ (current-buffer))
  (ffi-buffer-set-zoom-level buffer (current-zoom-ratio (current-buffer))))

(define-command unzoom-page (&key (buffer (current-buffer)))
  "Zoom out the current page."
  (ensure-zoom-ratio-range #'- (current-buffer))
  (ffi-buffer-set-zoom-level buffer (current-zoom-ratio (current-buffer))))

(define-command reset-page-zoom (&key (buffer (current-buffer))
                                      (ratio (zoom-ratio-default (current-buffer))))
  "Reset the page zoom to the zoom-ratio-default."
  (ffi-buffer-set-zoom-level buffer (setf (current-zoom-ratio (current-buffer)) ratio)))
