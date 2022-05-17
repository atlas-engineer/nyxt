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
  (ensure-zoom-ratio-range #'+ buffer)
  (setf (ffi-buffer-zoom-level buffer) (current-zoom-ratio buffer)))

(define-command unzoom-page (&key (buffer (current-buffer)))
  "Zoom out the current page."
  (ensure-zoom-ratio-range #'- buffer)
  (setf (ffi-buffer-zoom-level buffer) (current-zoom-ratio buffer)))

(define-command reset-page-zoom (&key (buffer (current-buffer))
                                 (ratio (zoom-ratio-default buffer)))
  "Reset the page zoom to the zoom-ratio-default."
  (setf (ffi-buffer-zoom-level buffer) (setf (current-zoom-ratio buffer) ratio)))
