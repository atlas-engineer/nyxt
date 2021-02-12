;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun get-text-from-external-editor ()
  "Open an external editor to capture text. Please note, this function
is blocking, invoke on a separate thread when possible."
  (uiop:with-temporary-file (:directory (uiop:xdg-data-home +data-root+) :pathname p)
    (log:debug "External Editor: ~a opening: ~a" (external-editor-program *browser*) p)
    (uiop:run-program (list (external-editor-program *browser*) (uiop:native-namestring p))
                      :ignore-error-status t)
    (uiop:read-file-string p)))

(define-command fill-input-from-external-editor ()
  "This command will open your editor specified by your VISUAL-EDITOR of
the BROWSER class, if unset, it will default to your VISUAL environment
variable. It will then capture whatever text you enter and save in your
editor."
  (bt:make-thread
   (lambda ()
     (let ((text (get-text-from-external-editor)))
       (ffi-within-renderer-thread
        *browser*
        (lambda () (%paste :input-text text)))))))
