;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun open-with-external-editor (&optional input-text)
  "Edit `input-text' using `external-editor-program'.
Create a temporary file and return its content.  The editor runs synchronously
so invoke on a separate thread when possible."
  (uiop:with-temporary-file (:directory (uiop:xdg-data-home +data-root+)
                             :pathname p)
    (when (> (length input-text) 0)
      (with-open-file (f p :direction :io
                           :if-exists :append)
        (write-sequence input-text f)))
    (log:debug "External Editor: ~a opening: ~a"
               (external-editor-program *browser*) p)
    ;; launch-program runs asynchronously but it misbehaves
    (uiop:run-program (list (external-editor-program *browser*)
                               (uiop:native-namestring p)) :ignore-error-status t)
    (uiop:read-file-string p)))

(define-parenscript select-input-field ()
  (let ((active-tag (ps:chain document active-element tag-name)))
    (when (or (string= active-tag "INPUT")
              (string= active-tag "TEXTAREA"))
      (ps:chain document active-element (select)))))

(define-parenscript undo-selection ()
  (ps:chain window (get-selection) (remove-all-ranges)))

;; known issues:
;; fails on duckduckgo's search bar because it loses its focus
(define-command fill-input-from-external-editor ()
  "Edit the current input field using `external-editor-program'."
  (select-input-field)
  (bt:make-thread
   (lambda () (%paste :input-text (open-with-external-editor (%copy)))
         (undo-selection))))
