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
    (log:debug "External Editor: ~s opening: ~s"
               (external-editor-program *browser*) p)
    (uiop:run-program (list (external-editor-program *browser*)
                            (uiop:native-namestring p))
                      :ignore-error-status t)
    (uiop:read-file-string p)))

(define-parenscript select-input-field ()
  (let ((active-tag (ps:chain document active-element tag-name)))
    (when (or (string= active-tag "INPUT")
              (string= active-tag "TEXTAREA"))
      (ps:chain document active-element (select)))))

(define-parenscript set-caret-on-end ()
  (ps:chain document (get-selection) (collapse-to-end)))

;; TODO:

;; BUG: Fails when the input field loses its focus, e.g the duckduckgo search
;; bar.  Can probably be solved with JS.

;; There could be an optional exiting behaviour -- set-caret-on-end or
;; undo-selection.

;; (define-parenscript undo-selection ()
;;   (ps:chain window (get-selection) (remove-all-ranges)))

;; It could be extended so that the coordinates of the cursor (line,column)
;; could be shared between Nyxt and the external editor.  A general solution
;; can't be achieved since not all editors, e.g. vi, accept the syntax
;; `+line:column' as an option to start the editor.

(define-command fill-input-from-external-editor ()
  "Edit the current input field using `external-editor-program'."
  (bt:make-thread
   (lambda ()
     (select-input-field)
     (%paste :input-text (open-with-external-editor (%copy)))
     (set-caret-on-end))))
