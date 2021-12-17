;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun %edit-with-external-editor (&optional input-text)
  "Edit `input-text' using `external-editor-program'.
Create a temporary file and return its content.  The editor runs synchronously
so invoke on a separate thread when possible."
  (uiop:with-temporary-file (:directory (uiop:xdg-data-home +data-root+)
                             :pathname p)
    (when (> (length input-text) 0)
      (with-open-file (f p :direction :io
                           :if-exists :append)
        (write-sequence input-text f)))
    (log:debug "External editor ~s opens ~s"
               (external-editor-program *browser*) p)
    (with-protect ("Failed editing: ~a" :condition)
      (uiop:run-program (append (external-editor-program *browser*)
                                (list (uiop:native-namestring p)))
                        :ignore-error-status t))
    (uiop:read-file-string p)))

(define-parenscript select-input-field ()
  (let ((active-element (ps:chain document active-element)))
    (when (nyxt/ps:element-editable-p active-element)
      (ps:chain active-element (select)))))

(define-parenscript move-caret-to-end ()
  ;; Inspired by https://stackoverflow.com/questions/4715762/javascript-move-caret-to-last-character.
  (let ((el (ps:chain document active-element)))
    (if (string= (ps:chain (typeof (ps:@ el selection-start)))
                 "number")
        (progn
          (setf (ps:chain el selection-end)
                (ps:chain el value length))
          (setf (ps:chain el selection-start)
                (ps:chain el selection-end)))
        (when (not (string= (ps:chain (typeof (ps:@ el create-text-range)))
                            "undefined"))
          (ps:chain el (focus))
          (let ((range (ps:chain el (create-text-range))))
            (ps:chain range (collapse false))
            (ps:chain range (select)))))))

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

(define-command edit-with-external-editor ()
  "Edit the current input field using `external-editor-program'."
  (if (external-editor-program *browser*)
      (run-thread
        (select-input-field)
        (%paste :input-text (%edit-with-external-editor (%copy)))
        (move-caret-to-end))
      (echo-warning "Please set `external-editor-program' browser slot.")))

(define-command edit-user-file-with-external-editor ()
  "Edit the queried user file using `external-editor-program'.
If the user file is GPG-encrypted, the editor must be capable of decrypting it."
  (if (external-editor-program *browser*)
      (let* ((file (prompt1 :prompt "Edit user file in external editor"
                            :sources 'data-path-source))
             (path (expand-path file)))

        (echo "Using \"~{~a~^ ~}\" to edit ~s." (external-editor-program *browser*) path)
        (uiop:launch-program `(,@(external-editor-program *browser*)
                               ,path)))
      (echo-warning "Please set `external-editor-program' browser slot.")))

(defun %view-source-with-external-editor ()
  "View page source using `external-editor-program'.
Create a temporary file. The editor runs synchronously so invoke on a
separate thread when possible."
  (let ((page-source (if (web-buffer-p (current-buffer))
                         (plump:serialize (document-model (current-buffer)) nil)
                         (ffi-buffer-get-document (current-buffer)))))
    (uiop:with-temporary-file (:directory (uiop:xdg-data-home nyxt::+data-root+)
                               :pathname p)
      (if (> (length page-source) 0)
          (progn
            (alexandria:write-string-into-file page-source p :if-exists :supersede)
            (log:debug "External editor ~s opens ~s"
                       (external-editor-program *browser*) p)
            (with-protect ("Failed editing: ~a" :condition)
              (uiop:run-program (append (external-editor-program *browser*)
                                        (list (uiop:native-namestring p)))
                                :ignore-error-status t)))
          (echo-warning "Nothing to edit.")))))

(define-command-global view-source-with-external-editor ()
  "Edit the current page source using `external-editor-program'.
Has no effect on the page, use only to look at sources!"
  (if (external-editor-program *browser*)
      (run-thread
        (%view-source-with-external-editor))
      (echo-warning "Please set `external-editor-program' browser slot.")))
