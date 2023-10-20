;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun %edit-with-external-editor (&optional input-text)
  "Edit `input-text' using `external-editor-program'.
Create a temporary file and return its content.  The editor runs synchronously
so invoke on a separate thread when possible."
  (uiop:with-temporary-file (:directory (files:expand (make-instance 'nyxt-data-directory))
                             :pathname p)
    (when (> (length input-text) 0)
      (with-open-file (f p :direction :io
                           :if-exists :append)
        (write-sequence input-text f)))
    (log:debug "External editor ~s opens ~s"
               (external-editor-program *browser*) p)
    (with-protect ("Failed editing: ~a" :condition)
      (uiop:run-program (append (uiop:ensure-list (external-editor-program *browser*))
                                (list (uiop:native-namestring p)))
                        :ignore-error-status t))
    (uiop:read-file-string p)))

(define-parenscript select-input-field ()
  (let ((active-element (nyxt/ps:active-element document)))
    (when (nyxt/ps:element-editable-p active-element)
      (ps:chain active-element (select)))))

(define-parenscript move-caret-to-end ()
  ;; Inspired by https://stackoverflow.com/questions/4715762/javascript-move-caret-to-last-character.
  (let ((el (nyxt/ps:active-element document)))
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

;; BUG: Fails when the input field loses its focus, e.g the DuckDuckGo search
;; bar.  Can probably be solved with JS.

;; There could be an optional exiting behavior -- set-caret-on-end or
;; undo-selection.

;; (define-parenscript undo-selection ()
;;   (ps:chain window (get-selection) (remove-all-ranges)))

;; It could be extended so that the coordinates of the cursor (line,column)
;; could be shared between Nyxt and the external editor.  A general solution
;; can't be achieved since not all editors, e.g. vi, accept the syntax
;; `+line:column' as an option to start the editor.

(define-command-global edit-with-external-editor ()
  "Edit the current input field using `external-editor-program'."
  (if (external-editor-program *browser*)
      (run-thread "external editor"
        (select-input-field)
        (ffi-buffer-paste (current-buffer) (%edit-with-external-editor (ffi-buffer-copy (current-buffer))))
        (move-caret-to-end))
      (echo-warning "Please set `external-editor-program' browser slot.")))

(define-command-global edit-user-file-with-external-editor ()
  "Edit the queried user file using `external-editor-program'.
If the user file is GPG-encrypted, the editor must be capable of decrypting it."
  (if (external-editor-program *browser*)
      (let* ((file (prompt1 :prompt "Edit user file in external editor"
                            :sources 'user-file-source))
             (path (files:expand file)))

        (echo "Using \"~{~a~^ ~}\" to edit ~s." (external-editor-program *browser*) path)
        (uiop:launch-program `(,@(uiop:ensure-list (external-editor-program *browser*))
                               ,(uiop:native-namestring path))))
      (echo-warning "Please set `external-editor-program' browser slot.")))

(defun %view-source-with-external-editor ()
  "View page source using `external-editor-program'.
Create a temporary file. The editor runs synchronously so invoke on a
separate thread when possible."
  (let ((page-source (if (web-buffer-p (current-buffer))
                         (plump:serialize (document-model (current-buffer)) nil)
                         (ffi-buffer-get-document (current-buffer)))))
    (uiop:with-temporary-file (:directory (files:expand (make-instance 'nyxt-data-directory))
                               :pathname p)
      (if (> (length page-source) 0)
          (progn
            (alexandria:write-string-into-file page-source p :if-exists :supersede)
            (log:debug "External editor ~s opens ~s"
                       (external-editor-program *browser*) p)
            (with-protect ("Failed editing: ~a" :condition)
              (uiop:run-program (append (uiop:ensure-list (external-editor-program *browser*))
                                        (list (uiop:native-namestring p)))
                                :ignore-error-status t)))
          (echo-warning "Nothing to edit.")))))

(define-command-global view-source-with-external-editor ()
  "Edit the current page source using `external-editor-program'.
Has no effect on the page, use only to look at sources!"
  (if (external-editor-program *browser*)
      (run-thread "source viewer"
        (%view-source-with-external-editor))
      (echo-warning "Please set `external-editor-program' browser slot.")))
