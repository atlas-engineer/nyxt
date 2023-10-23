;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun %edit-with-external-editor (content &key (read-only nil))
  "Edit CONTENT using `external-editor-program'.
Create a temporary file and return its content.  The editor runs synchronously
so invoke on a separate thread when possible."
  (with-accessors ((cmd external-editor-program)) *browser*
    (uiop:with-temporary-file (:directory (files:expand (make-instance 'nyxt-data-directory))
                               :pathname p)
      (with-open-file (f p :direction :io :if-exists :supersede) (write-sequence content f))
      (log:debug "External editor ~s opens ~s" cmd p)
      (with-protect ("Failed editing: ~a. See `external-editor-program' slot." :condition)
        (uiop:run-program `(,@cmd ,(uiop:native-namestring p))))
      (unless read-only (uiop:read-file-string p)))))

;; BUG: Fails when the input field loses its focus, e.g the DuckDuckGo search
;; bar.  A possible solution is to keep track of the last focused element for
;; each buffer.
(define-parenscript select-input-field ()
  (let ((active-element (nyxt/ps:active-element document)))
    (when (nyxt/ps:element-editable-p active-element)
      (ps:chain active-element (select)))))

(define-command-global edit-with-external-editor ()
  "Edit the current input field using `external-editor-program'."
  (run-thread "external editor"
    (select-input-field)
    (ffi-buffer-paste (current-buffer)
                      (%edit-with-external-editor (ffi-buffer-copy (current-buffer))))))

;; Should belong to user-files.lisp but the define-command-global macro is
;; defined later.
(define-command-global edit-user-file-with-external-editor ()
  "Edit the queried user file using `external-editor-program'.
If the user file is GPG-encrypted, the editor must be capable of decrypting it."
  (let ((cmd (external-editor-program *browser*))
        (path (files:expand (prompt1 :prompt "Edit user file in external editor"
                                     :sources 'user-file-source))))
    (echo "Issued \"~{~a~^ ~}\" to edit ~s." cmd path)
    (with-protect ("Failed editing: ~a. See `external-editor-program' slot." :condition)
      (uiop:run-program `(,@cmd ,(uiop:native-namestring path))))))

(define-command-global view-source-with-external-editor (&optional (buffer (current-buffer)))
  "View the current page source using `external-editor-program'."
  (run-thread "source viewer"
    (%edit-with-external-editor (if (web-buffer-p buffer)
                                    (plump:serialize (document-model buffer) nil)
                                    (ffi-buffer-get-document buffer))
                                :read-only t)))
