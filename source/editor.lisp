;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-mode editor-mode ()
  "Mode for editor modes to extend."
  ()
  (:documentation "This class is used to define a protocol for editors to implement."))

(defgeneric write-file (buffer-editor &key if-exists)
  (:documentation "Write the file to storage."))

(defgeneric open-file (buffer-editor file)
  (:documentation "Open the file in the editor."))

(defgeneric get-content (editor)
  (:documentation "Get the content of the editor."))

(defgeneric set-content (editor content)
  (:documentation "Set the content of the editor."))

(defmethod write-file ((buffer editor-buffer) &key (if-exists :error))
  (alexandria:write-string-into-file (get-content (editor buffer))
                                     (file buffer)
                                     :if-exists if-exists))

(defmethod open-file ((buffer editor-buffer) file)
  (if (uiop:file-exists-p file)
      (set-content (editor buffer) (uiop:read-file-string file))
      (set-content (editor buffer) "")))

(define-command editor-open-file (&key (buffer (current-buffer)))
  "Open a file in the internal editor."
  (let ((file (first (prompt
                      :prompt "Open file"
                      :sources (make-instance 'prompter:raw-source
                                              :name "Absolute file path")))))
    (open-file buffer file)
    (setf (file buffer) file)
    (setf (title buffer) file)
    (setf (url buffer) (quri:uri file))))

(define-command editor-write-file (&key (buffer (current-buffer)) (if-exists :error))
  "Write the FILE of the BUFFER.."
  (write-file buffer :if-exists if-exists)
  (echo "File ~a written to storage." (file buffer)))
