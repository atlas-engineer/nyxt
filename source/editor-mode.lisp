;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/editor-mode
    (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for editors."))
(in-package :nyxt/editor-mode)

(define-mode editor-mode ()
  "Mode for editor modes to extend."
  ((keymap-scheme (define-scheme "base"
                    scheme:cua
                    (list
                     "C-o" 'editor-open-file
                     "C-s" 'editor-write-file)
                    scheme:emacs
                    (list
                     "C-x C-f" 'editor-open-file
                     "C-x C-s" 'editor-write-file)
                    scheme:vi-normal
                    (list
                     "C-o" 'editor-open-file))
                  :type keymap:scheme))
  (:documentation "This class is used to define a protocol for editors to implement."))

(defmethod editor ((editor-buffer editor-buffer))
  (find-submode editor-buffer 'editor-mode))

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

(define-command nyxt::open-new-editor-with-file ()
  "Open a new editor and query a file."
  (let ((buffer (make-editor-buffer)))
    (set-current-buffer buffer)
    (editor-open-file :buffer buffer)))
