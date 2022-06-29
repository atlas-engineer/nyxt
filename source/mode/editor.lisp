;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/editor-mode
    (:documentation "Mode for editors."))
(in-package :nyxt/editor-mode)

(define-mode editor-mode ()
  "Mode for editor modes to extend. Importantly, it is required to implement the
methods get/set-content for each editor-mode. This will allow your mode to
get/set-content (which is necessary for operation)."
  ((keymap-scheme (define-keyscheme-map "editor" ()
                    keyscheme:cua
                    (list
                     "C-o" 'editor-open-file
                     "C-s" 'editor-write-file)
                    keyscheme:emacs
                    (list
                     "C-x C-f" 'editor-open-file
                     "C-x C-s" 'editor-write-file)
                    keyscheme:vi-normal
                    (list
                     "C-o" 'editor-open-file))
                  :type keymaps:keyscheme))
  (:toggler-command-p nil))

(defmethod enable ((editor editor-mode) &key) ; TODO: Use an internal page instead of this HTML injection?
  (let* ((content (spinneret:with-html-string
                    (:head (:style (style (buffer editor))))
                    (:body (:p "Please configure an editor mode to use an editor buffer."))))
         (insert-content (ps:ps (ps:chain document (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer editor) insert-content)))

(define-class editor-buffer (context-buffer modable-buffer document-buffer input-buffer)
  ((nyxt:url (quri:uri ""))
   (nyxt:title "*Editor*"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "Each editor buffer matches a file. Each editor buffer
contains an `nyxt/editor-mode:editor-mode' instance (or a subclass thereof)."))

(defmethod file ((buffer editor-buffer))
  (uiop:parse-native-namestring (quri:uri-path (url buffer))))

(defmethod nyxt:default-modes :around ((buffer editor-buffer))
  ;; REVIEW: Really remove document-mode from editor-buffer?
  (remove 'document-mode (call-next-method)))

(defmethod editor ((editor-buffer editor-buffer))
  (find-submode 'editor-mode editor-buffer))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(defgeneric get-content (editor)
  (:documentation "Get the content of the editor."))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(defgeneric set-content (editor content)
  (:documentation "Set the content of the editor."))

(defmethod write-file-with-editor ((buffer editor-buffer) &key (if-exists :error))
  (alexandria:if-let ((editor (editor buffer)))
    (alexandria:write-string-into-file (get-content editor)
                                       (file buffer)
                                       :if-exists if-exists)
    (echo "Editor buffer cannot write file without configured editor mode.")))

(defmethod open-file-with-editor ((buffer editor-buffer) file)
  (alexandria:if-let ((editor (editor buffer)))
    (if (uiop:file-exists-p file)
        (set-content editor (uiop:read-file-string file))
        (set-content editor ""))
    (echo "Editor buffer cannot open file without configured `editor-mode'.")))

(define-command editor-open-file (&key (buffer (current-buffer)))
  "Open a file in the internal editor."
  (let ((file (prompt1
                :prompt "Open file"
                :extra-modes '(nyxt/file-manager-mode:file-manager-mode)
                :input (uiop:native-namestring (uiop:getcwd))
                :sources
                (list (make-instance 'nyxt/file-manager-mode:file-source
                                     :name "Absolute file path"
                                     :return-actions '(identity))
                      (make-instance 'prompter:raw-source
                                     :name "New file")))))
    ;; TODO: Maybe make `editor-mode' and `editor-buffer' pathname-friendly?
    (let ((native-path (uiop:native-namestring file)))
      (setf (title buffer) native-path)
      (setf (url buffer) (quri::make-uri-file :path native-path)))
    (open-file-with-editor buffer file)))

(define-command editor-write-file (&key (buffer (current-buffer)) (if-exists :error))
  "Write the FILE of the BUFFER to storage."
  (if (uiop:file-exists-p (file buffer))
      (if-confirm ("Overwrite ~s?" (file buffer))
        (progn (write-file-with-editor buffer :if-exists :overwrite)
               (echo "File ~s saved." (file buffer)))
        (echo "File ~s not saved." (file buffer)))
      (progn  (write-file-with-editor buffer :if-exists if-exists)
              (echo "File ~s saved." (file buffer)))))

(define-command-global open-new-editor-with-file ()
  "Open a new editor and query a file."
  (let ((buffer (make-instance 'editor-buffer :title "*Editor*")))
    (set-current-buffer buffer)
    (editor-open-file :buffer buffer)))
