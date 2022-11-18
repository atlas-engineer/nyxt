;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/editor-mode
    (:documentation "Mode for editors."))
(in-package :nyxt/editor-mode)

(define-mode editor-mode ()
  "Mode for editor modes to extend.

Importantly, it is required to implement the methods `get-content',
`set-content', `markup' for each editor-mode. This will allow your mode
to get/set content from/to the file (which is necessary for operation).

To install the mode implementing the following, add this snippet to your config
(define-configuration nyxt/editor-mode::editor-buffer
  ((default-modes (cons 'your-editor-mode %slot-value%))))"
  ((keyscheme-map
    (define-keyscheme-map "editor-mode" ()
      keyscheme:default
      (list
       "C-r" 'reload-current-buffer
       "f11" 'toggle-fullscreen)
      keyscheme:cua
      (list
       "C-o" 'editor-open-file
       "C-s" 'editor-write-file
       "C-q" 'delete-current-buffer
       "C-tab" 'switch-buffer)
      keyscheme:emacs
      (list
       "C-x C-f" 'editor-open-file
       "C-x C-s" 'editor-write-file
       "C-x C-k" 'delete-current-buffer
       "C-x b" 'switch-buffer)
      keyscheme:vi-normal
      (list
       "C-o" 'editor-open-file
       "w" 'editor-write-file
       "R" 'reload-current-buffer
       "g b" 'switch-buffer
       "D" 'delete-current-buffer))))
  (:toggler-command-p nil))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(export-always 'get-content)
(defgeneric get-content (editor-submode)
  (:method ((editor editor-mode))
    (declare (ignore editor))
    (echo-warning "Editor buffer cannot edit files without configured editor mode."))
  (:documentation "Get the content of the editor."))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(export-always 'set-content)
(defgeneric set-content (editor-submode content)
  (:method ((editor editor-mode) (content t))
    (declare (ignore editor))
    (echo-warning "Editor buffer cannot edit files without configured editor mode.
See `describe-class editor-mode' for details."))
  (:documentation "Set the content of the editor."))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(export-always 'markup)
(defgeneric markup (editor-submode)
  (:method ((editor editor-mode))
    (spinneret:with-html-string
      (:head
       (:style (style (buffer editor))))
      (:body
       (:p "Please configure an editor mode to use an editor buffer. See "
           (:code "describe-class") " for " (:code "editor-buffer")
           " to see the list of functions to implement."))))
  (:documentation "Produce:
- A string/byte-array of the initial buffer contents.
- (optional, \"text/html\" is not provided) content type text."))

(define-class editor-buffer (network-buffer ; Questionable, but needed for `buffer-load'.
                             context-buffer modable-buffer document-buffer input-buffer)
  ((nyxt:title "*Editor*"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "Each editor buffer matches a file. Each editor buffer
contains an `nyxt/editor-mode:editor-mode' instance (or a subclass thereof)."))

(defmethod nyxt:default-modes :around ((buffer editor-buffer))
  ;; REVIEW: Really remove document-mode from editor-buffer?
  ;; FIXME: How to disable the annoying base-mode bindings in the editor!?
  (set-difference (call-next-method) '(document-mode base-mode)))

(defmethod file ((buffer editor-buffer))
  (uiop:parse-native-namestring (quri:uri-path (url buffer))))

(define-internal-scheme "editor"
    (lambda (url buffer)
      (let ((mode (find-submode 'editor-mode buffer))
            (file (quri:uri-path (quri:uri url))))
        (uiop:chdir (uiop:pathname-directory-pathname file))
        (run-thread "editor content setting"
          (sleep 2)
          (set-content mode (uiop:read-file-string file)))
        (markup mode))))

(defmethod editor ((editor-buffer editor-buffer))
  (let ((mode (find-submode 'editor-mode editor-buffer)))
    (unless (eq 'editor-mode (sera:class-name-of mode))
      mode)))

(defmethod write-file-with-editor ((buffer editor-buffer) &key (if-exists :error))
  (alexandria:if-let ((editor (editor buffer)))
    (alexandria:write-string-into-file (get-content editor)
                                       (file buffer)
                                       :if-exists if-exists)
    (echo "Editor buffer cannot write file without configured editor mode.")))

(defun prompt-for-editor-file ()
  (uiop:native-namestring
   (pathname
    (prompt1
     :prompt "Open file"
     :extra-modes 'nyxt/file-manager-mode:file-manager-mode
     :input (uiop:native-namestring (uiop:getcwd))
     :sources
     (list (make-instance 'nyxt/file-manager-mode:file-source
                          :name "Existing file"
                          :return-actions #'identity)
           (make-instance 'prompter:raw-source
                          :name "Create new file"))))))

(define-command editor-open-file (&key (buffer (current-buffer)) (file (prompt-for-editor-file)))
  "Open a file in the internal editor."
  (buffer-load (quri:make-uri :scheme "editor" :path file) :buffer buffer))

(define-command editor-write-file (&key (buffer (current-buffer)) (if-exists :error))
  "Write the FILE of the BUFFER to storage."
  (if (uiop:file-exists-p (file buffer))
      (if-confirm ((format nil "Overwrite ~s?" (file buffer))
                   :yes "overwrite" :no "cancel")
                  (progn (write-file-with-editor buffer :if-exists :overwrite)
                         (echo "File ~s saved." (file buffer)))
                  (echo "File ~s not saved." (file buffer)))
      (progn
        (write-file-with-editor buffer :if-exists if-exists)
        (echo "File ~s saved." (file buffer)))))

(define-command-global edit-file (&optional (file (prompt-for-editor-file)))
  "Open a new editor and query a FILE to edit in it."
  (let ((buffer (make-instance 'editor-buffer
                               :url (quri:make-uri :scheme "editor" :path file))))
    (set-current-buffer buffer)))
