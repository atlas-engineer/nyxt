;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/editor
  (:documentation "Package for `editor-mode', mode to implement Nyxt text editors.

Editors should subclass `editor-mode' and specialize methods `markup',
`get-content' and `set-content'."))
(in-package :nyxt/mode/editor)

(define-mode editor-mode ()
  "General-purpose editor mode, meant to be subclassed.

To enable a mode that inherits from `editor-mode', add this snippet to your
config:
(define-configuration nyxt/mode/editor:editor-buffer
  ((default-modes (cons 'custom-editor-mode %slot-value%))))

See `plaintext-editor-mode' for an example of inheritance."
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
       "C-w" 'delete-current-buffer
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

(export-always 'get-content)
(defgeneric get-content (editor-submode)
  (:method ((editor editor-mode))
    (declare (ignore editor))
    (echo-warning "Editor buffer cannot edit files without configured editor mode."))
  (:documentation "Get the content of the EDITOR-SUBMODE as a string."))

(export-always 'set-content)
(defgeneric set-content (editor-submode content)
  (:method ((editor editor-mode) (content t))
    (declare (ignore editor))
    (echo-warning "Editor buffer cannot edit files without configured editor mode.
See `describe-class editor-mode' for details."))
  (:documentation "Set the content of EDITOR-SUBMODE to the string CONTENT."))

(export-always 'markup)
(defgeneric markup (editor-submode content)
  (:method ((editor editor-mode) content)
    (declare (ignore content))
    (spinneret:with-html-string
      (:head
       (:nstyle (style (buffer editor))))
      (:body
       (:p "Please configure an editor mode to use an editor buffer. See "
           (:code "describe-class") " for " (:code "editor-buffer")
           " to see the list of functions to implement."))))
  (:documentation "Return an HTML string representation of the file to be edited."))

(define-class editor-buffer (network-buffer ; Questionable, but needed for `buffer-load'.
                             context-buffer modable-buffer document-buffer input-buffer)
  ((nyxt:title "*Editor*"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Buffer to edit files. See `nyxt/mode/editor:editor-mode'."))

(defmethod nyxt:default-modes append ((buffer editor-buffer))
  "Add `editor-mode' and `plaintext-editor-mode' to `editor-buffer' by default."
  (list 'editor-mode 'plaintext-editor-mode))

(defmethod nyxt:default-modes :around ((buffer editor-buffer))
  (set-difference (call-next-method) '(document-mode base-mode)))

(defmethod file ((buffer editor-buffer))
  (uiop:parse-native-namestring (quri:uri-path (url buffer))))

(define-internal-scheme "editor"
    (lambda (url)
      (markup (find-submode 'editor-mode)
              (uiop:read-file-string (quri:uri-path (quri:uri url))))))

(defmethod editor ((editor-buffer editor-buffer))
  (let ((mode (find-submode 'editor-mode editor-buffer)))
    (unless (eq 'editor-mode (sera:class-name-of mode))
      mode)))

(defmethod write-file-with-editor ((buffer editor-buffer) &key (if-exists :error))
  (cond
    ((editor buffer)
     (handler-case
         (alexandria:write-string-into-file (get-content (editor buffer))
                                            (file buffer)
                                            :if-exists if-exists)
       (file-error (e)
         (echo-warning "Cannot write ~a: ~a" (file buffer) e)
         nil)))
    (t
     (echo-warning "Editor buffer cannot write file without configured editor mode.")
     nil)))

(defun prompt-for-editor-file ()
  (uiop:native-namestring
   (pathname
    (prompt1
     :prompt "Open file"
     :extra-modes 'nyxt/mode/file-manager:file-manager-mode
     :input (uiop:native-namestring (uiop:getcwd))
     :sources
     (list (make-instance 'nyxt/mode/file-manager:file-source
                          :name "Existing file"
                          :actions-on-return #'identity)
           (make-instance 'prompter:raw-source
                          :name "Create new file"))))))

(define-command editor-open-file (&key (buffer (current-buffer)) (file-path (prompt-for-editor-file)))
  "Open a file.

BUFFER is of type `editor-buffer'."
  (buffer-load (quri:make-uri :scheme "editor" :path file-path) :buffer buffer))

(define-command editor-write-file (&key (buffer (current-buffer)))
  "Write a file to storage.

BUFFER is of type `editor-buffer'."
  (if (uiop:file-exists-p (file buffer))
      (if-confirm ((format nil "Overwrite ~s?" (file buffer))
                   :yes "overwrite" :no "cancel")
          (echo "File ~s ~:[not ~;~]saved."
                (file buffer) (write-file-with-editor buffer :if-exists :overwrite)))
      (echo "File ~s ~:[not ~;~]saved." (file buffer) (write-file-with-editor buffer))))

(define-command-global edit-file (&optional (file-path (prompt-for-editor-file)))
  "Open a new editor and query a FILE to edit in it."
  (set-current-buffer (make-instance 'editor-buffer
                                     :url (quri:make-uri :scheme "editor" :path file-path))))

(define-command-global edit-user-file
    (&optional (file-path (uiop:native-namestring
                           (files:expand
                            (prompt1 :prompt "Edit user file"
                                     :sources 'nyxt::user-file-source)))))
  (edit-file file-path))

(define-auto-rule '(match-scheme "editor")
  :included '(nyxt/mode/editor:editor-mode))
