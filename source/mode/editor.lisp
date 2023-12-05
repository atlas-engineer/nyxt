;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/editor
    (:documentation "Package for `editor-mode', mode for implementation of Nyxt-resident editors.

The new editor mode should subclass `editor-mode'. It is also required to
implement the methods `get-content', `set-content', and `markup'. This will
allow the subclassed mode to get/set content from/to the file (which is
necessary for its operation)."))
(in-package :nyxt/mode/editor)

(define-mode editor-mode ()
  "Mode for editor modes to extend.

Commands portable across all the `editor-mode' editors:
- `edit-file': Open a file in the new `editor-buffer'.
- `editor-open-file': Open a new file in the existing editor.
- `editor-write-file': Save the file to disk.

To install the mode implementing the following, add this snippet to your config
(define-configuration nyxt/mode/editor::editor-buffer
  ((default-modes (cons 'your-editor-mode %slot-value%))))

See `nyxt/mode/editor' package documentation for implementation details and
internal programming APIs, including the methods for custom editors'
implementation. Also see `plaintext-editor-mode' for an example
implementation."
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

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(define-generic get-content ((editor editor-mode))
  "Get the content of the EDITOR-SUBMODE as a string."
  (declare (ignore editor))
  (echo-warning "Editor buffer cannot edit files without configured editor mode.")
  (:export-generic-name-p t))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(define-generic set-content ((editor editor-mode) (content t))
  "Set the content of the EDITOR-SUBMODE to a new string/other CONTENT."
  (declare (ignore editor))
  (echo-warning "Editor buffer cannot edit files without configured editor mode.
See `describe-class editor-mode' for details.")
  (:export-generic-name-p t))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(define-generic markup ((editor editor-mode))
  "Produce at least a string/byte-array of the initial buffer contents.

See the `scheme' documentation for the format and the number of values that
`markup' specializations could/should return."
  (spinneret:with-html-string
    (:head
     (:nstyle (style (buffer editor))))
    (:body
     (:p "Please configure an editor mode to use an editor buffer. See "
         (:code "describe-class") " for " (:code "editor-buffer")
         " to see the list of functions to implement.")))
  (:export-generic-name-p t))

(define-class editor-buffer (network-buffer ; Questionable, but needed for `buffer-load'.
                             context-buffer modable-buffer document-buffer input-buffer)
  ((nyxt:title "*Editor*"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Each editor buffer matches a file. Each editor buffer
contains an `nyxt/mode/editor:editor-mode' instance (or a subclass thereof)."))

(defmethod nyxt:default-modes append ((buffer editor-buffer))
  "Add `editor-mode' and `plaintext-editor-mode' to `editor-buffer' by default."
  (list 'editor-mode 'plaintext-editor-mode))

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

(define-command editor-open-file (&key (buffer (current-buffer)) (file (prompt-for-editor-file)))
  "Open a file.

BUFFER is of type `editor-buffer'."
  (buffer-load (quri:make-uri :scheme "editor" :path file) :buffer buffer))

(define-command editor-write-file (&key (buffer (current-buffer)))
  "Write a file to storage.

BUFFER is of type `editor-buffer'."
  (if (uiop:file-exists-p (file buffer))
      (if-confirm ((format nil "Overwrite ~s?" (file buffer))
                   :yes "overwrite" :no "cancel")
          (echo "File ~s ~:[not ~;~]saved."
                (file buffer) (write-file-with-editor buffer :if-exists :overwrite)))
      (echo "File ~s ~:[not ~;~]saved." (file buffer) (write-file-with-editor buffer))))

(define-command-global edit-file (&optional (file (prompt-for-editor-file)))
  "Open a new editor and query a FILE to edit in it."
  (let ((buffer (make-instance 'editor-buffer
                               :url (quri:make-uri :scheme "editor" :path file))))
    (set-current-buffer buffer)))

(defun prompt-for-editor-user-file ()
  (uiop:native-namestring
   (files:expand
    (prompt1 :prompt "Edit user file"
             :sources 'nyxt::user-file-source))))

(define-command-global edit-user-file (&optional (file (prompt-for-editor-user-file)))
  (let ((buffer (make-instance 'editor-buffer
                               :url (quri:make-uri :scheme "editor" :path file))))
    (set-current-buffer buffer)))
