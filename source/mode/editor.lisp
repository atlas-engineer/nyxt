;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/editor
  (:documentation "Package for `editor-mode'.
The mode edits text in the buffer with Monaco. Keys go to the editor.
C-M-z runs `toggle-editor-keys'."))
(in-package :nyxt/mode/editor)

(alex:define-constant +file-limit+ 4194304 :test #'eql
  :documentation "Largest file, in octets, that the editor reads or writes.")
(alex:define-constant +chunk-bytes+ 12000 :test #'eql
  :documentation "Octets of text in one save request from the page.")
(alex:define-constant +asset-root+ "nyxt-editor://asset/vs" :test #'equal
  :documentation "URL prefix of the vendored Monaco files.")

(defun js-string (value)
  "Return VALUE as a JavaScript string literal."
  (with-output-to-string (stream)
    (prin1 (if (stringp value) value (princ-to-string value)) stream)))

(-> css-hex ((or string null)) (values string &optional))
(defun css-hex (color)
  "Return COLOR as #rrggbb.
Monaco rejects named colors such as \"black\"."
  (let ((rgb (cl-colors-ng:as-rgb (if (stringp color) color "black") :errorp nil)))
    (if rgb
        (format nil "#~2,'0X~2,'0X~2,'0X"
                (round (* 255 (cl-colors-ng:rgb-red rgb)))
                (round (* 255 (cl-colors-ng:rgb-green rgb)))
                (round (* 255 (cl-colors-ng:rgb-blue rgb))))
        "#000000")))

(defun selection-color (color)
  "Return a translucent #rrggbbaa form of COLOR."
  (uiop:strcat (css-hex color) "66"))

(defun editor-markup ()
  "Return the editor page body."
  (let ((color (theme:background-color (theme *browser*))))
    (spinneret:with-html-string
      (:style
       (:raw (format nil
                     "html,body{height:100%;margin:0;overflow:hidden;background:~a}~
                      #editor{position:fixed;inset:0;background:~a}"
                     color color)))
      (:div :id "editor"))))

(defun substitute-template (template replacements)
  "Replace each (OLD . NEW) pair in TEMPLATE."
  (reduce (lambda (text pair)
            (str:replace-all (car pair) (cdr pair) text))
          replacements
          :initial-value template))

(defun editor-script (buffer)
  "Return the script that builds the editor for BUFFER."
  (let* ((theme (theme *browser*))
         (template (alex:read-file-into-string
                    (asdf:system-relative-pathname :nyxt "assets/editor/boot.js"))))
    (substitute-template
     template
     `(("__BUFFER_ID__" . ,(princ-to-string (id buffer)))
       ("__CHUNK_BYTES__" . ,(princ-to-string +chunk-bytes+))
       ("__ASSET_ROOT__" . ,(js-string +asset-root+))
       ("__BASE_THEME__" . ,(js-string (if (theme:dark-p theme) "vs-dark" "vs")))
       ("__FONT__" . ,(js-string (theme:monospace-font-family theme)))
       ("__BACKGROUND__" . ,(js-string (css-hex (theme:background-color theme))))
       ("__FOREGROUND__" . ,(js-string (css-hex (theme:on-background-color theme))))
       ("__CURSOR__" . ,(js-string (css-hex (theme:action-color theme))))
       ("__SELECTION__" . ,(js-string (selection-color (theme:primary-color theme))))
       ("__LINE__" . ,(js-string (css-hex (theme:background-color+ theme))))))))

(defun install-editor-ui (mode)
  "Load Monaco into MODE's buffer."
  (let ((buffer (buffer mode)))
    (ffi-buffer-evaluate-javascript-async buffer (editor-script buffer))))

(defun base64-pad (text)
  (let ((remainder (mod (length text) 4)))
    (if (zerop remainder)
        text
        (uiop:strcat text (make-string (- 4 remainder) :initial-element #\=)))))

(-> base64url-octets (string) (values (array (unsigned-byte 8)) &optional))
(defun base64url-octets (text)
  "Decode URL-safe base64 TEXT into octets."
  (if (zerop (length text))
      (make-array 0 :element-type '(unsigned-byte 8))
      (cl-base64:base64-string-to-usb8-array
       (base64-pad (substitute #\+ #\- (substitute #\/ #\_ text))))))

(defun base64-text (text)
  "Encode TEXT as a single-line base64 string."
  (cl-base64:usb8-array-to-base64-string
   (babel:string-to-octets text :encoding :utf-8)
   :columns 0))

(defun query-integer (params name)
  "Return the integer query parameter NAME, or NIL."
  (let ((value (assoc-value params name :test #'equal)))
    (when (and value (plusp (length value)))
      (handler-case (parse-integer value) (error () nil)))))

(defun request-part (url)
  "Return the path or host that names a request in URL."
  (let* ((uri (quri:uri url))
         (path (string-trim '(#\/) (or (quri:uri-path uri) ""))))
    (if (plusp (length path))
        path
        (or (quri:uri-host uri) ""))))

(defun buffer-by-id (id-string)
  "Return the buffer whose id is ID-STRING."
  (let ((id (and id-string (handler-case (parse-integer id-string) (error () nil)))))
    (when id
      (find id (buffer-list) :key #'id))))

(defun editor-asset-root ()
  "Return the directory of vendored editor files."
  (uiop:ensure-directory-pathname
   (asdf:system-relative-pathname :nyxt "assets/editor/")))

(-> safe-relative-path-p (string) (values boolean &optional))
(defun safe-relative-path-p (relative)
  "Whether RELATIVE stays inside the editor asset directory."
  (let ((segments (remove "" (str:split "/" relative) :test #'string=)))
    (and segments
         (every (lambda (segment)
                  (and (plusp (length segment))
                       (not (string= segment "."))
                       (not (string= segment ".."))))
                segments))))

(-> editor-asset (string) (values (or pathname null) &optional))
(defun editor-asset (relative)
  "Return the file for RELATIVE, or NIL when it leaves the asset directory."
  (when (safe-relative-path-p relative)
    (let* ((root (editor-asset-root))
           (path (uiop:merge-pathnames* relative root)))
      (when (and (uiop:file-exists-p path)
                 (uiop:subpathp (truename path)
                                (uiop:ensure-directory-pathname (truename root))))
        path))))

(defun asset-mime (path)
  "Return the MIME type for PATH."
  (let ((type (string-downcase (or (pathname-type path) ""))))
    (cond
      ((or (string= type "js") (string= type "mjs")) "text/javascript")
      ((string= type "css") "text/css")
      ((string= type "json") "application/json")
      ((string= type "ttf") "font/ttf")
      ((string= type "woff") "font/woff")
      ((string= type "woff2") "font/woff2")
      ((string= type "svg") "image/svg+xml")
      ((string= type "png") "image/png")
      (t "application/octet-stream"))))

(defun asset-response (url)
  "Return the Monaco file named by URL."
  (let* ((raw (or (quri:uri-path (quri:uri url)) ""))
         (relative (string-left-trim "/" (quri:url-decode raw)))
         (path (editor-asset relative)))
    (if path
        (values (alex:read-file-into-byte-vector path)
                (asset-mime path)
                200
                '(("Access-Control-Allow-Origin" . "*")
                  ("Access-Control-Allow-Methods" . "GET, OPTIONS")
                  ("Cross-Origin-Resource-Policy" . "cross-origin"))
                "OK")
        (progn
          (log:debug "Editor asset not found: ~a" relative)
          (values "" "text/plain" 404
                  '(("Access-Control-Allow-Origin" . "*"))
                  "Not Found")))))

(defparameter *language-types*
  '(("lisp" . "lisp")
    ("lsp" . "lisp")
    ("cl" . "lisp")
    ("asd" . "lisp")
    ("el" . "lisp")
    ("js" . "javascript")
    ("mjs" . "javascript")
    ("cjs" . "javascript")
    ("jsx" . "javascript")
    ("ts" . "typescript")
    ("tsx" . "typescript")
    ("json" . "json")
    ("html" . "html")
    ("htm" . "html")
    ("css" . "css")
    ("scss" . "scss")
    ("less" . "less")
    ("md" . "markdown")
    ("markdown" . "markdown")
    ("py" . "python")
    ("rb" . "ruby")
    ("rs" . "rust")
    ("go" . "go")
    ("java" . "java")
    ("c" . "c")
    ("h" . "c")
    ("cpp" . "cpp")
    ("cc" . "cpp")
    ("cxx" . "cpp")
    ("hpp" . "cpp")
    ("hh" . "cpp")
    ("cs" . "csharp")
    ("sh" . "shell")
    ("bash" . "shell")
    ("zsh" . "shell")
    ("xml" . "xml")
    ("svg" . "xml")
    ("yml" . "yaml")
    ("yaml" . "yaml")
    ("sql" . "sql")
    ("php" . "php")
    ("lua" . "lua")
    ("r" . "r")
    ("pl" . "perl")
    ("clj" . "clojure")
    ("cljs" . "clojure")
    ("scm" . "scheme"))
  "Alist from a downcased pathname type to a Monaco language id.")

(-> language-for-path ((or pathname null)) (values string &optional))
(defun language-for-path (path)
  "Return the Monaco language id for PATH."
  (or (and path
           (pathname-type path)
           (assoc-value *language-types*
                        (string-downcase (pathname-type path))
                        :test #'equal))
      "plaintext"))

(-> editor-pathname ((or pathname string null)) (values (or pathname null) &optional))
(defun editor-pathname (designator)
  "Return an absolute file pathname for DESIGNATOR.
A directory, or an empty designator, returns NIL."
  (when (and designator
             (or (pathnamep designator)
                 (and (stringp designator) (plusp (length designator)))))
    (let ((path (uiop:ensure-pathname designator
                                      :want-pathname t
                                      :ensure-absolute t
                                      :defaults (uiop:getcwd))))
      (unless (uiop:directory-pathname-p path)
        path))))

(-> read-editor-file (pathname) (values string &optional))
(defun read-editor-file (pathname)
  "Return the UTF-8 text of PATHNAME.
Signal an error when the file is too large or contains a null octet."
  (let ((octets (alex:read-file-into-byte-vector pathname)))
    (cond
      ((> (length octets) +file-limit+)
       (error "File is larger than ~a bytes." +file-limit+))
      ((find 0 octets)
       (error "File is not text."))
      (t (babel:octets-to-string octets :encoding :utf-8 :errorp nil)))))

(-> write-editor-file (pathname string) (values pathname &optional))
(defun write-editor-file (pathname text)
  "Write TEXT to PATHNAME as UTF-8 and return PATHNAME."
  (when (> (length text) +file-limit+)
    (error "File is larger than ~a bytes." +file-limit+))
  (ensure-directories-exist pathname)
  (alex:write-byte-vector-into-file
   (babel:string-to-octets text :encoding :utf-8)
   pathname
   :if-exists :supersede
   :if-does-not-exist :create)
  pathname)

(define-mode editor-mode ()
  "A text editor in the buffer.
The text is drawn with Monaco. Keys go to the editor. C-M-z runs
`toggle-editor-keys', which gives the keys to Nyxt instead. The same binding
sends them back. C-M-s runs `save-editor'. C-M-o runs `edit-file'."
  ((keyscheme-map
    (define-keyscheme-map "editor-mode" ()
      keyscheme:default
      (list "C-M-z" 'toggle-editor-keys
            "C-M-s" 'save-editor
            "C-M-o" 'edit-file)
      keyscheme:vi-insert
      (list "C-M-z" 'toggle-editor-keys
            "C-M-s" 'save-editor
            "C-M-o" 'edit-file)))
   (path
    nil
    :type (or null pathname)
    :documentation "File this buffer edits. NIL is an unsaved buffer.")
   (text
    ""
    :export nil
    :documentation "Text last read from or written to `path'.")
   (modified-p
    nil
    :type boolean
    :documentation "Whether the buffer has edits that are not saved.")
   (keys-to-editor-p
    t
    :type boolean
    :documentation "When non-nil, keys are delivered to the editor.
When NIL, Nyxt keybindings handle them. `toggle-editor-keys' switches this.
The bindings on this mode stay available in both states.")
   (generation
    0
    :type integer
    :export nil
    :documentation "Edit counter from the page. A save clears `modified-p' only
when the page has not changed since that save started.")
   (chunks
    nil
    :export nil
    :documentation "Octet vectors of a save in progress, indexed by position.")
   (chunk-count
    0
    :type integer
    :export nil
    :documentation "How many entries of `chunks' have arrived.")
   (chunk-generation
    0
    :type integer
    :export nil
    :documentation "Edit counter of the save in `chunks'."))
  (:toggler-command-p nil))

(defun editor-title (mode)
  "Return the buffer title for MODE."
  (let ((name (if (path mode)
                  (file-namestring (path mode))
                  "*Editor*")))
    (if (modified-p mode)
        (uiop:strcat name " *")
        name)))

(defun update-editor-title (mode)
  "Set MODE's buffer title from the file and whether it is modified."
  (let ((buffer (buffer mode))
        (name (editor-title mode)))
    (when buffer
      (setf (title buffer) name)
      (when (find buffer (buffer-list) :test #'eq)
        (ffi-buffer-evaluate-javascript-async
         buffer
         (format nil "document.title=~a;" (js-string name)))))))

(defun load-editor-file (mode pathname)
  "Point MODE at PATHNAME and read it when the file exists."
  (setf (path mode) pathname
        (text mode) ""
        (modified-p mode) nil
        (generation mode) 0)
  (handler-case
      (when (uiop:file-exists-p pathname)
        (setf (text mode) (read-editor-file pathname)))
    (error (condition)
      (echo-warning "Could not read ~a: ~a" pathname condition)))
  (update-editor-title mode))

(defun show-editor-text (mode)
  "Replace the page's text with MODE's text."
  (let ((buffer (buffer mode)))
    (when (and buffer (find buffer (buffer-list) :test #'eq))
      (ffi-buffer-evaluate-javascript-async
       buffer
       (format nil "window.nyxtEditor&&window.nyxtEditor.open(~a,~a,~a);"
               (js-string (base64-text (or (text mode) "")))
               (js-string (language-for-path (path mode)))
               (js-string (if (path mode)
                              (uiop:native-namestring (path mode))
                              "")))))))

(defun mark-clean (mode generation)
  "Tell MODE's page that GENERATION was saved."
  (let ((buffer (buffer mode)))
    (when (and buffer (find buffer (buffer-list) :test #'eq))
      (ffi-buffer-evaluate-javascript-async
       buffer
       (format nil "window.nyxtEditor&&window.nyxtEditor.markClean(~d);"
               generation)))))

(defun prompt-save-path (mode)
  "Prompt for a file and return it, or NIL when the prompt is a directory."
  (let* ((directory (if (path mode)
                        (uiop:pathname-directory-pathname (path mode))
                        (uiop:getcwd)))
         (name (if (path mode)
                   (file-namestring (path mode))
                   "untitled.txt"))
         (entered (prompt1 :prompt "Save file"
                           :input (uiop:native-namestring
                                   (merge-pathnames name directory))
                           :sources 'prompter:raw-source)))
    (editor-pathname entered)))

(defun save-editor-text (mode text generation)
  "Write TEXT for MODE and clear the modified mark when GENERATION is current."
  (handler-case
      (let ((pathname (or (path mode) (prompt-save-path mode))))
        (unless pathname
          (echo "Choose a file.")
          (return-from save-editor-text nil))
        (write-editor-file pathname text)
        (setf (path mode) pathname
              (text mode) text)
        (when (= generation (generation mode))
          (setf (modified-p mode) nil)
          (mark-clean mode generation))
        (update-editor-title mode)
        (echo "Saved ~a." (uiop:native-namestring pathname)))
    (prompt-buffer-canceled ()
      nil)
    (error (condition)
      (echo-warning "Could not save: ~a" condition))))

(defun decode-chunks (chunks)
  "Concatenate CHUNKS, a vector of octet vectors, into a string."
  (let* ((size (reduce #'+ chunks :key #'length :initial-value 0))
         (octets (make-array size :element-type '(unsigned-byte 8)))
         (offset 0))
    (loop for chunk across chunks
          do (replace octets chunk :start1 offset)
             (incf offset (length chunk)))
    (babel:octets-to-string octets :encoding :utf-8 :errorp nil)))

(defun accept-chunk (mode index count generation octets)
  "Store OCTETS at INDEX of a COUNT-wide save for GENERATION.
Return the assembled text when every chunk has arrived, otherwise NIL."
  (when (and (>= index 0)
             (> count 0)
             (< index count)
             (<= count (ceiling +file-limit+ +chunk-bytes+)))
    (unless (and (vectorp (chunks mode))
                 (= (length (chunks mode)) count)
                 (= (chunk-generation mode) generation))
      (setf (chunks mode) (make-array count :initial-element nil)
            (chunk-count mode) 0
            (chunk-generation mode) generation))
    (unless (aref (chunks mode) index)
      (setf (aref (chunks mode) index) octets)
      (incf (chunk-count mode) 1))
    (when (= (chunk-count mode) count)
      (prog1 (decode-chunks (chunks mode))
        (setf (chunks mode) nil
              (chunk-count mode) 0)))))

(defun mode-from-params (params)
  "Return the editor mode named by the buffer query parameter in PARAMS."
  (let ((buffer (buffer-by-id (assoc-value params "buffer" :test #'equal))))
    (when buffer
      (find-submode 'editor-mode buffer))))

(defun handle-session-request (url)
  "Apply a ready, edit, or save request from the editor page."
  (let* ((params (quri:uri-query-params (quri:uri url)))
         (mode (mode-from-params params))
         (action (request-part url)))
    (when mode
      (cond
        ((string= action "ready")
         (setf (generation mode) 0
               (modified-p mode) nil)
         (update-editor-title mode)
         (show-editor-text mode))
        ((string= action "changed")
         (let ((generation (query-integer params "generation")))
           (when (and (integerp generation) (>= generation (generation mode)))
             (setf (generation mode) generation
                   (modified-p mode) t)
             (update-editor-title mode))))
        ((string= action "chunk")
         (let ((index (query-integer params "index"))
               (count (query-integer params "count"))
               (generation (query-integer params "generation"))
               (data (or (assoc-value params "data" :test #'equal) "")))
           (when (and (integerp index) (integerp count) (integerp generation))
             (let ((text (accept-chunk mode index count generation
                                       (base64url-octets data))))
               (when text
                 (let ((saved-mode mode)
                       (saved-generation generation))
                   (run-thread "editor-save"
                     (save-editor-text saved-mode text saved-generation))))))))
        ((string= action "open")
         (run-thread "editor-open"
           (with-current-buffer (buffer mode)
             (edit-file))))
        ((string= action "failed")
         (let ((message (assoc-value params "message" :test #'equal)))
           (if (and message (plusp (length message)))
               (echo-warning "Editor failed to load: ~a" message)
               (echo-warning "Editor failed to load."))))))))

(defun handle-editor-request (url)
  "Serve a Monaco file or apply a session request."
  (let ((host (or (quri:uri-host (quri:uri url)) "")))
    (cond
      ((string-equal host "asset")
       (asset-response url))
      ((string-equal host "session")
       (handle-session-request url)
       (values "ok" "text/plain" 200
               '(("Access-Control-Allow-Origin" . "*")
                 ("Access-Control-Allow-Methods" . "GET, OPTIONS"))
               "OK"))
      (t (values "" "text/plain" 404 nil "Not Found")))))

(define-internal-scheme "nyxt-editor"
    (lambda (url)
      (handler-case (handle-editor-request url)
        (error (condition)
          (log:debug "Editor request failed: ~a" condition)
          (values "" "text/plain" 500 nil "Error"))))
  (lambda (condition)
    (log:debug "Editor scheme failed: ~a" condition)))

(defmethod enable ((mode editor-mode) &key)
  (setf (forward-input-events-p (buffer mode)) (keys-to-editor-p mode)))

(defmethod disable ((mode editor-mode) &key)
  (when (buffer mode)
    (setf (forward-input-events-p (buffer mode)) nil)))

(defmethod on-signal-load-finished ((mode editor-mode) url title)
  (declare (ignore title))
  (install-editor-ui mode)
  url)

(defmethod on-signal-notify-title ((mode editor-mode) title)
  (declare (ignore title))
  (unless (string= (title (buffer mode)) (editor-title mode))
    (update-editor-title mode)))

(define-internal-page editor-page (&key path)
    (:title "*Editor*"
     :page-mode 'editor-mode)
  "A text editor."
  (let ((mode (find-submode 'editor-mode %buffer%)))
    (when (and mode path (plusp (length path)))
      (let ((pathname (editor-pathname path)))
        (if pathname
            (load-editor-file mode pathname)
            (echo "Choose a file.")))))
  (editor-markup))

(define-command-global edit (&optional path)
  "Open an editor. PATH is a file to edit. Without PATH, the buffer is empty."
  (when (and path (null (editor-pathname path)))
    (echo "Choose a file.")
    (return-from edit nil))
  (let ((pathname (editor-pathname path))
        (buffer (make-instance 'web-buffer)))
    (set-current-buffer buffer)
    (ffi-buffer-load buffer
                     (quri:uri (if pathname
                                   (nyxt-url 'editor-page
                                             :path (uiop:native-namestring pathname))
                                   (nyxt-url 'editor-page))))
    buffer))

(define-command-global edit-file ()
  "Prompt for a file and open it in the editor."
  (let ((file (prompt1 :prompt "Edit file"
                       :input (uiop:native-namestring (uiop:getcwd))
                       :extra-modes 'nyxt/mode/file-manager:file-manager-mode
                       :sources (list (make-instance 'nyxt/mode/file-manager:file-source)
                                      (make-instance 'prompter:raw-source)))))
    (edit file)))

(define-command-global save-editor (&optional (buffer (current-buffer)))
  "Save the editor buffer."
  (let ((mode (find-submode 'editor-mode buffer)))
    (if mode
        (ffi-buffer-evaluate-javascript-async
         buffer
         "window.nyxtEditor&&window.nyxtEditor.save();")
        (echo "This buffer has no editor."))))

(define-command-global toggle-editor-keys (&optional (buffer (current-buffer)))
  "Toggle whether keys go to the editor or to Nyxt."
  (let ((mode (find-submode 'editor-mode buffer)))
    (if mode
        (let ((to-editor (not (keys-to-editor-p mode))))
          (setf (keys-to-editor-p mode) to-editor)
          (setf (forward-input-events-p buffer) to-editor)
          (when to-editor
            (ffi-buffer-evaluate-javascript-async
             buffer
             "window.nyxtEditor&&window.nyxtEditor.focus();"))
          (echo (if to-editor
                    "Keys go to the editor."
                    "Keys go to Nyxt.")))
        (echo "This buffer has no editor."))))
