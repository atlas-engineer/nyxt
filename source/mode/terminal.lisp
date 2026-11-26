;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/terminal
  (:documentation "Package for `terminal-mode'.
The mode runs a shell on a pseudoterminal in this process and draws it with
xterm.js. Keys go to the terminal. C-M-z runs `toggle-terminal-keys'."))
(in-package :nyxt/mode/terminal)

(load-assets "terminal" #'alex:read-file-into-string)

(defun js-string (value)
  "Return VALUE as a JavaScript string literal."
  (with-output-to-string (stream)
    (prin1 (if (stringp value) value (princ-to-string value)) stream)))

(defun terminal-markup ()
  "Return the terminal page body."
  (spinneret:with-html-string
    (:nstyle
      (or (gethash "xterm.css" *static-data*) "")
      `("html, body"
        :height "100%"
        :margin 0
        :padding 0
        :overflow "hidden"
        :background ,theme:background-color
        :color ,theme:on-background-color)
      '("#terminal, .xterm"
        :height "100%"
        :width "100%")
      '(".xterm .xterm-viewport"
        :background-color "transparent"
        :scrollbar-width "none")
      '(".xterm .xterm-viewport::-webkit-scrollbar"
        :width 0
        :height 0))
    (:div :id "terminal")))

(defun terminal-script (buffer)
  "Return the script that builds the terminal for BUFFER."
  (let ((theme (theme *browser*)))
    (ps:ps
      ((lambda ()
         (let* ((tries 0)
                (opened ps:false)
                (buffer-id (ps:lisp (id buffer)))
                (font-family (ps:lisp (theme:monospace-font-family theme)))
                (background (ps:lisp (theme:background-color theme)))
                (foreground (ps:lisp (theme:on-background-color theme)))
                (cursor (ps:lisp (theme:action-color theme)))
                (selection (ps:lisp (theme:primary-color theme)))
                (face (ps:+ "15px \""
                            (ps:chain font-family (split "\"") (join ""))
                            "\""))
                (font-ready (when (and (ps:@ document fonts)
                                       (ps:@ document fonts load))
                              (ps:chain document fonts (load face))))
                (term nil)
                (fit nil))
           (defun send (action query)
             (ps:chain window
                       (fetch (ps:+ "nyxt-terminal://session/" action "?" query)
                              (ps:create mode "no-cors"))))
           (defun encode-data (text)
             (let* ((bytes (ps:chain (ps:new (-text-encoder)) (encode text)))
                    (binary ""))
               (loop for i from 0 below (ps:@ bytes length)
                     do (setf binary
                              (ps:+ binary
                                    (ps:chain -string
                                              (from-char-code (aref bytes i))))))
               (ps:chain (ps:chain (ps:chain ((ps:@ window btoa) binary)
                                             (split "+")
                                             (join "-"))
                                   (split "/")
                                   (join "_"))
                         (split "=")
                         (join ""))))
           (defun fill ()
             (when (and term fit)
               (let ((viewport (and (ps:@ term _core)
                                    (ps:@ term _core viewport))))
                 (when viewport
                   (setf (ps:@ viewport scroll-bar-width) 0)))
               (ps:chain fit (fit))))
           (defun remeasure ()
             (when term
               (let ((size (ps:@ term options font-size)))
                 (setf (ps:@ term options font-size) (ps:+ size 1))
                 (setf (ps:@ term options font-size) size)
                 (fill))))
           (defun open-terminal (node)
             (unless opened
               (setf opened t
                     term (ps:new ((ps:@ window "Terminal")
                                   (ps:create cursor-blink ps:false
                                              font-size 15
                                              scrollback 5000
                                              font-family font-family
                                              theme (ps:create background background
                                                               foreground foreground
                                                               cursor cursor
                                                               selection-background selection))))
                     fit (ps:new ((ps:@ (ps:@ window "FitAddon") "FitAddon"))))
               (ps:chain term (load-addon fit))
               (ps:chain term (open node))
               (ps:chain term
                         (on-data
                          (lambda (data)
                            (let ((start 0)
                                  (step 1024))
                              (loop while (< start (ps:@ data length))
                                    do (send "write"
                                             (ps:+ "buffer="
                                                   buffer-id
                                                   "&data="
                                                   (encode-u-r-i-component
                                                    (encode-data
                                                     (ps:chain data
                                                               (slice start
                                                                      (ps:+ start step)))))))
                                       (setf start (ps:+ start step)))))))
               (ps:chain term
                         (on-resize
                          (lambda (size)
                            (send "resize"
                                  (ps:+ "buffer="
                                        buffer-id
                                        "&columns="
                                        (ps:@ size cols)
                                        "&rows="
                                        (ps:@ size rows))))))
               (ps:chain term
                         (on-title-change
                          (lambda (title)
                            (setf (ps:@ document title) (or title "*Terminal*"))
                            (send "title"
                                  (ps:+ "buffer="
                                        buffer-id
                                        "&title="
                                        (encode-u-r-i-component (or title "")))))))
               (fill)
               (ps:chain window
                         (add-event-listener "resize"
                                             (lambda () (fill))))
               (setf (ps:@ window nyxt-terminal)
                     (ps:create
                      write (lambda (b64)
                              (let* ((binary ((ps:@ window atob) b64))
                                     (bytes (ps:new (-uint8-array
                                                     (ps:@ binary length)))))
                                (loop for i from 0 below (ps:@ binary length)
                                      do (setf (aref bytes i)
                                               (ps:chain binary (char-code-at i))))
                                (ps:chain term (write bytes))))
                      focus (lambda () (ps:chain term (focus)))))
               (ps:chain term (focus))
               (send "ready" (ps:+ "buffer=" buffer-id))))
           (defun boot ()
             (let ((node (ps:chain document (get-element-by-id "terminal"))))
               (cond
                 ((and node
                       (ps:@ window "Terminal")
                       (ps:@ window "FitAddon"))
                  (if font-ready
                      (progn
                        (ps:chain font-ready
                                  (then (lambda ()
                                          (if opened
                                              (remeasure)
                                              (open-terminal node)))
                                        (lambda ()
                                          (open-terminal node))))
                        (ps:chain window
                                  (set-timeout (lambda ()
                                                 (open-terminal node))
                                               400)))
                      (open-terminal node)))
                 ((< tries 50)
                  (incf tries)
                  (ps:chain window (set-timeout boot 30))))))
           (boot)))))))

(defun install-terminal-ui (mode)
  "Load xterm.js into MODE's buffer and connect it to the shell."
  (let ((buffer (buffer mode)))
    (ffi-buffer-evaluate-javascript-async buffer (gethash "xterm.js" *static-data*))
    (ffi-buffer-evaluate-javascript-async buffer (gethash "addon-fit.js" *static-data*))
    (ffi-buffer-evaluate-javascript-async buffer (terminal-script buffer))))

(defun send-terminal-output (mode octets)
  "Write OCTETS into MODE's xterm.js instance."
  (let ((buffer (buffer mode)))
    (when (and buffer (find buffer (buffer-list) :test #'eq) (plusp (length octets)))
      (handler-case
          (ffi-buffer-evaluate-javascript-async
           buffer
           (uiop:strcat "window.nyxtTerminal&&window.nyxtTerminal.write("
                        (js-string (cl-base64:usb8-array-to-base64-string octets :columns 0))
                        ");"))
        (error (condition)
          (log:debug "Terminal output dropped: ~a" condition))))))

(defun base64-pad (text)
  (let ((remainder (mod (length text) 4)))
    (if (zerop remainder)
        text
        (uiop:strcat text (make-string (- 4 remainder) :initial-element #\=)))))

(-> base64url-octets (string) (values (array (unsigned-byte 8)) &optional))
(defun base64url-octets (text)
  "Decode URL-safe base64 TEXT into octets."
  (cl-base64:base64-string-to-usb8-array
   (base64-pad (substitute #\+ #\- (substitute #\/ #\_ text)))))

(defun query-integer (params name)
  "Return the integer query parameter NAME, or NIL."
  (let ((value (assoc-value params name :test #'equal)))
    (when (and value (plusp (length value)))
      (handler-case (parse-integer value) (error () nil)))))

(defun request-action (url)
  "Return the path or host action of URL."
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

(defun attach-terminal (mode)
  "Send the buffered shell output to MODE's page."
  (let ((octets (terminal:take-transcript (pty mode))))
    (send-terminal-output mode octets)))

(defun terminal-title (text)
  "Return TEXT as a buffer title.
A blank TEXT names the terminal itself."
  (let ((title (string-trim '(#\space #\tab #\newline #\return)
                            (or text ""))))
    (if (plusp (length title))
        title
        "*Terminal*")))

(defun handle-terminal-request (url)
  "Apply a write, resize, title, or ready request from the terminal page."
  (let* ((params (quri:uri-query-params (quri:uri url)))
         (buffer (buffer-by-id (assoc-value params "buffer" :test #'equal)))
         (mode (and buffer (find-mode buffer 'terminal-mode)))
         (pty (and mode (pty mode))))
    (when pty
      (let ((action (request-action url)))
        (cond
          ((string= action "write")
           (let ((data (assoc-value params "data" :test #'equal)))
             (when (and data (plusp (length data)))
               (terminal:enqueue-input pty (base64url-octets data)))))
          ((string= action "resize")
           (let ((columns (query-integer params "columns"))
                 (rows (query-integer params "rows")))
             (when (and columns rows (plusp columns) (plusp rows))
               (terminal:resize pty rows columns))))
          ((string= action "title")
           (note-terminal-title mode (assoc-value params "title" :test #'equal)))
          ((string= action "ready")
           (attach-terminal mode)))))))

(define-internal-scheme "nyxt-terminal"
    (lambda (url)
      (handler-case (handle-terminal-request url)
        (error (condition)
          (log:debug "Terminal request failed: ~a" condition)))
      (values "ok" "text/plain" 200
              '(("Access-Control-Allow-Origin" . "*")
                ("Access-Control-Allow-Methods" . "GET, OPTIONS"))
              "OK"))
  (lambda (condition)
    (log:debug "Terminal scheme failed: ~a" condition)))

(define-mode terminal-mode ()
  "A terminal in the buffer.
The shell runs on a pseudoterminal in this process. Output is drawn with xterm.js.
The buffer title is the title the program sets. While another program is in the
foreground, the title is that program's name.
Keys go to the terminal. C-M-z runs `toggle-terminal-keys', which gives the keys
to Nyxt instead. The same binding sends them back."
  ((keyscheme-map
    (define-keyscheme-map "terminal-mode" ()
      keyscheme:default
      (list "C-M-z" 'toggle-terminal-keys)
      keyscheme:vi-insert
      (list "C-M-z" 'toggle-terminal-keys)))
   (program
    nil
    :type (or null string)
    :documentation "Program run in the terminal.
When NIL, the SHELL environment variable is used, or /bin/sh. The program is
started with -i.")
   (keys-to-terminal-p
    t
    :type boolean
    :documentation "When non-nil, keys are delivered to the terminal.
When NIL, Nyxt keybindings handle them. `toggle-terminal-keys' switches this.
The binding on this mode stays available in both states.")
   (pty
    nil
    :export nil
    :documentation "The pseudoterminal for this buffer.")
   (foreground-name
    nil
    :export nil
    :documentation "Name of the program last seen in the foreground.")
   (title-from-program-p
    nil
    :type boolean
    :export nil
    :documentation "Whether the buffer title was set by the program itself.")
   (delete-handler
    nil
    :export nil
    :documentation "Hook that closes the shell when the buffer is deleted.")
   (exit-handler
    nil
    :export nil
    :documentation "Hook that closes the shell when Nyxt quits."))
  (:toggler-command-p nil))

(defun program-argv (mode)
  "Return the program and arguments for MODE."
  (let ((program (or (and (stringp (program mode))
                          (plusp (length (program mode)))
                          (program mode))
                     (uiop:getenv "SHELL")
                     "/bin/sh")))
    (list program "-i")))

(defun shell-command-name (mode)
  "Return the file name of the shell MODE starts."
  (file-namestring (first (program-argv mode))))

(defun note-terminal-title (mode text)
  "Set MODE's buffer title from the title TEXT the program chose."
  (when (buffer mode)
    (setf (title-from-program-p mode) t
          (title (buffer mode)) (terminal-title text))))

(defun note-foreground-program (mode)
  "Name MODE's buffer after a foreground program other than its shell."
  (handler-case
      (when-let ((pty (pty mode)))
        (let ((name (terminal:foreground-name pty)))
          (when (and name (not (equal name (foreground-name mode))))
            (setf (foreground-name mode) name
                  (title-from-program-p mode) nil))
          (when (and name
                     (not (title-from-program-p mode))
                     (not (string= name (shell-command-name mode)))
                     (buffer mode)
                     (not (string= name (title (buffer mode)))))
            (setf (title (buffer mode)) name))))
    (error (condition)
      (log:debug "Terminal title was not updated: ~a" condition))))

(defun detach-terminal-handlers (mode)
  "Remove MODE's buffer-delete and browser-exit hook handlers."
  (when (delete-handler mode)
    (hooks:remove-hook (buffer-delete-hook (buffer mode)) (delete-handler mode))
    (setf (delete-handler mode) nil))
  (when (exit-handler mode)
    (hooks:remove-hook (before-exit-hook *browser*) (exit-handler mode))
    (setf (exit-handler mode) nil)))

(defun ensure-delete-handler (mode)
  "Close MODE's shell when its buffer is deleted."
  (unless (delete-handler mode)
    (setf (delete-handler mode)
          (make-instance 'hooks:handler
                         :name (gensym "TERMINAL-DELETE")
                         :fn (lambda (buffer)
                               (declare (ignore buffer))
                               (detach-terminal-handlers mode)
                               (stop-terminal mode))))
    (hooks:add-hook (buffer-delete-hook (buffer mode))
                    (delete-handler mode))))

(defun ensure-exit-handler (mode)
  "Close MODE's shell when Nyxt quits.
The shell is its own session, so it outlives the process unless it is reaped."
  (when (and *browser* (not (exit-handler mode)))
    (setf (exit-handler mode)
          (make-instance 'hooks:handler
                         :name (gensym "TERMINAL-EXIT")
                         :fn (lambda ()
                               (stop-terminal mode))))
    (hooks:add-hook (before-exit-hook *browser*)
                    (exit-handler mode))))

(defun stop-terminal (mode)
  "Reap MODE's shell and stop sending it keys."
  (when (pty mode)
    (terminal:stop (pty mode))
    (setf (pty mode) nil))
  (when (buffer mode)
    (setf (forward-input-events-p (buffer mode)) nil)))

(defmethod enable ((mode terminal-mode) &key)
  (setf (forward-input-events-p (buffer mode)) (keys-to-terminal-p mode))
  (unless (pty mode)
    (handler-case
        (progn
          (setf (pty mode)
                (terminal:start (program-argv mode)
                                :on-output (lambda (octets)
                                             (send-terminal-output mode octets)
                                             (note-foreground-program mode))
                                :on-exit (lambda ()
                                           (echo "Terminal exited."))))
          (ensure-delete-handler mode)
          (ensure-exit-handler mode))
      (error (condition)
        (echo-warning "Terminal failed to start: ~a" condition)))))

(defmethod disable ((mode terminal-mode) &key)
  (detach-terminal-handlers mode)
  (stop-terminal mode))

(defmethod on-signal-load-finished ((mode terminal-mode) url title)
  (declare (ignore title))
  (when (pty mode)
    (terminal:detach (pty mode))
    (install-terminal-ui mode))
  url)

(define-internal-page terminal-page ()
    (:title "*Terminal*"
     :page-mode 'terminal-mode)
  "A terminal."
  (terminal-markup))

(define-command-global terminal ()
  "Open a terminal."
  (let ((buffer (make-instance 'web-buffer)))
    (set-current-buffer buffer)
    (ffi-buffer-load buffer (quri:uri (nyxt-url 'terminal-page)))
    buffer))

(define-command-global toggle-terminal-keys (buffer)
  "Toggle whether keys go to the terminal or to Nyxt."
  (let ((mode (find-mode buffer 'terminal-mode)))
    (if mode
        (let ((to-terminal (not (keys-to-terminal-p mode))))
          (setf (keys-to-terminal-p mode) to-terminal)
          (setf (forward-input-events-p buffer) to-terminal)
          (when to-terminal
            (ffi-buffer-evaluate-javascript-async
             buffer
             "window.nyxtTerminal&&window.nyxtTerminal.focus();"))
          (echo (if to-terminal
                    "Keys go to the terminal."
                    "Keys go to Nyxt.")))
        (echo "This buffer has no terminal."))))
