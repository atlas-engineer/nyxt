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
  (let ((theme (theme *browser*)))
    (spinneret:with-html-string
      (:style
       (:raw (or (gethash "xterm.css" *static-data*) ""))
       (:raw (format nil
                     "html,body{height:100%;margin:0;padding:0;overflow:hidden;~
                      background:~a;color:~a}#terminal{height:100vh;width:100%;}"
                     (theme:background-color theme)
                     (theme:on-background-color theme))))
      (:div :id "terminal"))))

(defun terminal-script (buffer)
  "Return the script that builds the terminal for BUFFER."
  (let ((theme (theme *browser*)))
    (uiop:strcat
     "(function(){var tries=0;function boot(){"
     "var node=document.getElementById(\"terminal\");"
     "if(!node||!window.Terminal||!window.FitAddon){"
     "if(tries++<50) window.setTimeout(boot,30);return;}"
     "var term=new Terminal({cursorBlink:false,fontSize:15,scrollback:5000,fontFamily:"
     (js-string (theme:monospace-font-family theme))
     ",theme:{background:"
     (js-string (theme:background-color theme))
     ",foreground:"
     (js-string (theme:on-background-color theme))
     ",cursor:"
     (js-string (theme:action-color theme))
     ",selectionBackground:"
     (js-string (theme:primary-color theme))
     "}});"
     "var fit=new FitAddon.FitAddon();term.loadAddon(fit);term.open(node);"
     "var id=" (princ-to-string (id buffer)) ";"
     "function send(action,query){"
     "fetch(\"nyxt-terminal://session/\"+action+\"?\"+query,{mode:\"no-cors\"});}"
     "function encodeData(text){var bytes=new TextEncoder().encode(text);var binary=\"\";"
     "for(var i=0;i<bytes.length;i++) binary+=String.fromCharCode(bytes[i]);"
     "return btoa(binary).replace(/\\+/g,\"-\").replace(/\\//g,\"_\").replace(/=+$/g,\"\");}"
     "term.onData(function(data){var step=1024;"
     "for(var start=0;start<data.length;start+=step)"
     "send(\"write\",\"buffer=\"+id+\"&data=\"+"
     "encodeURIComponent(encodeData(data.slice(start,start+step))));});"
     "term.onResize(function(size){"
     "send(\"resize\",\"buffer=\"+id+\"&columns=\"+size.cols+\"&rows=\"+size.rows);});"
     "fit.fit();window.addEventListener(\"resize\",function(){fit.fit();});"
     "window.nyxtTerminal={write:function(b64){var binary=atob(b64);"
     "var bytes=new Uint8Array(binary.length);"
     "for(var i=0;i<binary.length;i++) bytes[i]=binary.charCodeAt(i);term.write(bytes);},"
     "focus:function(){term.focus();}};term.focus();send(\"ready\",\"buffer=\"+id);}"
     "boot();})();")))

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

(defun handle-terminal-request (url)
  "Apply a write, resize, or ready request from the terminal page."
  (let* ((params (quri:uri-query-params (quri:uri url)))
         (buffer (buffer-by-id (assoc-value params "buffer" :test #'equal)))
         (mode (and buffer (find-submode 'terminal-mode buffer)))
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

(defun ensure-delete-handler (mode)
  "Close MODE's shell when its buffer is deleted."
  (unless (delete-handler mode)
    (setf (delete-handler mode)
          (make-instance 'hooks:handler
                         :name (gensym "TERMINAL-DELETE")
                         :fn (lambda (buffer)
                               (declare (ignore buffer))
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
                                             (send-terminal-output mode octets))
                                :on-exit (lambda ()
                                           (echo "Terminal exited."))))
          (ensure-delete-handler mode)
          (ensure-exit-handler mode))
      (error (condition)
        (echo-warning "Terminal failed to start: ~a" condition)))))

(defmethod disable ((mode terminal-mode) &key)
  (when (and *browser* (exit-handler mode))
    (hooks:remove-hook (before-exit-hook *browser*) (exit-handler mode))
    (setf (exit-handler mode) nil))
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

(define-command-global toggle-terminal-keys (&optional (buffer (current-buffer)))
  "Toggle whether keys go to the terminal or to Nyxt."
  (let ((mode (find-submode 'terminal-mode buffer)))
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
