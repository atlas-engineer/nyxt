;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class socket-data-path (data-path)
  ((editable nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Socket files are typically stored in a dedicated directory."))

(defmethod expand-data-path ((profile data-profile) (data-path socket-data-path))
  "Return finalized path for socket files."
  (unless (getf *options* :no-socket)
    (let ((path (or (getf *options* :socket)
                    (uiop:getenv "NYXT_SOCKET")
                    (expand-default-path
                     data-path
                     :root (namestring (uiop:xdg-runtime-dir +data-root+))))))
      (unless (uiop:emptyp path)
        path))))

(export-always '*socket-path*)
(defvar *socket-path* (make-instance 'socket-data-path :basename "nyxt.socket")
  "Path of the Unix socket used to communicate between different instances of
Nyxt.

If `expand-path' resolves this to `nil', then Nyxt starts in multi-instance mode.
This means that re-running Nyxt will start a new instance of Nyxt instead of
prompting the first instance.

This path cannot be set from the initialization file because we want to be able
to set and use the socket without parsing any file.  Instead, the socket can be
set from the corresponding command line option or the NYXT_SOCKET environment
variable.")

(export-always 'nyxt-init-file)
(defun nyxt-init-file (&optional subpath)
  "Return SUBPATH relative to `*init-file-path*'.
Return nil if `*init-file-path*' is nil."
  (if subpath
      (uiop:subpathname* (uiop:pathname-directory-pathname
                          (expand-path *init-file-path*))
                         subpath)
      (expand-path *init-file-path*)))

(defun handle-malformed-cli-arg (condition)
  (format t "Error parsing argument ~a: ~a.~&" (opts:option condition) condition)
  (opts:describe)
  (uiop:quit))

(sera:eval-always
  (defun define-opts ()
    "Define command line options.
This must be called on startup so that code is executed in the user environment
and not the build environment."
    (opts:define-opts
      (:name :help
       :description "Print this help and exit."
       :short #\h
       :long "help")
      (:name :verbose
       :short #\v
       :long "verbose"
       :description "Print debugging information to stdout.")
      (:name :version
       :long "version"
       :description "Print version and exit.")
      (:name :system-information
       :long "system-information"
       :description "Print system information and exit.")
      (:name :init
       :short #\i
       :long "init"
       :arg-parser #'identity
       :description (format nil "Set path to initialization file.
Default: ~s" (expand-path *init-file-path*)))
      (:name :no-init
       :short #\I
       :long "no-init"
       :description "Do not load the initialization file.")
      (:name :auto-config
       :short #\c
       :long "auto-config"
       :arg-parser #'identity
       :description (format nil "Set path to auto-config file.
Default: ~s" (expand-path *auto-config-file-path*)))
      (:name :no-auto-config
       :short #\C
       :long "no-auto-config"
       :description "Do not load the user auto-config file.")
      (:name :socket
       :short #\s
       :long "socket"
       :arg-parser #'identity
       :description "Set path to socket.
Unless evaluating remotely (see --remote), Nyxt starts in single-instance mode when a socket is set.
The socket can also be set from the NYXT_SOCKET environment variable.")
      (:name :no-socket
       :short #\S
       :long "no-socket"
       :description "Do not use any socket.")
      (:name :eval
       :short #\e
       :long "eval"
       :arg-parser #'identity
       :description "Eval the Lisp expressions.  Can be specified multiple times.")
      (:name :load
       :short #\l
       :long "load"
       :arg-parser #'identity
       :description "Load the Lisp file.  Can be specified multiple times.")
      (:name :quit
       :short #\q
       :long "quit"
       :description "Quit after --load or --eval.")
      (:name :script
       :long "script"
       :arg-parser #'identity
       :description "Load the Lisp file (skip #! line if any), skip init file, then exit.
Set to '-' to read standard input instead.")
      (:name :remote
       :short #\r
       :long "remote"
       :description "Send the --eval and --load arguments to the running instance of Nyxt.
Implies --quit.
The remote instance must be listening on a socket which you can specify with --socket
and have the `remote-execution-p' browser slot to non-nil.")
      (:name :data-profile
       :short #\d
       :long "data-profile"
       :arg-parser #'identity
       :description "Use the given data profile. ")
      (:name :list-data-profiles
       :long "list-data-profiles"
       :description "List the known data profiles and exit.
Known profiles are found among global variables that are a subclass of
`data-profile'.")
      (:name :with-path
       :long "with-path"
       :arg-parser (lambda (arg) (str:split "=" arg :limit 2))
       :description "Set data path reference to the given path.
Can be specified multiple times.  An empty path means it won't be used.
Example: --with-path bookmarks=/path/to/bookmarks
         --with-path session="))))
;; Also define command line options at read-time because we parse
;; `opts::*options*' in `start'.
(sera:eval-always (define-opts))

(define-command quit ()
  "Quit Nyxt."
  (hooks:run-hook (before-exit-hook *browser*))
  (loop for window in (window-list)
        do (ffi-window-delete window))
  (ffi-kill-browser *browser*)
  (setf (slot-value *browser* 'ready-p) nil)
  (when (socket-thread *browser*)
    (ignore-errors
     (bt:destroy-thread (socket-thread *browser*)))
    ;; Warning: Don't attempt to remove socket-path if socket-thread was not
    ;; running or we risk remove an unrelated file.
    (let ((socket-path (expand-path *socket-path*)))
      (when (uiop:file-exists-p socket-path)
        (log:info "Deleting socket ~s." socket-path)
        (uiop:delete-file-if-exists socket-path))))
  (unless *run-from-repl-p*
    (uiop:quit 0 nil))
  (mapc #'bt:destroy-thread (non-terminating-threads *browser*)))

(define-command quit-after-clearing-session (&key confirmation-p) ; TODO: Rename?
  "Close all buffers then quit Nyxt."
  (delete-all-buffers :confirmation-p confirmation-p)
  (quit))

(define-command start-swank (&optional (swank-port *swank-port*))
  "Start a Swank server that can be connected to, for instance, in
Emacs via SLIME.

Warning: This allows Nyxt to be controlled remotely, that is, to
execute arbitrary code with the privileges of the user running Nyxt.
Make sure you understand the security risks associated with this
before running this command."
  (swank:create-server :port swank-port :dont-close t)
  (echo "Swank server started at port ~a" swank-port))

;; From sbcl/src/code/load.lisp
(defun maybe-skip-shebang-line (stream)
  (let ((p (file-position stream)))
    (when p
      (flet ((next () (read-byte stream nil)))
        (unwind-protect
             (when (and (eq (next) (char-code #\#))
                        (eq (next) (char-code #\!)))
               (setf p nil)
               (loop for x = (next)
                     until (or (not x) (eq x (char-code #\newline)))))
          (when p
            (file-position stream p)))))))

(cffi:defcallback handle-interrupt
    :void ((signum :int) (siginfo :pointer) (ptr :pointer))
  (declare (ignore signum siginfo ptr))
  (quit))

(export-always 'entry-point)
(defun entry-point ()
  "Read the CLI arguments and start the browser.
This is the entry point of the binary program.
Don't run this from a REPL, prefer `start' instead."
  (define-opts)
  (multiple-value-bind (options free-args)
      (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                     (opts:missing-arg #'handle-malformed-cli-arg)
                     (opts:arg-parser-failed #'handle-malformed-cli-arg))
        (opts:get-opts))
    (setf *run-from-repl-p* nil)             ; Not a REPL.
    (let ((interrupt-sigaction (cffi:foreign-alloc '(:struct isys::sigaction))))
      ;; Mimics https://www.systutorials.com/catching-the-signal-sent-by-kill-in-c-on-linux/
      (isys:memset interrupt-sigaction 0 (cffi:foreign-type-size '(:struct isys::sigaction)))
      (setf (cffi:foreign-slot-value
             interrupt-sigaction '(:struct isys:sigaction) 'isys::sigaction)
            (cffi:callback handle-interrupt))
      (setf (cffi:foreign-slot-value
             interrupt-sigaction '(:struct isys:sigaction) 'isys::flags)
            isys:sa-siginfo)
      (isys:sigaction isys:sigint interrupt-sigaction (cffi:null-pointer))
      (isys:sigaction isys:sigterm interrupt-sigaction (cffi:null-pointer)))
    (apply #'start (append options (list :urls free-args)))))

(declaim (ftype (function ((or null trivial-types:pathname-designator)
                           &key (:package (or null package))))
                load-lisp))
(defun load-lisp (file &key package)
  "Load the Lisp FILE (or stream).
We accept a null FILE value so that `load-lisp' may be called over the expansion
of data-paths, which may be nil.
Return the short error message and the full error message as second value."
  (let ((*package* (or (find-package package) *package*)))
    (flet ((unsafe-load ()
             (unless (uiop:emptyp file)
               (cond
                 ((streamp file)
                  (load file))
                 ((uiop:file-exists-p file)
                  (log:info "Loading Lisp file ~s." file)
                  (load file))
                 (t
                  (log:debug "Lisp file ~s does not exist." file))))
             nil))
      (if *run-from-repl-p*
          (unsafe-load)
          (handler-case
              (unsafe-load)
            (error (c)
              (let* ((error-message "Could not load the init file")
                     (type-error-message (str:concat error-message
                                                     " because of a type error"))
                     (message (if (subtypep (type-of c) 'type-error)
                                  type-error-message
                                  error-message))
                     (full-message (format nil "~a: ~a" message c)))
                (if *browser*
                    (error-in-new-window "*Init file errors*" full-message)
                    (values message full-message)))))))))

(define-command load-file ()
  "Load the prompted Lisp file."
  (prompt
   :prompt "Load file"
   :input (namestring (alex:if-let ((init-path (expand-path *init-file-path*)))
                        (uiop:pathname-directory-pathname (pathname init-path))
                        (uiop:getcwd)))
   :sources
   ;; TODO: Load several files at once? `file-source' allows that.
   (make-instance 'user-file-source
                  :actions (list (make-command load-file* (files)
                                   (dolist (file files)
                                     (load-lisp file)))))))

(define-command load-init-file (&key (init-file (expand-path *init-file-path*)))
  "Load or reload the init file."
  (load-lisp init-file :package (find-package :nyxt-user)))

(defun eval-expr (expr)
  "Evaluate the form EXPR (string) and print the result of the last expresion."
  (handler-case
      (with-input-from-string (input expr)
        (format t "~a~&"
                (loop with result = nil
                      for object = (read input nil :eof)
                      until (eq object :eof)
                      do (setf result (eval object))
                      finally (return result))))
    (error (c)
      (log:error "Evaluation error:~&~a" c))))

(defun parse-urls (expr)
  "Do _not_ evaluate EXPR and try to open URLs that were send to it.
EXPR is expected to be as per the expression sent in `listen-or-query-socket'."
  (let ((urls (ignore-errors (rest (read-from-string expr nil)))))
    (if (and urls (every #'stringp urls))
        (apply #'open-external-urls urls)
        (progn
          (log:warn "Could not extract URLs from ~s." expr)
          nil))))

(export-always 'open-external-urls)
(declaim (ftype (function (&rest string)) open-external-urls))
(defun open-external-urls (&rest url-strings)
  "Open URL-STRINGS on the renderer thread and return URLs.
This is a convenience wrapper to make remote code execution to open URLs as
short as possible.
It takes URL-STRINGS so that the URL argument can be `cl-read' in case
`remote-execution-p' is nil."
  (let ((urls (strings->urls url-strings)))
    (if urls
        (log:info "Externally requested URL(s): ~{~a~^, ~}" urls)
        (log:info "Externally pinged."))
    (ffi-within-renderer-thread
     *browser*
     (lambda () (open-urls urls)))
    urls))

(defun listen-socket ()
  (let ((socket-path (expand-path *socket-path*)))
    (when socket-path
      (ensure-parent-exists socket-path)
      ;; TODO: Catch error against race conditions?
      (iolib:with-open-socket (s :address-family :local
                                 :connect :passive
                                 :local-filename socket-path)
        ;; We don't want group members or others to flood the socket or, worse,
        ;; execute code.
        (setf (iolib/os:file-permissions socket-path)
              (set-difference (iolib/os:file-permissions socket-path)
                              '(:group-read :group-write :group-exec
                                :other-read :other-write :other-exec)))
        (loop as connection = (iolib:accept-connection s)
              while connection
              do (progn
                   (alex:when-let ((expr (alex:read-stream-content-into-string connection)))
                     (unless (uiop:emptyp expr)
                       (if (remote-execution-p *browser*)
                           (progn
                             (log:info "External evaluation request: ~s" expr)
                             (eval-expr expr))
                           (parse-urls expr))))
                   ;; If we get pinged too early, we do not have a current-window yet.
                   (when (current-window)
                     (ffi-window-to-foreground (current-window))))))
      (log:info "Listening on socket ~s" socket-path))))

(defun listening-socket-p ()
  (ignore-errors
   (iolib:with-open-socket (s :address-family :local
                              :remote-filename (expand-path *socket-path*))
     (iolib:socket-connected-p s))))

(defun file-is-socket-p (path)
  "Return non-nil if a PATH is a socket."
  (and (uiop:file-exists-p path)
       #+darwin
       (equal "=" (uiop:run-program (list "stat" "-f" "%T" path)
                                    :output '(:string :stripped t)))
       #+(and (not darwin) (not sbcl))
       (eq :socket (osicat:file-kind path))
       #+(and (not darwin) sbcl)
       (flet ((socket-p (path)
                (let ((socket-mask 49152)
                      (mode-mask 61440))
                  (= socket-mask
                     (logand mode-mask
                             (sb-posix:stat-mode (sb-posix:stat path)))))))
         (socket-p path))))

(declaim (ftype (function ((or null (cons quri:uri *)))) listen-or-query-socket))
(defun listen-or-query-socket (urls)
  "If another Nyxt is listening on the socket, tell it to open URLS.
Otherwise bind socket and return the listening thread."
  (let ((socket-path (expand-path *socket-path*)))
    (cond
      ((listening-socket-p)
       (if urls
           (log:info "Nyxt already started, requesting to open URL(s): ~{~a~^, ~}" urls)
           (log:info "Nyxt already started."))
       (iolib:with-open-socket (s :address-family :local
                                  :remote-filename socket-path)
         ;; Can't use `render-url' at this point because the GTK loop is not running.
         (format s "~s" `(open-external-urls ,@(mapcar #'quri:render-uri urls))))
       nil)
      ((and (uiop:file-exists-p socket-path)
            (not (file-is-socket-p socket-path)))
       (log:error "Could not bind socket ~a, non-socket file exists." socket-path)
       nil)
      (t
       (log:info "Listening to socket ~s." socket-path)
       (uiop:delete-file-if-exists socket-path) ; Safe since socket-path is a :socket at this point.
       (run-thread (listen-socket))))))

(defun remote-eval (expr)
  "If another Nyxt is listening on the socket, tell it to evaluate EXPR."
  (if (listening-socket-p)
      (progn
        (iolib:with-open-socket (s :address-family :local
                                   :remote-filename (expand-path *socket-path*))
          (write-string expr s))
        (uiop:quit))
      (progn
        (log:info "No instance running.")
        (uiop:quit))))

(defun indent (s space-count)
  "Return s with all lines but the first indented by SPACE-COUNT."
  (let* ((lines (sera:lines s))
         (indent (make-string space-count :initial-element #\space)))
    (str:join +newline+
              (cons (first lines)
                    (mapcar (lambda (s)
                              (str:concat indent s))
                            (rest lines))))))

(export-always 'start)
(define-function start `(&rest options &key urls
                               ,@(mapcar (alex:compose #'intern
                                                       #'symbol-name
                                                       #'opts::name)
                                         opts::*options*))
  (format nil "Parse command line or REPL options then start the browser.
Load URLS if any (a list of strings).

This functions focuses on OPTIONS parsing, see `start-browser' for the actual
startup procedure.

The OPTIONS are the same as the command line options.

~a

Examples:

- Display version and return immediately:
  (nyxt:start :version t)

- Start the browser with increased verbosity, a different history data path and
  open the given URLs.
  (nyxt:start :urls '(\"https://nyxt.atlas.engineer\" \"https://en.wikipedia.org\")
              :verbose t
              :with-path '(\"history\" \"/tmp/nyxt/history.lisp\"))"
          (with-output-to-string (s) (opts:describe :stream s)))
  ;; Extensions should be made accessible straight from the beginning,
  ;; e.g. before a script is run.
  (pushnew 'nyxt-source-registry asdf:*default-source-registries*)
  (asdf:clear-configuration)

  ;; Options should be accessible anytime, even when run from the REPL.
  (setf *options* options)

  (if (getf options :verbose)
      (progn
        (log:config :debug)
        (format t "Arguments parsed: ~a and ~a~&" options urls))
      (log:config :pattern "<%p> [%D{%H:%M:%S}] %m%n"))

  (cond
    ((getf options :help)
     (opts:describe :prefix "nyxt [options] [URLs]"))

    ((getf options :version)
     (format t "Nyxt version ~a~&" +version+))

    ((getf options :system-information)
     (princ (system-information)))

    ((getf options :list-data-profiles)
     (load-lisp (expand-path *init-file-path*) :package (find-package :nyxt-user))
     (mapcar (lambda (pair)
               (format t "~a~10t~a~&" (first pair) (indent (second pair) 10)))
             (mapcar #'rest (package-data-profiles))))

    ((getf options :script)
     (flet ((run-script (stream)
              (maybe-skip-shebang-line stream)
              (load-lisp stream)))
       (match (getf options :script)
         ("-" (run-script *standard-input*))
         (file (with-open-file (f file :element-type :default)
                 (run-script f))))))

    ((and (or (getf options :load)
              (getf options :eval))
          (or (getf options :quit)
              (getf options :remote)))
     (start-load-or-eval))

    (t
     (with-protect ("Error: ~a" :condition)
       (start-browser urls))))

  (unless *run-from-repl-p* (uiop:quit)))

(defun load-or-eval (&key remote)
  (loop for (opt value . nil) on *options*
        do (match opt
             (:load (let ((value (uiop:truename* value)))
                      ;; Absolute path is necessary since remote process may have
                      ;; a different working directory.
                      (if remote
                          (remote-eval (format nil "~s" `(load-lisp ,value)))
                          (load-lisp value))))
             (:eval (if remote
                        (remote-eval value)
                        (eval-expr value))))))

(defun start-load-or-eval ()
  "Evaluate Lisp.
The evaluation may happen on its own instance or on an already running instance."
  (load-lisp (expand-path *auto-config-file-path*) :package (find-package :nyxt-user))
  (load-lisp (expand-path *init-file-path*):package (find-package :nyxt-user))
  (load-or-eval :remote (getf *options* :remote)))

(defun start-browser (url-strings)
  "Start Nyxt.
First load AUTO-CONFIG-FILE.
Then load INIT-FILE if non-nil.
Instantiate `*browser*'.
Finally, run the browser, load URL-STRINGS if any, then run
`*after-init-hook*'."
  (let* ((urls (strings->urls url-strings))
         (thread (when (expand-path *socket-path*)
                   (listen-or-query-socket urls)))
         (startup-timestamp (local-time:now))
         (startup-error-reporter nil))
    (when (or thread
              (getf *options* :no-socket)
              (null (expand-path *socket-path*)))
      (format t "Nyxt version ~a~&" +version+)
      (load-lisp (expand-path *auto-config-file-path*) :package (find-package :nyxt-user))
      (match (multiple-value-list (load-lisp (expand-path *init-file-path*)
                                             :package (find-package :nyxt-user)))
        (nil nil)
        ((list message full-message)
         (setf startup-error-reporter
               (lambda ()
                 (echo-warning "~a." message)
                 (error-in-new-window "*Init file errors*" full-message)))))
      (load-or-eval :remote nil)
      (setf *browser* (make-instance 'user-browser
                                     :startup-error-reporter-function startup-error-reporter
                                     :startup-timestamp startup-timestamp))
      (setf (socket-thread *browser*) thread)
      ;; Defaulting to :nyxt-user is convenient when evaluating code (such as
      ;; remote execution or the integrated REPL).
      ;; This must be done in a separate thread because the calling thread may
      ;; have set `*package*' as an initial-binding (see `bt:make-thread'), as
      ;; is the case with the SLY mrepl thread.
      (bt:make-thread (lambda ()
                        (in-package :nyxt-user)))
      (ffi-initialize *browser* urls startup-timestamp))))

(define-command nyxt-init-time ()
  "Return the duration of Nyxt initialization."
  (echo "~,2f seconds" (slot-value *browser* 'init-time)))
