(in-package :nyxt)

(export-always '*init-file-path*)
(defvar *init-file-path* (make-instance 'data-path :basename "init")
  "The path of the initialization file.")

(export-always '*socket-path*)
(defvar *socket-path* (make-instance 'data-path :basename "nyxt.socket")
  "Path string of the Unix socket used to communicate between different
instances of Nyxt.

This path cannot be set from the init file because we want to be able to set and
use the socket without parsing any init file.")

(defmethod expand-data-path ((profile data-profile) (path (eql *init-file-path*)))
  "Return path of the init file."
  (unless (getf *options* :no-init)
    (match (getf *options* :init)
      (new-path
       (expand-default-path (make-instance 'data-path
                                           :basename (or new-path (basename path))
                                           ;; Specify `dirname' here since
                                           ;; *init-file-path* is evaluated
                                           ;; at compile-time.
                                           :dirname (uiop:xdg-config-home +data-root+)))))))

(defun handle-malformed-cli-arg (condition)
  (format t "Error parsing argument ~a: ~a.~&" (opts:option condition) condition)
  (opts:describe)
  (uiop:quit))

(defun parse-cli-args ()
  "Parse command line arguments."
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
    (:name :init
           :short #\i
           :long "init"
           :arg-parser #'identity
           :description "Set path to initialization file.")
    (:name :no-init
           :short #\I
           :long "no-init"
           :description "Do not load the user init file.")
    (:name :socket
           :short #\s
           :long "socket"
           :arg-parser #'identity
           :description "Set path to socket.
Unless evaluating remotely (see --remote), Nyxt starts in single-instance mode when a socket is set.")
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
         --with-path session="))
  (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                 (opts:missing-arg #'handle-malformed-cli-arg)
                 (opts:arg-parser-failed #'handle-malformed-cli-arg))
    (opts:get-opts)))

(define-command quit ()
  "Quit Nyxt."
  (hooks:run-hook (before-exit-hook *browser*))
  (loop for window in (window-list)
        do (ffi-window-delete window))
  (ffi-kill-browser *browser*)
  (when (socket-thread *browser*)
    (ignore-errors
     (bt:destroy-thread (socket-thread *browser*)))
    ;; Warning: Don't attempt to remove socket-path if socket-thread was not
    ;; running or we risk remove an unrelated file.
    (let ((socket-path (expand-path *socket-path*)))
      (when (uiop:file-exists-p socket-path)
        (log:info "Deleting socket ~s." socket-path)
        (uiop:delete-file-if-exists socket-path))))
  (unless *keep-alive*
    (uiop:quit 0 nil)))

(define-command quit-after-clearing-session ()
  "Clear session then quit Nyxt."
  (setf
   (session-store-function *browser*) nil
   (session-restore-function *browser*) nil)
  (uiop:delete-file-if-exists (expand-path (session-path *browser*)))
  (quit))

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

(export-always 'entry-point)
(defun entry-point ()
  "Read the CLI arguments and start the browser.
This is the entry point of the binary program.
Don't run this from a REPL, prefer `start' instead."
  (multiple-value-bind (options free-args)
      (parse-cli-args)
    (setf *keep-alive* nil)             ; Not a REPL.
    (in-package :nyxt-user)
    (apply #'start options free-args)))

(declaim (ftype (function (trivial-types:pathname-designator &key (:package (or null package))))
                load-lisp))
(defun load-lisp (file &key package)
  "Load the Lisp FILE (or stream).
Return the short error message and the full error message as second value."
  (let ((*package* (or (find-package package) *package*)))
    (flet ((unsafe-load ()
             (when (equal "" file)
               (error "Can't load empty file name."))
             (cond
               ((streamp file)
                (load file))
               ((uiop:file-exists-p file)
                (log:info "Loading Lisp file ~s." file)
                (load file)))
             nil))
      (if *keep-alive*
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
  (with-result (file-name-input (read-from-minibuffer
                                 (make-minibuffer
                                  :input-prompt "Load file"
                                  :hide-suggestion-count-p t)))
    (load-lisp file-name-input)))

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
EXPR is expected to be as per the expression sent in `bind-socket-or-quit'."
  (let ((urls (ignore-errors (second (second (read-from-string expr nil))))))
    (if (and urls (every #'stringp urls))
        (open-external-urls urls)
        (progn
          (log:warn "Could not extract URLs from ~s." expr)
          nil))))

(export-always 'make-startup-function)
(defun make-startup-function (&key (buffer-fn #'help))
  "Return a function suitable as a `browser' `startup-function'.
To change the default buffer, e.g. set it to a given URL:

  (make-startup-function
   :buffer-fn (lambda () (make-buffer :url \"https://example.org\")))"
  (lambda (&optional urls)
    (let ((window (window-make *browser*)))
      (if urls
          (open-urls urls)
          (window-set-active-buffer window (funcall-safely buffer-fn))))
    (when (startup-error-reporter-function *browser*)
      (funcall-safely (startup-error-reporter-function *browser*)))
    (when (expand-path (session-path *browser*))
      (flet ((restore-session ()
               (when (and (session-restore-function *browser*)
                          (uiop:file-exists-p (expand-path (session-path *browser*))))
                 (log:info "Restoring session ~s." (expand-path (session-path *browser*)))
                 (funcall (session-restore-function *browser*)))))
        (match (session-restore-prompt *browser*)
          (:always-ask (with-confirm ("Restore previous session?")
                         (restore-session)))
          (:always-restore (restore-session))
          (:never-restore (log:info "Not restoring session.")))))))

(export-always 'open-external-urls)
(defun open-external-urls (urls)
  "Open URLs on the renderer thread and return URLs.
This is a convenience wrapper to make remote code execution to open URLs as
short as possible."
  (if urls
      (log:info "Externally requested URL(s): ~{~a~^, ~}" urls)
      (log:info "Externally pinged."))
  (ffi-within-renderer-thread
   *browser*
   (lambda () (open-urls urls)))
  urls)

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
        (setf (osicat:file-permissions socket-path) '(:user-read :user-write))
        ;; Since we are in a separate thread, we need to set the default package
        ;; for remote execution.
        (in-package :nyxt-user)
        (loop as connection = (iolib:accept-connection s)
              while connection
              do (progn
                   (match (alex:read-stream-content-into-string connection)
                     ((guard expr (not (uiop:emptyp expr)))
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

(defun bind-socket-or-quit (urls)
  "If another Nyxt is listening on the socket, tell it to open URLS.
Otherwise bind socket."
  (let ((socket-path (expand-path *socket-path*)))
    (cond
      ((listening-socket-p)
       (progn
         (if urls
             (log:info "Nyxt already started, requesting to open URL(s): ~{~a~^, ~}" urls)
             (log:info "Nyxt already started."))
         (iolib:with-open-socket (s :address-family :local
                                    :remote-filename socket-path)
           (format s "~s" `(open-external-urls ',urls)))
         (unless *keep-alive*
           (uiop:quit))))
      ((and (uiop:file-exists-p socket-path)
            (not (eq :socket (osicat:file-kind socket-path))))
       (log:error "Could not bind socket ~a, non-socket file exists." socket-path))
      (t (progn
           (log:info "Listening to socket ~s." socket-path)
           (uiop:delete-file-if-exists socket-path) ; Safe since socket-path is a :socket at this point.
           (setf (socket-thread *browser*) (bt:make-thread #'listen-socket)))))))

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

(export-always 'start)
(defun start (&optional options &rest free-args)
  "Parse options (either from command line or from the REPL) and perform the
corresponding action.
With no action, start the browser.

REPL examples:

- Display version and return immediately:
  (nyxt:start '(:version t))

- Start the browser with increased verbosity, a different history data path and
  open the given URLs.
  (nyxt:start '(:verbose t :with-path (\"history\" \"/tmp/nyxt/history.lisp\"))
              \"https://nyxt.atlas.engineer\" \"https://en.wikipedia.org\")"
  ;; Options should be accessible anytime, even when run from the REPL.
  (setf *options* options)

  ;; Preset socket here, we want --no-socket to work regardless of the user
  ;; config data-profile.
  ;; We also want to expand xdg-data-home dynamically.
  (setf *socket-path*
        (if (getf *options* :no-socket)
            nil
            (make-instance 'data-path
                           :basename (or (getf *options* :socket) (basename *socket-path*))
                           :dirname (uiop:xdg-data-home +data-root+))))

  (if (getf *options* :verbose)
      (progn
        (log:config :debug)
        (format t "Arguments parsed: ~a and ~a~&" *options* free-args))
      (log:config :pattern "<%p> [%D{%H:%M:%S}] %m%n"))

  (cond
    ((getf options :help)
     (opts:describe :prefix "nyxt [options] [URLs]"))

    ((getf options :version)
     (format t "Nyxt version ~a~&" +version+))

    ((getf options :list-data-profiles)
     (unless (or (getf *options* :no-init)
                 (not (expand-path *init-file-path*)))
       (load-lisp (expand-path *init-file-path*) :package (find-package :nyxt-user)))
     (mapcar (lambda (pair)
               (format t "~a~10t~a~&" (first pair) (second pair)))
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
     (start-browser free-args)))

  (unless *keep-alive* (uiop:quit)))

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
  (unless (or (getf *options* :no-init)
              (not (expand-path *init-file-path*)))
    (load-lisp (expand-path *init-file-path*) :package (find-package :nyxt-user)))
  (load-or-eval :remote (getf *options* :remote)))

(defun start-browser (free-args)
  "Load INIT-FILE if non-nil.
Instantiate `*browser*'.
Start Nyxt and load URLS if any.
Finally,run the `*after-init-hook*'."
  (let ((startup-timestamp (local-time:now))
        (startup-error-reporter nil))
    (format t "Nyxt version ~a~&" +version+)

    (unless (or (getf *options* :no-init)
                (not (expand-path *init-file-path*)))
      (match (multiple-value-list (load-lisp (expand-path *init-file-path*)
                                             :package (find-package :nyxt-user)))
        (nil nil)
        ((list message full-message)
         (setf startup-error-reporter
               (lambda ()
                 (notify (str:concat message "."))
                 (error-in-new-window "*Init file errors*" full-message))))))
    (load-or-eval :remote nil)
    (setf *browser* (make-instance *browser-class*
                                   :startup-error-reporter-function startup-error-reporter
                                   :startup-timestamp startup-timestamp))
    (log:info "Using data profile ~s." (name (data-profile *browser*)))
    (when (expand-path *socket-path*)
      (bind-socket-or-quit free-args))
    (ffi-initialize *browser* free-args startup-timestamp)))

(define-command nyxt-init-time ()
  "Return the duration of Nyxt initialization."
  (echo "~,2f seconds" (slot-value *browser* 'init-time)))
