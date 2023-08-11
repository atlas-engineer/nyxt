;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class config-directory-file (files:config-file nyxt-file)
  ((files:base-path #p""))
  (:export-class-name-p t)
  (:documentation "Nyxt directory for config files."))

(define-class config-special-file (config-directory-file)
  ((files:base-path #p"")
   (command-line-option :config
                        :accessor nil
                        :type keyword))
  (:export-class-name-p t)
  (:documentation "Like `config-directory-file' but can be controlled from command line options."))

(define-class config-file (config-special-file nyxt-lisp-file)
  ((files:base-path #p"config")
   (command-line-option :config
                        :accessor nil
                        :type keyword))
  (:export-class-name-p t)
  (:documentation "Lisp configuration file which path can be controlled from command line options.
Unlike `auto-config-file', it can only be loaded with `cl:load', it is not meant to be read with
`nfiles:read-file' or `nfiles:content'."))

(defmethod files:read-file ((profile nyxt-profile) (file config-file) &key)
  "Don't load anything for `config-file's since they are Lisp file to be loaded with `cl:load'."
  nil)

(define-class auto-config-file (config-special-file nyxt-lisp-file)
  ((files:base-path (files:join #p"auto-config." (princ-to-string (version))))
   (command-line-option :auto-config
                        :accessor nil
                        :type keyword))
  (:export-class-name-p t)
  (:documentation "Lisp configuration file which path can be controlled from command line options.
Unlike `config-file', it can both loaded with `cl:load' and read with
`nfiles:read-file'.  The latter should return a structured reification of the configuration."))

(defmethod files:resolve ((profile nyxt-profile) (config-file config-special-file))
  (let* ((option (slot-value config-file 'command-line-option))
         (no-option (alex:make-keyword
                     (uiop:strcat "NO-" (symbol-name option)))))
    (if (getf *options* no-option)
        #p""
        (let ((path (or (uiop:ensure-pathname (getf *options* option))
                        (call-next-method))))
          (unless (uiop:emptyp path)
            (when (and (getf *options* option) (not (uiop:file-exists-p path)))
              (log:warn "File ~s does not exist." path))
            path)))))

(defparameter %report-existing-nyxt-2-config
  (sera:once
   (lambda (path)
     (when (not (uiop:file-exists-p path))
       (let ((nyxt-2-path (files:expand (make-instance 'config-file
                                                       :base-path #p"init"))))
         (when (uiop:file-exists-p nyxt-2-path)
           (log:warn "Found ~a, possibly a Nyxt 2 configuration.
Consider porting your configuration to ~a."
                     nyxt-2-path path))))
     nil)))

(defmethod files:resolve ((profile nyxt-profile) (config-file config-file))
  (let ((path (call-next-method)))
    (funcall %report-existing-nyxt-2-config path)
    path))

(export-always '*auto-config-file*)
(defvar *auto-config-file* (make-instance 'auto-config-file)
  "The generated configuration file.")

(export-always '*config-file*)
(defvar *config-file* (make-instance 'config-file)
  "The configuration file entry point.")

(define-class log-file (files:data-file nyxt-file)
  ((files:base-path #p"nyxt.log")
   (files:name "log-file"))
  (:export-class-name-p t)
  (:documentation "Data file for Nyxt logs."))

(export-always '*log-file*)
(defvar *log-file* (make-instance 'log-file)
  "Path to the file where log is saved.
This is global because logging starts before the `*browser*' is even initialized.")

(defvar *log-pattern* "<%p> [%D{%H:%M:%S}] %m%n"
  "Non-verbose log pattern.")

(define-class nyxt-source-directory (nyxt-file)
  ((files:name "source"))
  (:export-class-name-p t)
  (:documentation "Directory with Nyxt sources."))

(defmethod files:resolve ((profile nyxt-profile) (directory nyxt-source-directory))
  "Try hard to find Nyxt source on disk.
Return #p\"\" if not found."
  (let ((asd-path (ignore-errors (asdf:system-source-directory :nyxt))))
    (if (uiop:directory-exists-p asd-path)
        asd-path
        (or
         ;; XDG / FHS:
         (find-if (lambda (d)
                    (uiop:file-exists-p (uiop:merge-pathnames* "nyxt.asd" d)))
                  (uiop:xdg-data-dirs "nyxt"))
         ;; Location relative to the binary:
         (let ((relative-dir (uiop:merge-pathnames*
                              "share/nyxt/"
                              (files:parent
                               (files:parent
                                (uiop:ensure-pathname
                                 (first (uiop:raw-command-line-arguments)) :truenamize t))))))
           (when (uiop:file-exists-p (uiop:merge-pathnames* "nyxt.asd" relative-dir))
             relative-dir))
         ;; Not found:
         #p""))))

(export-always '*source-directory*)
(defvar *source-directory* (make-instance 'nyxt-source-directory)
  "The directory where the source code is stored.
This is set globally so that it can be looked up if there is no
`*browser*' instance.")

(define-class extensions-directory (files:data-file nyxt-file)
  ((files:base-path #p"extensions/")
   (files:name "extensions"))
  (:export-class-name-p t)
  (:documentation "Nyxt data subdirectory for Lisp extensions."))

(export-always '*extensions-directory*)
(defvar *extensions-directory* (make-instance 'extensions-directory)
  "The directory where extensions are stored.
This is set globally so that extensions can be loaded even if there is no
`*browser*' instance.")

(export-always 'nyxt-source-registry)
(defun nyxt-source-registry ()
  "Return Nyxt-specific ASDF registry, with source and extension directories."
  (let ((source-dir (files:expand *source-directory*)))
    `(:source-registry
      (:tree ,(files:expand *extensions-directory*))
      ,@(unless (uiop:absolute-pathname-p source-dir)
          `((:tree ,source-dir))) ; Probably useless since systems are immutable.
      :inherit-configuration)))

(defun set-nyxt-source-location (pathname) ; From `sb-ext:set-sbcl-source-location'.
  "Initialize the NYXT logical host based on PATHNAME, which should be the
top-level directory of the Nyxt sources. This will replace any existing
translations for \"NYXT:source;\" and \"NYXT:libraries;\". Other \"NYXT:\"
translations are preserved."
  (let ((truename (truename pathname))
        (current-translations
          (remove-if (lambda (translation)
                       (or (pathname-match-p "NYXT:source;" translation)
                           (pathname-match-p "NYXT:libraries;" translation)))
                     (logical-pathname-translations "NYXT")
                     :key #'first)))
    (flet ((physical-target (component)
             (merge-pathnames
              (make-pathname :directory (list :relative component
                                              :wild-inferiors)
                             :name :wild
                             :type :wild)
              truename)))
      (setf (logical-pathname-translations "NYXT")
            `(("NYXT:source;**;*.*.*" ,(physical-target "source"))
              ("NYXT:libraries;**;*.*.*" ,(physical-target "libraries"))
              ,@current-translations)))))

(define-class slot-form ()
  ((name nil
         :type symbol)
   (value nil
          :type t))
  (:export-class-name-p t)
  (:documentation "A form to set slot with `name' to `value'."))

(define-class class-form ()
  ((class-name nil
               :type symbol)
   (forms '()
          :type (maybe (cons (or cons slot-form) *))))
  (:export-class-name-p t)
  (:documentation "A set of `forms' for class configuration."))

(defun read-init-form-slot (class-name sexp)
  "Return 2 values:
- the slot name;
- the slot value.
Return NIL if not a slot setting."
  (when (and (= 3 (length sexp))
             (eq (first sexp) 'setf)
             (eq (first (second sexp)) 'slot-value)
             (eq (second (second sexp)) class-name))
    (let ((slot-name (second (third (second sexp))))
          (slot-value (third sexp)))
      (values slot-name slot-value))))

(defun write-init-form-slot (class-name slot-form)
  `(setf (slot-value ,class-name ',(name slot-form)) ,(value slot-form)))

(defun read-init-form-class (form)
  "Return:
- the class name
- the list of forms, either `slot-form' or a raw s-exp.
Return NIL if not a class form."
  (alex:when-let ((class-name (when (and (eq (first form) 'defmethod)
                                         (eq (second form) 'customize-instance))
                                (second (first (find-if #'consp form))))))
    (let ((body (alex:parse-body (sera:nlet lp ((sexp form))
                                   (if (consp (first sexp))
                                       (rest sexp)
                                       (lp (rest sexp))))
                                 :documentation t)))
      (values class-name
              (mapcar (lambda (sexp)
                        (multiple-value-bind (name value)
                            (read-init-form-slot class-name sexp)
                          (if name
                              (make-instance 'slot-form
                                             :name name
                                             :value value)
                              sexp)))
                      body)))))

(defun write-init-form-class (class-form)
  `(defmethod customize-instance ((,(class-name class-form) ,(class-name class-form)) &key)
     ,@(mapcar (lambda (form)
                 (if (slot-form-p form)
                     (write-init-form-slot (class-name class-form) form)
                     form))
               (forms class-form))))

;; TODO: Instantiate directly in read-init-*?
(defmethod files:deserialize ((profile nyxt-profile) (file auto-config-file) raw-content &key)
  (flet ((make-init-form (form)
           (multiple-value-bind (name forms)
               (read-init-form-class form)
             (if name
                 (make-instance 'class-form
                                :class-name name
                                :forms forms)
                 form))))
    (mapcar #'make-init-form
            (uiop:slurp-stream-forms raw-content))))

(defmethod files:serialize ((profile nyxt-profile) (file auto-config-file) stream &key)
  (dolist (form (files:content file))
    (write
     (if (class-form-p form)
         (write-init-form-class form)
         form)
     :stream stream)
    (fresh-line stream)))

(defmethod files:write-file ((profile nyxt-profile) (file auto-config-file) &key &allow-other-keys)
  (let ((*print-case* :downcase)
        (*package* (find-package :nyxt-user)))
    (log:info "Writing auto configuration to ~s." (files:expand file))
    (call-next-method)))

(defun auto-configure (&key form class-name slot (slot-value nil slot-value-p))
  (files:with-file-content (config *auto-config-file*)
    (if class-name
        (flet ((ensure-class-form (class-name)
                 (or (when config
                       (find-if (sera:eqs class-name) (sera:filter #'class-form-p config) :key #'class-name))
                     (sera:lret ((form (make-instance 'class-form :class-name class-name)))
                       (alex:appendf config (list form)))))
               (ensure-slot-form (class-form slot)
                 (or (find-if (sera:eqs slot) (sera:filter #'slot-form-p (forms class-form)) :key #'name)
                     (sera:lret ((form (make-instance 'slot-form :name slot)))
                       (alex:appendf (forms class-form) (list form)))))
               (delete-slot-form (class-form slot)
                 (delete-if (sera:eqs slot) (sera:filter #'slot-form-p (forms class-form)) :key #'name)))
          (let ((class-form (ensure-class-form class-name)))
            (if slot
                (if slot-value-p
                    (sera:lret ((slot-form (ensure-slot-form class-form slot)))
                      (setf (value slot-form) slot-value))
                    (setf (forms class-form) (delete-slot-form class-form slot)))
                (alex:appendf (forms class-form) (list form)))))
        (alex:appendf config (list form))))
  (echo "Updated configuration in ~s." (files:expand *auto-config-file*)))

(export-always '%slot-value%)
(defvar %slot-value% nil
  "Holds the value of the slot being configured when in `define-configuration'.")

(export-always '%slot-default%)
(defvar %slot-default% nil
  "Holds the default value of the slot being configured when in `define-configuration'.")

(export-always 'define-configuration)
(defmacro define-configuration (classes &body slots-and-values)
  "Helper macro to customize the class slots of the CLASSES.
CLASSES is either a symbol or a list of symbols.

Classes can be modes or a one of the user-configurable classes like `browser',
`buffer', `prompt-buffer', `window'.

SLOTS-AND-VALUES is a list of slot re-definitions, optionally preceded by a
docstring. The `%slot-default%' variable is replaced by the slot initform, the
`%slot-value%' is replaced by the current value of the slot.

Example that sets some defaults for all buffers:

\(define-configuration (buffer web-buffer)
  \"Increase the height of the status buffer (mode line) and use VI keybindings.\"
  ((status-buffer-height (* 2 %slot-value%)
                         :doc \"Use this is status buffer is too small.\")
   (default-modes (append '(vi-normal-mode) %slot-default%)
                  :documentation \"You can use %SLOT-VALUE% instead.
This will make your config more composable.\")))

In the above, `%slot-default%' will be substituted with the return value of
`default-modes', and `%slot-value%' will be substituted with the value of
`status-buffer-height' at the moment of configuration code running.

Example to get the `blocker-mode' command to use a new default hostlists:

\(define-configuration nyxt/mode/blocker:blocker-mode
  ((nyxt/mode/blocker:hostlists (append (list *my-blocked-hosts*) %slot-default%)
                                :doc \"You have to define *my-blocked-hosts* first.\")))

To discover the default value of a slot or all slots of a class, use the
`describe-slot' or `describe-class' commands respectively."
  (alex:with-gensyms (handler hook)
    `(progn
       ,@(loop
           ;; Strip off the docstring, it's merely cosmetic
           with slots-and-values = (if (stringp (first slots-and-values))
                                       (rest slots-and-values)
                                       slots-and-values)
           for class-name in (uiop:ensure-list classes)
           ;; NOTE: `or' here because `sym:resolve-symbol' only searches through
           ;; Nyxt packages, while one may try to configure the
           ;; extension/application-specific class too. If `sym:resolve-symbol'
           ;; fails, then hope that `find-class' will either work or highlight
           ;; the problem.
           for class = (resolve-user-symbol class-name :class)
           append (loop for ((slot-name value . rest)) on (first slots-and-values)
                        ;; FIXME: It's alarming that we resolve the slot name at
                        ;; compile-time instead of run-time. Move to the handler
                        ;; body maybe?
                        for slot = (find (symbol-name slot-name) (mopu:slot-names class)
                                         :key #'symbol-name :test #'equal)
                        ;; TODO: Shall we really make the name unique?  Since we
                        ;; are configuring slots, maybe not.
                        for handler-name = (gensym (format nil "CONFIGURE-~a-~a" class slot))
                        when slot
                          collect
                        `(let ((,hook (slot-value (find-class (quote ,class)) 'nyxt::customize-hook))
                               (,handler (make-instance
                                          'hooks:handler
                                          :fn (lambda (object)
                                                ,@(when (or (getf rest :documentation)
                                                            (getf rest :doc))
                                                    (list (or (getf rest :documentation)
                                                              (getf rest :doc))))
                                                (declare (ignorable object))
                                                (setf (slot-value object (quote ,slot))
                                                      (let* ((%slot-value% (slot-value object (quote ,slot)))
                                                             (%slot-default%
                                                               ,(if (c2mop:class-finalized-p (find-class class))
                                                                    (getf (mopu:slot-properties class slot) :initform)
                                                                    (progn
                                                                      (echo-warning
                                                                       "Slot default not found for slot ~a of class ~a, falling back to its current value"
                                                                       slot class)
                                                                      '%slot-value%))))
                                                        (declare (ignorable %slot-value% %slot-default%))
                                                        ,value)))
                                          :name (quote ,handler-name))))
                           (hooks:add-hook ,hook ,handler))
                        else
                          do (log:warn "Not found slot ~a in class ~a, generating the wrapper method for configuration."
                                       slot-name class)
                          and collect `(handler-bind ((warning #'muffle-warning))
                                         (defmethod ,slot-name :around ((object ,class))
                                           (let* ((%slot-value% (call-next-method))
                                                  (%slot-default% %slot-value%))
                                             ,value))))))))


(defparameter %buffer nil)              ; TODO: Make a monad?

(export-always 'current-buffer)
(defun current-buffer (&optional window)
  "Get the active buffer for WINDOW, or the active window otherwise."
  (or %buffer
      (alex:if-let ((w (or window (current-window))))
        (active-buffer w)
        (when *browser*
          (log:debug "No active window, picking last active buffer.")
          (last-active-buffer)))))

(defmethod initialize-instance :after ((file nyxt-file) &key (profile t profile-p) &allow-other-keys)
  (declare (ignorable profile))
  (when (and (not profile-p) (current-buffer))
    (setf (slot-value file 'files:profile) (profile (current-buffer)))))

(export-always 'with-current-buffer)
(defmacro with-current-buffer (buffer &body body)
  "Execute BODY in a context in which `current-buffer' returns BUFFER."
  ;; We `unwind-protect' to restore the right buffer when nesting this macro.
  `(let ((old-%buffer %buffer))
     (if (buffer-p ,buffer)
         (unwind-protect
              (let ((%buffer ,buffer))
                ,@body)
           (setf %buffer old-%buffer))
         ;; TODO: Raise error instead?
         (log:warn "Expected buffer, got ~a" ,buffer))))

;; TODO: Disallow canceling the prompt? Allow changing order of YES and NO so
;; that one makes a conscious effort to choose a YES?
;; TODO: Add an "always (yes|no)" answers/clauses and do something with those?
;; - Remembering prompt answers in history.
;; - Serializing thing (like notification permissions) to disk.
;; - Or simply leaving the interpretation of this clause to the user.
;; But maybe that's beyond if-confirm.
(export-always 'if-confirm)
(defmacro if-confirm ((prompt &key (yes "yes" explicit-yes-p) (no "no" explicit-no-p))
                      &optional (yes-form t) no-form)
  "Ask the user for confirmation before executing either YES-FORM or NO-FORM.
YES-FORM is executed on YES answer, NO-FORM -- otherwise (including NO and
prompt cancellation).
PROMPT should evaluate to a string.

Examples:

;; Return t/nil on user decision.
\(if-confirm (\"you agree?\"))

;; Customize the yes/no answers, and get the mood of the user as boolean.
\(if-confirm ((format nil \"How are you?\") :yes \"Good!\" :no \"Don't even ask...\"))

;; Commit an action in case of yes, clean up on no
\(if-confirm (\"Overwrite the file?\" :no \"cancel\")
            (overwrite-file-because-confirmed)
            (clean-up/abort/stop))"
  `(let ((answer (handler-case
                     (prompt1
                      :prompt ,prompt
                      :sources (make-instance 'prompter:yes-no-source
                                              ,@(when explicit-yes-p
                                                  (list :yes yes))
                                              ,@(when explicit-no-p
                                                  (list :no no)))
                      :hide-suggestion-count-p t)
                   (prompt-buffer-canceled () nil))))
     (if answer
         ,yes-form
         ,no-form)))

(export-always 'reset-asdf-registries)
(defun reset-asdf-registries ()
  "Nyxt sets the ASDF registries to its own location.
Call this function from your initialization file to re-enable the default ASDF registries."
  (setf asdf:*default-source-registries*
        '(nyxt-source-registry
          ;; Default value:
          asdf/source-registry:environment-source-registry
          asdf/source-registry:user-source-registry
          asdf/source-registry:user-source-registry-directory
          asdf/source-registry:default-user-source-registry
          asdf/source-registry:system-source-registry
          asdf/source-registry:system-source-registry-directory
          asdf/source-registry:default-system-source-registry))
  (asdf:clear-configuration))

(defun set-as-default-browser (&key (name "nyxt")
                                 (targets
                                  (list (uiop:xdg-config-home "mimeapps.list")
                                        (uiop:xdg-data-home "applications/mimeapps.list"))))
  "Return the modified MIME apps list.
Return the persisted file as second value."
  #+(and unix (not darwin))
  (let* ((target (or (first (sera:filter #'uiop:file-exists-p targets))
                     (first targets)))
         (config (py-configparser:read-files (py-configparser:make-config)
                                             (list target)))
         (desktop-file (uiop:strcat name ".desktop")))
    (dolist (section '("Added Associations" "Default Applications"))
      (dolist (key '("text/html"
                     "text/gemini"
                     "x-scheme-handler/http"
                     "x-scheme-handler/https"
                     "x-scheme-handler/chrome"
                     "application/x-extension-htm"
                     "application/x-extension-html"
                     "application/x-extension-shtml"
                     "application/xhtml+xml"
                     "application/x-extension-xhtml"
                     "application/x-extension-xht"))
        (py-configparser:set-option config section key desktop-file)))
    (with-open-file (s target
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (py-configparser:write-stream config s))

    (values config target))
  #-(and unix (not darwin))
  (progn
    (declare (ignore name targets))
    (log:warn "Only supported on GNU / BSD systems running XDG-compatible desktop environments.")))


;; TODO: Report compilation errors.

(export-always 'nyxt-user-system)
(defclass nyxt-user-system (asdf:system)
  ;; We cannot use :pathname because ASDF forces its value.
  ((config-directory
    :initarg :config-directory
    :initform nil
    :accessor config-directory))
  (:documentation "Specialized systems for Nyxt users.
This automatically defaults :pathname to the `*config-file*' directory unless
overridden by the `:config-directory' option.
See `define-nyxt-user-system' and `define-nyxt-user-system-and-load'."))

(defvar *nyxt-user-systems-with-missing-dependencies* '())

(defmethod asdf:component-pathname ((system nyxt-user-system))
  "Default to `config-directory-file'."
  (or (config-directory system)
      (files:expand (make-instance 'config-directory-file))) )

(export-always 'load-system*)
(defun load-system* (system &rest keys &key force force-not verbose version &allow-other-keys)
  "Like `asdf:load-system' but, instead of signaling an error on missing
dependency, it warns the user, skips the load gracefully and returns NIL.

When loading succeeds, it goes through the list of all the systems that failed
to load and attempts to load them if their dependencies now seem to be met."
  ;; TODO: Ideally we would make this the default behavior of
  ;; `nyxt-user-system' by specializing a method Unfortunately
  ;; `resolve-dependency-name' is a function and `find-component' is called
  ;; against the `depends-on' element but not the system itself.
  (declare (ignore force force-not verbose version))
  (block done
    (flet ((report (c)
             (pushnew (asdf:coerce-name system) *nyxt-user-systems-with-missing-dependencies*
                      :test #'string=)
             (log:warn "Could not load system ~a: ~a" system c)
             (return-from done nil)))
      (handler-bind ((asdf:missing-dependency #'report)
                     (asdf:missing-dependency-of-version #'report))
        (prog1 (apply #'asdf:load-system system keys)
          (alex:removef *nyxt-user-systems-with-missing-dependencies*
                        system
                        :test #'string=)
          (dolist (system *nyxt-user-systems-with-missing-dependencies*)
            (when (every (rcurry #'asdf:find-system nil) (asdf:system-depends-on (asdf:find-system system)))
              (log:info "Load system ~s" system)
              (load-system* system))))))))

(defun ensure-component (component-designator)
  (if (consp component-designator)
      component-designator
      (list :file (sera:drop-suffix ".lisp" component-designator :test #'string-equal))))

(asdf:defsystem "nyxt-user") ; Dummy parent needs to exist for `define-nyxt-user-system' to define subsystems.

(export-always 'define-nyxt-user-system)
(defmacro define-nyxt-user-system (name &rest args &key depends-on components
                                   &allow-other-keys)
  "Define a user system, usually meant to load configuration files.
Example to load the \"my-slynk-config\" file in your configuration directory.

  (define-nyxt-user-system nyxt-user/slynk
    :components (\"my-slynk-config\"))
  (asdf:load-system :nyxt-user/slynk)

See also `define-nyxt-user-system-and-load'.

It catches potential load dependency cycles.

Arguments are the same as for `asdf:defsystem'.
For convenience, we also support `string's or `pathname's directly in COMPONENTS.
So instead of

:components `((:file \"foo\")
              (:file #p\"bar\"))

you can write

:components `(\"foo\" #p\"bar\")

It only works for top-level components, so if you introduce a module you'll have
to use the full syntax.

To change the base directory, pass the `:config-directory' option."
  ;; We specify DEPENDS-ON to emphasize its availability.
  (declare (ignore depends-on))
  (unless (sera:string-prefix-p "nyxt-user/" (string name) )
    (error "User system name must start with 'nyxt-user/'."))
  ;; We cannot call `make-instance 'asdf:system' because we need to register the
  ;; system, and `register-system' is unexported.
  `(asdf:defsystem ,name
     :class nyxt-user-system
     ,@(uiop:remove-plist-key :components args)
     :components ,(mapcar #'ensure-component
                          components)))

(export-always 'define-nyxt-user-system-and-load)
(defmacro define-nyxt-user-system-and-load (name &rest args &key depends-on components
                                            &allow-other-keys)
  "Like `define-nyxt-user-system' but schedule to load the system when all
DEPENDS-ON packages are loaded.
If they already are, load the system now.
Return the system."
  ;; We specify DEPENDS-ON and COMPONENTS to emphasize their availability.
  (declare (ignore depends-on components))
  `(prog1 (define-nyxt-user-system ,name ,@args)
     (load-system* ',name)))
