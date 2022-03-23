;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class init-file (nfiles:config-file nyxt-lisp-file nfiles:virtual-file)
  ((nfiles:base-path #p"init")
   (command-line-option :init
                        :accessor nil
                        :type keyword))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class auto-init-file (init-file) ; TODO: Be consistent here for 3.0!
  ((nfiles:base-path #p"auto-config")
   (command-line-option :auto-init
                        :accessor nil
                        :type keyword))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:resolve ((profile nyxt-profile) (init-file init-file))
  (let* ((option (slot-value init-file 'command-line-option))
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

(export-always '*auto-config-file*)
(defvar *auto-config-file* (make-instance 'auto-init-file)
  "The generated configuration file.")

(export-always '*init-file*)
(defvar *init-file* (make-instance 'init-file)
  "The initialization file.")

(define-class extensions-directory (nfiles:data-file nyxt-file)
  ((nfiles:base-path #p"extensions/")
   (nfiles:name "extensions"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always '*extensions-directory*)
(defvar *extensions-directory* (make-instance 'extensions-directory)
  "The directory where extensions are stored.
This is set globally so that extensions can be loaded even if there is no
`*browser*' instance.")

(export-always 'nyxt-source-registry)
(defun nyxt-source-registry ()
  `(:source-registry
    (:tree ,(nfiles:expand *extensions-directory*))
    :inherit-configuration))

(defparameter %buffer nil)              ; TODO: Make a monad?

(export-always 'current-buffer)
(defun current-buffer (&optional window)
  "Get the active buffer for WINDOW, or the active window otherwise."
  (or %buffer
      (alex:if-let ((w (or window (current-window))))
        (active-buffer w)
        (if *browser*
            (progn 
              (log:debug "No active window, picking last active buffer.")
              (or (last-active-buffer)
                  (error 'no-current-buffer :message "No active buffer.")))
            (error 'no-current-buffer :message "No active buffer. No *browser*")))))

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

(defun user-class-name (class-sym)
  (intern (str:concat "USER-" (string class-sym))
          (symbol-package class-sym)))

(defvar *user-classes* (make-hash-table)
  "Keys are the user class symbols (without the 'USER-' prefix), values their
superclasses as specified by `define-user-class'.

In particular, generated user classes from `define-configuration' are not
included.")

(export-always 'define-user-class)
(defmacro define-user-class (name &optional superclasses)
  "Define the user class of NAME.
This helper function is useful to compose the customizations of a class.

The resulting class is named with the return value of (user-class-name NAME).

This may be called multiple times.
NAME must be an existing class.
NAME is automatically appended to SUPERCLASSES, so that 'user-NAME' inherits
from NAME last."
  `(set-user-class ',name ',superclasses :register-p))

(defun set-user-class (class-sym &optional superclasses register-p)
  "See `define-user-class'
When register-p is nil, does not register in `*user-classes*'.
This is useful for local changes to a class, or to add generated superclasses."
  (let ((user-class-name (user-class-name class-sym))
        (superclasses-with-original (remove-duplicates
                                     (append superclasses (list class-sym)))))
    (progn
      (export-always user-class-name (symbol-package user-class-name))
      ;; Probably no need to call the defclass macro if we just need to
      ;; set the superclasses.
      (when (and register-p (hash-table-p *user-classes*))
        (setf (gethash class-sym *user-classes*) superclasses-with-original))
      (closer-mop:ensure-class user-class-name
                               :direct-superclasses superclasses-with-original
                               :documentation (documentation class-sym 'type)))))

(defun reset-user-class (class-sym)
  (set-user-class class-sym (gethash class-sym *user-classes*)))

(defun reset-all-user-classes ()
  (mapc #'reset-user-class (alex:hash-table-keys *user-classes*)))

(defun user-class-p (class-specifier)
  (sera:true (gethash (if (symbolp class-specifier)
                          class-specifier
                          (class-name class-specifier))
                      *user-classes*)))

(defmacro with-user-class ((class-sym new-superclasses) &body body) ; TODO: Export if users ever demand it.
  "Dynamically override the superclasses of the user class corresponding to
CLASS-SYM to NEW-SUPERCLASSES.  The class is restored when exiting BODY."
  ;; Test:
  ;; (with-user-class (buffer (buffer))
  ;;   (mopu:direct-superclasses 'user-buffer))
  (let ((user-class (user-class-name class-sym)))
    (unless (user-class-p user-class)
      (error "Argument must be a user class (see `user-class-p')."))
    (let ((old-superclasses (mapcar #'class-name (mopu:direct-superclasses user-class))))
      `(unwind-protect
            (progn
              (set-user-class ',class-sym ',new-superclasses)
              ,@body)
         (set-user-class ',class-sym ',old-superclasses)))))

(-> method-combination-name ((or symbol function)) *)
(defun method-combination-name (fun)
  (let ((fun (if (functionp fun)
                 fun
                 (symbol-function fun))))
    (funcall
     #+sbcl
     'sb-pcl::method-combination-type-name
     #+ccl
     'ccl::method-combination-name
     #-(or sbcl ccl)
     (error "Not implemented")
     (closer-mop:generic-function-method-combination fun))))

(-> standard-method-combination-p ((or symbol function)) boolean)
(defun standard-method-combination-p (fun)
  (eq 'standard
      (method-combination-name fun)))

(export-always '%slot-default%)
(defmacro %define-configuration (name &body slots)
  (let* ((final-name (user-class-name name))
         (temp-name (gentemp (string final-name) (symbol-package name))))
    (dolist (name (list name final-name))
      (unless (find-class name nil)
        (error "define-configuration argument ~a is not a known class." name)))
    `(progn
       (define-class ,temp-name ()
         ,(let ((super-class (closer-mop:ensure-finalized (find-class final-name))))
            (loop for slot in (sera:filter #'standard-method-combination-p (first slots)
                                           :key #'first)
                  for known-slot? = (find (first slot) (mopu:slot-names super-class))
                  for initform = (and known-slot?
                                      (getf (mopu:slot-properties super-class (first slot))
                                            :initform))
                  if known-slot?
                    collect (list (first slot)
                                  :initform `(funcall (lambda (%slot-default%)
                                                        (declare (ignorable %slot-default%))
                                                        ,(cadr slot))
                                                      ,initform))
                  else do
                    (log:warn "Undefined slot ~a in ~a" (first slot) final-name)))
         (:accessor-name-transformer (class*:make-name-transformer name)))
       ;; TODO: Register the user methods and add function to remove them, like
       ;; `reset-user-class'.
       ;; Non-standard accessors, e.g. `default-modes':
       ,@(loop for slot in (remove-if #'standard-method-combination-p (first slots)
                                      :key #'first)
               for slot-name = (first slot)
               collect
               `(defmethod ,slot-name :around ((,(user-class-name name) ,(user-class-name name)))
                  (funcall (lambda (%slot-default%)
                             (declare (ignorable %slot-default%))
                             ,(cadr slot))
                           (call-next-method))))
       (set-user-class ',name ',(cons temp-name
                                      (mapcar #'class-name
                                              (mopu:direct-superclasses final-name)))))))

(defun get-initform (class-symbol class-slot)
  (getf (mopu:slot-properties (find-class class-symbol) class-slot) :initform))

(export-always 'define-configuration)
(defmacro define-configuration (names &body slots)
  "Helper macro to customize the class slots of the NAMES classes.
NAMES is either a symbol or a list of symbols.

Classes can be modes or a one of the user-configurable classes like `browser',
`buffer', `prompt-buffer', `window'.  Note that the classes must _not_ be prefixed
by 'user-'.

The `%slot-default%' variable is replaced by the slot initform.

Example that sets some defaults for all buffers:

\(define-configuration (buffer web-buffer)
  ((status-buffer-height 24)
   (default-modes (append '(vi-normal-mode) %slot-default%))))

Example to get the `blocker-mode' command to use a new default hostlists:

\(define-configuration nyxt/blocker-mode:blocker-mode
  ((nyxt/blocker-mode:hostlists (append (list *my-blocked-hosts*) %slot-default%))))

In the above, `%slot-default%' will be substituted with the return value of
`default-modes'.

In the last example, `nyxt/blocker-mode:user-blocker-mode' is defined to inherit
from the original `blocker-mode' and a generated class containing the
specialized hostlists.

To discover the default value of a slot or all slots of a class, use the
`describe-slot' or `describe-class' commands respectively."
  (if (listp names)
      `(progn
         ,@(mapcar (lambda (name)
                     `(%define-configuration ,name ,@slots))
                   names))
      `(%define-configuration ,names ,@slots)))

(export-always 'if-confirm)
(defmacro if-confirm (prompt yes-form &optional no-form)
  "Ask the user for confirmation before executing either YES-FORM or NO-FORM.
YES-FORM is executed on  \"yes\" answer, NO-FORM -- on \"no\".
PROMPT is a list fed to `format nil'."
  `(let ((answer (first (handler-case
                            (prompt
                             :prompt (format nil ,@prompt)
                             :sources '(prompter:yes-no-source)
                             :hide-suggestion-count-p t)
                          (nyxt-prompt-buffer-canceled (c) (declare (ignore c)) '("no"))))))
     (if (string= "yes" answer)
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

(export-always 'load-after-system)
(defun load-after-system (system &optional file)
  "Load Common Lisp SYSTEM, then on success load FILE.
Use Quicklisp if possible.
See also `reset-asdf-registries'.

Example:

  (load-after-system :xyz \"configure-xyz.lisp\")"
  (flet ((load-system (system)
           (handler-case
               (progn
                 #+quicklisp
                 (ql:quickload system :silent t)
                 #-quicklisp
                 (asdf:load-system system))
             (error (c)
               (echo-warning "Could not load system ~a: ~a" system c)
               (log:warn "Maybe you want to use `reset-asdf-registries'?")
               (log:warn "Current ASDF registries: ~{~a~^, ~}"
                         asdf:*default-source-registries*)
               nil))))
    (when (and (load-system system) file)
      (load file))))

(defun make-ring (&key (size 1000))
  "Return a new ring buffer."
  (containers:make-ring-buffer size :last-in-first-out))

(export-always 'trim-list)
(defun trim-list (list &optional (limit 100))
  (handler-case
      (if (< limit (length list))
          (nconc (sera:nsubseq list 0 (1- limit)) (list "…"))
          list)
    (error ()
      ;; Improper list.
      list)))

(defun public-initargs (class-specifier)
  (delete-if (lambda (name) (eq :internal (nth-value 1 (find-symbol (string name)
                                                                    (symbol-package name)))))
             (mopu:direct-slot-names class-specifier)))

(export-always 'funcall*)
(defun funcall* (f &rest args)
  "Like `funcall' but does nothing when F is nil."
  (when f (apply #'funcall f args)))

(defun ensure-file-exists (pathname)
  (open pathname :direction :probe :if-does-not-exist :create))

(export-always 'destroy-thread*)
(defun destroy-thread* (thread)
  "Like `bt:destroy-thread' but does not raise an error.
Particularly useful to avoid errors on already terminated threads."
  (ignore-errors (bt:destroy-thread thread)))

(export-always 'on)
(defmacro on (hook args &body body)
  "Attach a handler with ARGS and BODY to the HOOK.

ARGS can be
- A symbol if there's only one argument to the callback.
- A list of arguments.
- An empty list, if the hook handlers take no argument."
  (let ((handler-name (gensym "on-hook-handler"))
        (args (alex:ensure-list args)))
    `(hooks:add-hook
      ,hook (make-instance 'hooks:handler
                           :fn (lambda ,args
                                 (declare (ignorable ,@args))
                                 ,@body)
                           :name (quote ,handler-name)))))

(export-always 'once-on)
(defmacro once-on (hook args &body body)
  "Attach a handler with ARGS and BODY to the HOOK.

Remove the handler after it fires the first time.

See `on'."
  (let ((handler-name (gensym "once-on-hook-handler"))
        (args (alex:ensure-list args)))
    (alex:once-only (hook)
      `(hooks:add-hook
        ,hook (make-instance 'hooks:handler
                             :fn (lambda ,args
                                   (declare (ignorable ,@args))
                                   (hooks:remove-hook ,hook (quote ,handler-name))
                                   ,@body)
                             :name (quote ,handler-name))))))
