;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class config-directory-file (files:config-file nyxt-file)
  ((files:base-path #p"")
   (command-line-option :config
                        :accessor nil
                        :type keyword))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class config-file (config-directory-file files:virtual-file nyxt-lisp-file)
  ((files:base-path #p"config")
   (command-line-option :config
                        :accessor nil
                        :type keyword))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class slot-form ()
  ((name nil
         :type symbol)
   (value nil
         :type t))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class class-form ()
  ((class-name nil
               :type symbol)
   (forms '()
          :type (maybe (cons (or cons slot-form) *))))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class auto-config-file (config-directory-file nyxt-lisp-file)
  ((files:base-path #p"auto-config")
   (command-line-option :auto-config
                        :accessor nil
                        :type keyword))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod files:resolve ((profile nyxt-profile) (config-file config-directory-file))
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

(define-class nyxt-source-directory (nyxt-file)
  ((files:base-path asdf-user::*dest-source-dir*)
   (files:name "source"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always '*source-directory*)
(defvar *source-directory* (make-instance 'nyxt-source-directory)
  "The directory where the source code is stored.
This is set globally so that it can be looked up if there is no
`*browser*' instance.")

(define-class extensions-directory (files:data-file nyxt-file)
  ((files:base-path #p"extensions/")
   (files:name "extensions"))
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
    (:tree ,(files:expand *extensions-directory*))
    (:tree ,(files:expand *source-directory*))
    :inherit-configuration))

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
  (echo "Updated configuration in ~s." (nfiles:expand *auto-config-file*)))

(export-always '%slot-value%)
(defvar %slot-value% nil
  "Holds the value of the slot being configured when in `define-configuration'.")

(export-always '%slot-default%)
(defvar %slot-default% nil
  "Holds the default value of the slot being configured when in `define-configuration'.")

(export-always 'define-configuration)
(defmacro define-configuration (classes &body slots-and-values)
  `(progn
     ,@(loop
         for class in (uiop:ensure-list classes)
         for handler-name = (gensym "DEFINE-CONFIGURATION")
         collect
         `(hooks:add-hook
           (slot-value (find-class (quote ,class)) 'nyxt::customize-hook)
           (make-instance
            'hooks:handler
            :fn (lambda (object)
                  (declare (ignorable object))
                  ,@(loop for ((slot value)) on (first slots-and-values)
                          when (find slot (mopu:slot-names class))
                            collect `(setf (slot-value object (quote ,slot))
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
                                             ,value))
                          else
                            collect `(defmethod ,slot :around ((object ,class))
                                       (let* ((%slot-value% (call-next-method))
                                              (%slot-default% %slot-value%))
                                         ,value))))
            :name (quote ,handler-name))))))


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

(export-always 'last-word)
(defun last-word (s)
  (if (uiop:emptyp s)
      ""
      (alex:last-elt (sera:words s))))

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

(defun guess-external-format (filename)
  (or (swank-backend:guess-external-format filename)
      (swank-backend:find-external-format "latin-1")
      :default))

(defun function-lambda-string (fun)
  "Like `function-lambda-expression' for the first value, but return a string.
On failure, fall back to other means of finding the source.
Return the lambda s-expression as a second value, if possible."
  (alex:if-let ((expression (when (functionp fun) (function-lambda-expression fun))))
    (values (let ((*print-case* :downcase)
                  (*print-pretty* t))
              (write-to-string expression))
            expression)
    (sera:and-let* ((definition (rest (swank:find-definition-for-thing fun)))
                    (*package* (symbol-package (swank-backend:function-name
                                                (if (functionp fun)
                                                    fun
                                                    (closer-mop:method-generic-function fun)))))
                    (file (first (alexandria:assoc-value definition :file)))
                    (file-content (alexandria:read-file-into-string
                                   file
                                   :external-format (guess-external-format file)))
                    (start-position (first (alexandria:assoc-value definition :position))))
      (restart-case
          (handler-bind ((reader-error (lambda (c)
                                         (declare (ignore c))
                                         (invoke-restart 'use-value
                                                         (str:trim-right
                                                          (subseq file-content
                                                                  (1- start-position)
                                                                  (search (uiop:strcat +newline+ "(") file-content :start2 start-position)))))))
            (let ((*read-eval* nil))
              (let ((expression (read-from-string file-content t nil
                                                  :start (1- start-position))))
                (values (let ((*print-case* :downcase)
                              (*print-pretty* t))
                          (write-to-string expression))
                        expression))))
        (use-value (arg)
          arg)))))

(export-always 'defmemo)
(defmacro defmemo (name params &body body) ; TODO: Replace with https://github.com/AccelerationNet/function-cache?
  (multiple-value-bind (required optional rest keyword)
      (alex:parse-ordinary-lambda-list params)
    (alex:with-gensyms (memo-table args result result?)
      `(let ((,memo-table (make-hash-table :test 'equal)))
         (defun ,name (,@params)
           (let ((,args (append (list ,@required)
                                (list ,@optional)
                                ,rest
                                (list ,@(alex:mappend #'first keyword)))))
             (multiple-value-bind (,result ,result?)
                 (gethash ,args ,memo-table)
               (if ,result?
                   (progn
                     (log:info t "Got a cache match!")
                     ,result)
                   (setf (gethash ,args ,memo-table)
                         (apply (lambda ,params
                                  ;; This block is here to catch the return-from
                                  ;; FUNCTION-NAME and cache it too.
                                  ;;
                                  ;; TODO: Better way? Maybe use methods and
                                  ;; :around qualifiers?
                                  (block ,name ,@body))
                                ,args))))))))))
