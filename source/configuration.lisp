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

(define-class auto-init-file (nfiles:config-file nyxt-lisp-file) ; TODO: Be consistent here for 3.0!
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

(define-class nyxt-source-directory (nyxt-file)
  ((nfiles:base-path asdf-user::*dest-source-dir*)
   (nfiles:name "source"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always '*source-directory*)
(defvar *source-directory* (make-instance 'nyxt-source-directory)
  "The directory where the source code is stored.
This is set globally so that it can be looked up if there is no
`*browser*' instance.")

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
    (:tree ,(nfiles:expand *source-directory*))
    :inherit-configuration))



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
(defmethod nfiles:deserialize ((profile nyxt-profile) (file auto-init-file) raw-content &key)
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

(defmethod nfiles:serialize ((profile nyxt-profile) (file auto-init-file) stream &key)
  (dolist (form (nfiles:content file))
    (write
     (if (class-form-p form)
         (write-init-form-class form)
         form)
     :stream stream)
    (fresh-line stream)))

(defmethod nfiles:write-file ((profile nyxt-profile) (file auto-init-file) &key)
  (let ((*print-case* :downcase)
        (*package* (find-package :nyxt-user)))
    (log:info "Writing auto configuration to ~s." (nfiles:expand file))
    (call-next-method)))

(defun auto-configure (&key form class-name slot (slot-value nil slot-value-p))
  (nfiles:with-file-content (config *auto-config-file*)
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
        (alex:appendf config (list form)))))



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
