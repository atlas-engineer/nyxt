;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; TODO: Leverage `activate'.
;; TODO: What to do of `make-mode'?  Shall we enable modes on instantiation?

(defclass mode-class (user-class)
  ((toggler-command-p
    :initform t
    :initarg :toggler-command-p
    :type boolean
    :documentation "Whether to define a toggler command for the defined mode.")))
(export-always 'mode-class)

(defmethod closer-mop:validate-superclass ((class mode-class)
                                           (superclass user-class))
  t)

;; (defmethod initialize-instance :after ((class mode-class) &key)
;;   (when (toggler-command-p class)
;;     (define-command-global NAME (&rest args
;;                                  &key
;;                                  ;; TODO: Shall we have a function that
;;                                  ;; returns the focused buffer?
;;                                  ;; `focused-buffer'?  `current-buffer*'?
;;                                  ;; Rename `current-buffer' to
;;                                  ;; `current-view-buffer' and add
;;                                  ;; `current-buffer' for this task?
;;                                  (buffer (or (current-prompt-buffer) (current-buffer)))
;;                                  (activate t explicit?)
;;                                  &allow-other-keys)
;;       (let ((,existing-instance (find-mode buffer ',NAME)))
;;         (unless explicit?
;;           (setf activate (not ,existing-instance)))
;;         (if activate
;;             (unless ,existing-instance
;;               (enable
;;                (apply #'make-instance ',name
;;                       :buffer buffer
;;                       args)))
;;             (when ,existing-instance
;;               (disable ,existing-instance)))))))

(define-class mode ()
  ((buffer
    (current-buffer)                    ; TODO: Is this reasonable?
    :type buffer)
   (glyph
    nil
    :type (maybe string)
    :accessor nil
    :documentation "A glyph used to represent this mode, if unset, it will
be dynamically calculated as the first letters of the mode name.")
   (visible-in-status-p
    t
    :documentation "Whether the mode is visible in the status line.")
   (rememberable-p
    t
    :documentation "Whether this mode is visible to `auto-mode'.")
   ;; (activate :accessor activate :initarg :activate)
                                        ; TODO: This can be used in the future to temporarily turn off modes without destroying the object.
   (enabled-p
    nil
    :accessor nil
    :documentation "Whether the mode is enabled in `buffer'.")
   (enable-hook
    (make-instance 'hook-mode)
    :type hook-mode
    :documentation "This hook is run when enabling the mode.
It takes the mode as argument
It is run before the destructor.")
   (disable-hook
    (make-instance 'hook-mode)
    :type hook-mode
    :documentation "This hook is run when disabling the mode.
It takes the mode as argument.
It is run before the destructor.")
   (keymap-scheme
    (make-hash-table :size 0)
    :type keymap:scheme))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass mode-class))

(export-always 'mode-name)
(defun mode-name (mode)
  "Return the full MODE symbol (with package prefix).
If MODE does not exist, return nil."
  (when (or (mode-p mode)
            (and (find-class mode)
                 (closer-mop:subclassp (find-class mode)
                                       (find-class 'mode))))
    (sera:class-name-of mode)))

(defmethod initialize-instance :after ((mode mode) &key)
  (when (eq 'mode (type-of mode))
    (error "Cannot initialize `mode', you must subclass it.")))

(defmethod customize-instance :after ((mode mode) &key)
  ;; TODO: Should we move mode to the front when it already exists?
  (push mode (modes (buffer mode))))

(export-always 'enable)
(defgeneric enable (mode &key &allow-other-keys)
  (:method ((mode mode) &key)
    nil)
  (:documentation "Run when enabling a mode.
The pre-defined `:after' method handles further setup."))

(defmethod enable :after ((mode mode) &key)
  (hooks:run-hook (enable-hook mode) mode)
  (let ((buffer (buffer mode)))
    (hooks:run-hook (enable-mode-hook buffer) mode)
    (if (and (prompt-buffer-p buffer)
             (eq (first (active-prompt-buffers (window buffer)))
                 buffer))
        (prompt-render-prompt buffer)
        (print-status)))
  (log:debug "~a enabled." (mode-name mode)))

(export-always 'disable)
(defgeneric disable (mode &key &allow-other-keys)
  (:method ((mode mode) &key)
    nil)
  (:documentation "Run when disabling a mode.
The pre-defined `:after' method handles further cleanup."))

(defmethod disable :after ((mode mode) &key)
  (hooks:run-hook (disable-hook mode) mode)
  (let ((buffer (buffer mode)))
    (hooks:run-hook (disable-mode-hook (buffer mode)) mode)
    ;; TODO: Remove from list or not?
    ;; (setf (modes buffer) (delete ,existing-instance (modes buffer)))
    (if (and (prompt-buffer-p buffer)
             (eq (first (active-prompt-buffers (window buffer)))
                 buffer))
        (prompt-render-prompt buffer)
        (print-status)))
  (log:debug "~a disabled." (mode-name name)))

(export-always 'define-mode)
(defmacro define-mode (name direct-superclasses &body body)
  "Shorthand to define a mode.  It has the same syntax as `define-class' but
optionally accepts a docstring after the superclass declaration.
The `mode' superclass is automatically added if not present."
  (let* ((docstring (when (stringp (first body))
                      (first body)))
         (body (if docstring
                   (rest body)
                   body))
         (direct-slots (first body))
         (options (rest body)))
    `(define-class ,name (,@(append direct-superclasses
                                    (unless (find 'mode direct-superclasses) '(mode))))
       ,direct-slots
       ,@(append options
                 (when docstring
                   `((:documentation ,docstring)))
                 `((:export-class-name-p t)
                   (:export-accessor-names-p t)
                   (:export-predicate-name-p t)
                   (:accessor-name-transformer (class*:make-name-transformer name))
                   (:metaclass mode-class))))))

(hooks:define-hook-type mode (function (mode)))

(defmethod prompter:object-attributes ((mode mode))
  `(("Name" ,(princ-to-string (mode-name mode)))))

(export-always 'glyph)
(defmethod glyph ((mode mode))
  "Return the glyph for a mode, or if unset, return a standard formatted mode."
  (or (slot-value mode 'glyph)
      (format-mode mode)))

(defmethod (setf glyph) (glyph (mode mode))
  (setf (slot-value mode 'glyph) glyph))

(defmethod format-mode ((mode mode))
  "Produce a string representation of a mode suitable for use in a status
  buffer."
  (str:replace-all "-mode" "" (str:downcase (mode-name mode))))

(export-always 'find-mode)
(defmethod find-mode ((buffer buffer) mode-symbol)
  "Return the mode corresponding to MODE-SYMBOL in active in BUFFER.
Return nil if mode is not found.  MODE-SYMBOL does not have to be namespaced, it
can be 'web-mode as well as 'nyxt/web-mode:web-mode."
  (alex:when-let ((mode-full-symbol (mode-name mode-symbol)))
    (find mode-full-symbol
          (modes buffer)
          :key #'sera:class-name-of)))

(export-always 'find-submode)
(defmethod find-submode ((buffer buffer) mode-symbol)
  "Like `find-mode' but return the first mode in BUFFER that is a sub-mode of MODE-SYMBOL.
It may be MODE-SYMBOL itself."
  (alex:when-let ((mode-full-symbol (mode-name mode-symbol)))
    (find-if (lambda (m)
               (closer-mop:subclassp (class-of m)
                                     (find-class mode-full-symbol)))
             (modes buffer))))

(export-always 'current-mode)
(defun current-mode (mode-sym)
  "Return mode instance of MODE-SYM in current buffer.
Return NIL if none.
The \"-mode\" suffix is automatically appended to MODE-SYM if missing."
  (find-submode (current-buffer)
                (let ((name (string mode-sym)))
                  (if (str:ends-with-p "-mode" name :ignore-case t)
                      mode-sym
                      (intern (str:concat name "-MODE")
                              (symbol-package mode-sym))))))

(defun mode-list ()
  "Return the list of all namespaced mode symbols."
  (mopu:subclasses 'mode))

(defun make-mode (mode-symbol buffer)
  ""
  (if (mode-name mode-symbol)
      (enable (or (find-mode buffer mode-symbol)
                  (make-instance mode-symbol :buffer buffer)))
      (log:warn "Mode command ~a not found." mode-symbol)))

(export-always 'find-buffer)
(defun find-buffer (mode-symbol)
  "Return first buffer matching MODE-SYMBOL."
  (find-if (lambda (b)
             (find-mode b mode-symbol))
           (buffer-list)))

(export-always 'keymap)
(defmethod keymap ((mode mode))
  "Return the keymap of MODE according to its buffer keymap scheme.
If there is no corresponding keymap, return nil."
  (keymap:get-keymap (if (buffer mode)
                         (keymap-scheme-name (buffer mode))
                         scheme:cua)
                     (keymap-scheme mode)))

(defmethod on-signal-notify-uri ((mode mode) url)
  (set-window-title)
  (print-status)
  url)

(defmethod on-signal-notify-title ((mode mode) title)
  (on-signal-notify-uri mode (url (buffer mode)))
  title)

(defmethod on-signal-load-started ((mode mode) url)
  url)

(defmethod on-signal-load-redirected ((mode mode) url)
  url)

(defmethod on-signal-load-committed ((mode mode) url)
  url)

(defmethod on-signal-load-finished ((mode mode) url)
  url)

(defmethod s-serialization:serializable-slots ((object mode))
  "Discard keymaps which can be quite verbose."
  (delete 'keymap-scheme
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of object)))))
