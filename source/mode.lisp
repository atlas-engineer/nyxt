;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defclass mode-class (user-class)
  ((toggler-command-p                   ; TODO: Rename to `togglable-p'?
    :initform (list t)
    :initarg :toggler-command-p
    :type (cons boolean null)
    :documentation "Whether to define a toggler command for the defined mode.")))
(export-always 'mode-class)

(defmethod closer-mop:validate-superclass ((class mode-class)
                                           (superclass user-class))
  t)

(defun define-or-undefine-command-for-mode (class)
  (let ((name (class-name class)))
    ;; FIXME: SBCL `slot-value' returns a list, while CCL returns the boolean.  Why?
    (if (alex:ensure-car (slot-value class 'toggler-command-p))
        (sera:lret ((command (make-command
                              name
                              `(lambda (&rest args
                                        &key (buffer (or (current-prompt-buffer) (current-buffer)))
                                          (activate t explicit?)
                                        &allow-other-keys)
                                 ,(format nil "Toggle ~a." name)
                                 (declare (ignorable buffer activate explicit?))
                                 (apply #'toggle-mode ',name args))
                              :global)))
          (setf (fdefinition name) command))
        (delete-command name))))

(defmethod initialize-instance :after ((class mode-class) &key)
  (define-or-undefine-command-for-mode class))

(defmethod reinitialize-instance :after ((class mode-class) &key)
  (define-or-undefine-command-for-mode class))

(define-class mode ()
  ((buffer
    nil
    :type (maybe null buffer))
   (glyph
    nil
    :type (maybe string)
    :accessor nil
    :documentation "A glyph used to represent this mode.")
   (visible-in-status-p
    t
    :documentation "Whether the mode is visible in the status line.")
   (rememberable-p
    t
    :documentation "Whether this mode is visible to `auto-mode'.")
   (enabled-p
    nil
    :accessor nil
    :reader t
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
  (:toggler-command-p nil)
  (:metaclass mode-class))

(defmethod initialize-instance :after ((mode mode) &key)
  (when (eq 'mode (type-of mode))
    (error "Cannot initialize `mode', you must subclass it.")))

(defmethod name ((mode mode))
  (sera:class-name-of mode))

(export-always 'enable)
(defgeneric enable (mode &key &allow-other-keys)
  (:method ((mode mode) &key)
    nil)
  (:documentation "Run when enabling a mode.
The pre-defined `:after' method handles further setup."))

(defmethod enable :after ((mode mode) &key)
  (setf (slot-value mode 'enabled-p) t)
  (hooks:run-hook (enable-hook mode) mode)
  (let ((buffer (buffer mode)))
    ;; TODO: Should we move mode to the front when it already exists?
    (pushnew mode (slot-value (buffer mode) 'modes))
    (hooks:run-hook (enable-mode-hook buffer) mode)
    (if (and (prompt-buffer-p buffer)
             (eq (first (active-prompt-buffers (window buffer)))
                 buffer))
        (prompt-render-prompt buffer)
        (print-status))))

(export-always 'disable)
(defgeneric disable (mode &key &allow-other-keys)
  (:method ((mode mode) &key)
    nil)
  (:documentation "Run when disabling a mode.
The pre-defined `:after' method handles further cleanup."))

(defmethod disable :after ((mode mode) &key)
  (setf (slot-value mode 'enabled-p) nil)
  (hooks:run-hook (disable-hook mode) mode)
  (let ((buffer (buffer mode)))
    (hooks:run-hook (disable-mode-hook (buffer mode)) mode)
    ;; TODO: Remove from list or not?
    ;; (setf (modes buffer) (delete ,existing-instance (modes buffer)))
    (if (and (prompt-buffer-p buffer)
             (eq (first (active-prompt-buffers (window buffer)))
                 buffer))
        (prompt-render-prompt buffer)
        (print-status))))

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
    `(sera:eval-always ; Important so that classes can be found from the same file at compile-time.
       (define-class ,name (,@(append direct-superclasses
                                      (unless (find 'mode direct-superclasses) '(mode))))
         ,direct-slots
         ,@(append options
                   (when docstring
                     `((:documentation ,docstring)))
                   `((:export-class-name-p t)
                     (:export-accessor-names-p t)
                     (:export-predicate-name-p t)
                     (:accessor-name-transformer (class*:make-name-transformer name))
                     (:metaclass mode-class)))))))

(hooks:define-hook-type mode (function (mode)))

(defmethod prompter:object-attributes ((mode mode))
  `(("Name" ,(princ-to-string mode))))

(export-always 'glyph)
(defmethod glyph ((mode mode))
  "Return the glyph for a mode.
When unset, it corresponds to the mode name."
  (or (slot-value mode 'glyph)
      (princ-to-string mode)))

(defmethod (setf glyph) (glyph (mode mode))
  (setf (slot-value mode 'glyph) glyph))

(defmethod print-object ((mode mode) stream)
  (if *print-escape*
      (print-unreadable-object (mode stream :type t :identity t))
      (let ((name (symbol-name (sera:class-name-of mode)))
            (suffix "-MODE"))
        (format stream "~(~a~)" (sera:string-replace
                                 suffix name ""
                                 :start (- (length name ) (length suffix)))))))

(-> mode-class (symbol) (maybe class))
(defun mode-class (symbol)
  "Return the mode class associated to SYMBOL.
Return NIL if it's not a mode."
  (alex:when-let ((class (find-class symbol nil)))
    (when (mopu:subclassp class (find-class 'mode))
      class)))

(defun package-modes (&optional (packages (nyxt-packages))
                        (user-packages (nyxt-user-packages)))
  "Return the list of mode symbols in PACKAGES and USER-PACKAGES.
See `package-symbols' for details on the arguments."
  (delete-if (complement #'mode-class)
             (package-symbols packages user-packages)))

;; TODO: Should allow search all packages, e.g. when PACKAGES is NIL.
(-> resolve-symbol ((or keyword string) (member :function :variable :class :mode :slot :command) &optional (cons *)) symbol)
(export-always 'resolve-symbol)
(defun resolve-symbol (designator type &optional (packages (list :nyxt :nyxt-user)))
  "Find the symbol (of TYPE) designated by DESIGNATOR in PACKAGE.
PACKAGES should be a list of package designators."
  (sera:and-let* ((designator (string designator))
                  (subpackages (append
                                (mapcar #'find-package packages)
                                (sera:filter
                                 (apply #'alex:disjoin
                                        (mapcar (lambda (pkg)
                                                  (alex:rcurry #'subpackage-p (find-package pkg)))
                                                packages))
                                 (list-all-packages))))
                  (symbols (case type
                             (:function (package-functions subpackages))
                             (:variable (package-variables subpackages))
                             (:class (package-classes subpackages))
                             (:mode (package-modes subpackages))
                             (:slot (mapcar #'name (package-slots subpackages)))
                             (:command (mapcar #'name (list-commands))))))
    (let ((results (delete designator symbols :key #'symbol-name :test #'string/=)))
      (unless (sera:single results)
        (log:warn "Multiple ~a modes found: ~a" designator results))
      (values (first results)
              results))))

(deftype mode-symbol ()
  `(satisfies mode-class))

(-> find-submode (mode-symbol &optional buffer) (maybe mode))
(export-always 'find-submode)
(defun find-submode (mode-symbol &optional (buffer (current-buffer)))
  "Return the first submode instance of MODE-SYMBOL in BUFFER.
As a second value, return all matching submode instances.
Return nil if mode is not found."
  (when (modable-buffer-p buffer)
    (alex:if-let ((class (mode-class mode-symbol)))
      (let ((results (sera:filter
                      (alex:rcurry #'closer-mop:subclassp class)
                      (modes buffer)
                      :key #'class-of)))
        (when (< 1 (length results))
          (log:warn "Found multiple matching modes: ~a" results))
        (values (first results)
                results))
      ;; CCL catches the error at compile time but not all implementations do,
      ;; hence the redundant error report here.
      (error "Mode ~a does not exist" mode-symbol))))

(-> current-mode ((or keyword string) &optional buffer) (maybe mode))
(export-always 'current-mode)
(defun current-mode (mode-designator &optional (buffer (current-buffer)))
  "Return mode instance of MODE-DESIGNATOR in BUFFER.
Return NIL if none.
The \"-mode\" suffix is automatically appended to MODE-KEYWORD if missing.
This is convenience function for interactive use.
For production code, see `find-submode' instead."
  (let ((mode-designator (sera:ensure-suffix (string mode-designator) "-MODE")))
    (find-submode (resolve-symbol mode-designator :mode)
                  buffer)))

(defun all-mode-symbols ()
  "Return the list of mode symbols."
  (mapcar #'class-name (mopu:subclasses 'mode)))

(defun make-mode-suggestion (mode &optional source input)
  "Return a `suggestion' wrapping around ATTRIBUTE. "
  (declare (ignore source input))
  (make-instance 'prompter:suggestion
                 :value mode
                 :attributes `(("Mode" ,(string-downcase (symbol-name mode)))
                               ("Documentation" ,(or (first (sera:lines (documentation mode 'function)))
                                                     "")))))

(define-class mode-source (prompter:source)
  ((prompter:name "Modes")
   (prompter:multi-selection-p t)
   (prompter:constructor (sort (all-mode-symbols) #'string< :key #'symbol-name))
   (prompter:suggestion-maker 'make-mode-suggestion))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-class active-mode-source (prompter:source)
  ((prompter:name "Active modes")
   (buffers '())
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (delete-duplicates
                            (alex:mappend
                             #'modes
                             (uiop:ensure-list (buffers source)))
                            :test (lambda (i y) (eq (sera:class-name-of i)
                                                    (sera:class-name-of y)))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-class inactive-mode-source (prompter:source)
  ((prompter:name "Inactive modes")
   (buffers '())
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (let ((common-modes
                                   (reduce #'intersection
                                           (mapcar (lambda (b)
                                                     (mapcar #'sera:class-name-of (modes b)))
                                                   (uiop:ensure-list (buffers source))))))
                             (set-difference (all-mode-symbols) common-modes)))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command enable-modes (&optional modes buffers args)
  "Enable MODES for BUFFERS.
MODES should be a list of mode symbols.
BUFFERS and MODES are automatically coerced into a list.
ARGS are passed to the mode `enable' method.

If BUFFERS is a list, return it.
If it's a single buffer, return it directly (not as a list)."
  ;; TODO: Report if mode is not found.
  (let* ((buffers (if buffers
                      (uiop:ensure-list buffers)
                      (prompt
                       :prompt "Enable mode(s) for buffer(s)"
                       :sources (make-instance 'buffer-source
                                               :multi-selection-p t
                                               :return-actions '()))))
         (modes (if modes
                    (uiop:ensure-list modes)
                    (prompt
                     :prompt "Enable mode(s)"
                     :sources (make-instance 'inactive-mode-source
                                             :buffers buffers)))))
    (mapcar (lambda (buffer)
              (mapcar (lambda (mode-sym)
                        (apply #'enable (or (find mode-sym (modes buffer) :key #'name)
                                            (make-instance mode-sym :buffer buffer))
                               args))
                      modes))
            (sera:filter #'modable-buffer-p buffers)))
  buffers)

(define-command disable-modes (&optional modes buffers)
  "Disable MODES for BUFFERS.
MODES should be a list of mode symbols.
BUFFERS and MODES are automatically coerced into a list.

If BUFFERS is a list, return it.
If it's a single buffer, return it directly (not as a list)."
  ;; TODO: Report if mode is not found.
  (let* ((buffers (if buffers
                      (uiop:ensure-list buffers)
                      (prompt
                       :prompt "Enable mode(s) for buffer(s)"
                       :sources (make-instance 'buffer-source
                                               :multi-selection-p t
                                               :return-actions '()))))
         (modes (if modes
                    (uiop:ensure-list modes)
                    (prompt
                     :prompt "Enable mode(s)"
                     :sources (make-instance 'inactive-mode-source
                                             :buffers buffers)))))
    (mapcar (lambda (buffer)
              (mapcar #'disable (delete nil (mapcar (lambda (mode) (find mode (modes buffer) :key #'name))
                                                    (uiop:ensure-list modes)))))
            buffers))
  buffers)

;; TODO: Factor `toggle-mode' and `toggle-modes' somehow?
;; TODO: Shall we have a function that returns the focused buffer?
;; `focused-buffer'?  `current-buffer*'?  Rename `current-buffer' to
;; `current-view-buffer' and add `current-buffer' for this task?
(defun toggle-mode (mode-sym
                    &rest args
                    &key (buffer (or (current-prompt-buffer) (current-buffer)))
                      (activate t explicit?)
                    &allow-other-keys)
  "Enable MODE-SYM if not already enabled, disable it otherwise."
  (when (modable-buffer-p buffer)
    (let ((existing-instance (find mode-sym (slot-value buffer 'modes) :key #'sera:class-name-of)))
      (unless explicit?
        (setf activate (or (not existing-instance)
                           (not (enabled-p existing-instance)))))
      (if activate
          ;; TODO: Shall we pass args to `make-instance' or `enable'?
          ;; Have 2 args parameters?
          (let ((mode (or existing-instance
                          (apply #'make-instance mode-sym
                                 :buffer buffer
                                 args))))
            (enable mode)
            (echo "~@(~a~) mode enabled." mode))
          (when existing-instance
            (disable existing-instance)
            (echo "~@(~a~) mode disabled." existing-instance))))))

(define-command toggle-modes (&key (buffer (current-buffer)))
  "Enable marked modes, disable unmarked modes for BUFFER."
  (let* ((modes-to-enable
           (prompt
            :prompt "Mark modes to enable, unmark to disable"
            :sources (make-instance
                      'mode-source
                      :return-actions (list 'identity
                                            (lambda-command force-disable-auto-mode (modes)
                                              "Return selection but force disabling auto-mode.
This is convenient when you use auto-mode by default and you want to toggle a
mode permanently for this buffer."
                                              (delete (read-from-string "nyxt/auto-mode:auto-mode")
                                                      modes)))
                      :marks (mapcar #'sera:class-name-of (modes buffer)))))
         (modes-to-disable (set-difference (all-mode-symbols) modes-to-enable
                                           :test #'string=)))
    (disable-modes (uiop:ensure-list modes-to-disable) buffer)
    (enable-modes (uiop:ensure-list modes-to-enable) buffer))
  buffer)

(export-always 'find-buffer)
(defun find-buffer (mode-symbol)
  "Return first buffer matching MODE-SYMBOL."
  (find-if (lambda (b)
             (find-submode mode-symbol b))
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

(defmethod on-signal-load-canceled ((mode mode) url)
  url)

(defmethod on-signal-load-committed ((mode mode) url)
  url)

(defmethod on-signal-load-finished ((mode mode) url)
  url)

(defmethod on-signal-load-failed ((mode mode) url)
  url)

(defmethod url-sources ((mode mode) return-actions)
  (declare (ignore return-actions))
  nil)

(defmethod url-sources :around ((mode mode) return-actions)
  (declare (ignore return-actions))
  (alex:ensure-list (call-next-method)))

(defmethod s-serialization:serializable-slots ((object mode))
  "Discard keymaps which can be quite verbose."
  (delete 'keymap-scheme
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of object)))))
