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
    :accessor t
    :documentation "Whether the mode is enabled in `buffer'.")
   (enable-hook
    (make-instance 'hook-mode)
    :type hook-mode
    :documentation "Hook run when enabling the mode, after the constructor.
The handlers take the mode as argument.")
   (disable-hook
    (make-instance 'hook-mode)
    :type hook-mode
    :documentation "Hook run when disabling the mode, before the destructor.
The handlers take the mode as argument.")
   (keyscheme-map
    (make-hash-table :size 0)
    :type keymaps:keyscheme-map))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:toggler-command-p nil)
  (:metaclass mode-class))

(defmethod initialize-instance :after ((mode mode) &key)
  (when (eq 'mode (sera:class-name-of mode))
    (error "Cannot initialize `mode', you must subclass it.")))

(defmethod name ((mode mode))
  (sera:class-name-of mode))

(export-always 'enable)
(defgeneric enable (mode &key &allow-other-keys)
  (:method-combination cascade)
  (:method ((mode mode) &key)
    nil)
  (:documentation "Run when enabling a mode.
The pre-defined `:after' method handles further setup.
This method is meant to be specialized for every mode.
It is not meant to be called directly, see `enable-modes' instead.

All the parent modes' `enable' methods run after the exact mode one, cascading
upwards to allow a more useful mode inheritance without duplicating the
functionality. A `cascade' method combination is used for that.

See also `disable'."))

(defmethod enable :around ((mode mode) &key bypass-auto-rules-p &allow-other-keys)
  (let* ((buffer (buffer mode))
         (existing-instance (find (sera:class-name-of mode)
                                  (remove-if (sera:eqs mode) (slot-value buffer 'modes))
                                  :key #'sera:class-name-of)))
    (if existing-instance
        (log:debug "Not enabling ~s since other ~s instance is already in buffer ~a" mode existing-instance buffer)
        (if bypass-auto-rules-p
            (let ((old-bypass (bypass-auto-rules-p (buffer mode))))
              (setf (bypass-auto-rules-p (buffer mode)) t)
              (unwind-protect
                   (call-next-method)
                (setf (bypass-auto-rules-p (buffer mode)) old-bypass)))
            (call-next-method)))))

(defmethod enable :after ((mode mode) &key)
  (setf (enabled-p mode) t)
  (hooks:run-hook (enable-hook mode) mode)
  (let ((buffer (buffer mode)))
    ;; TODO: Should we move mode to the front on re-enable?
    (unless (find mode (slot-value buffer 'modes))
      (setf (modes buffer)
            (cons mode (slot-value buffer 'modes))))
    (hooks:run-hook (enable-mode-hook buffer) mode)
    (when (and (prompt-buffer-p buffer)
               (eq (first (active-prompt-buffers (window buffer)))
                   buffer))
      (prompt-render-prompt buffer))))

(export-always 'disable)
(defgeneric disable (mode &key &allow-other-keys)
  (:method-combination cascade)
  (:method ((mode mode) &key)
    nil)
  (:documentation "Run when disabling a mode.
The pre-defined `:after' method handles further cleanup.
This method is meant to be specialized for every mode.
It is not meant to be called directly, see `disable-modes' instead.

All the parent modes' `disable' methods run after the exact mode one, cascading
upwards to allow a more useful mode inheritance without duplicating the
functionality. A `cascade' method combination is used for that.

See also `enable'."))

(defmethod disable :around ((mode mode) &key bypass-auto-rules-p &allow-other-keys)
  (if bypass-auto-rules-p
      (let ((old-bypass (bypass-auto-rules-p (buffer mode))))
        (setf (bypass-auto-rules-p (buffer mode)) t)
        (unwind-protect
             (call-next-method)
          (setf (bypass-auto-rules-p (buffer mode)) old-bypass)))
      (call-next-method)))

(defmethod disable :after ((mode mode) &key)
  (setf (enabled-p mode) nil)
  (hooks:run-hook (disable-hook mode) mode)
  (let ((buffer (buffer mode)))
    (hooks:run-hook (disable-mode-hook (buffer mode)) mode)
    ;; TODO: Remove from list or not?
    ;; (setf (modes buffer) (delete ,existing-instance (modes buffer)))
    (when (and (prompt-buffer-p buffer)
               (eq (first (active-prompt-buffers (window buffer)))
                   buffer))
      (prompt-render-prompt buffer))))

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

(defun package-modes (packages &key (visibility :any))
  "Return the list of mode symbols in PACKAGES.
See `package-symbols'."
  (delete-if (complement #'mode-class)
             (package-symbols packages :visibility visibility)))

;; TODO: Should allow search all packages, e.g. when PACKAGES is NIL.
(-> resolve-symbol ((or keyword string) (member :function :variable :class :mode :slot :command) &optional (cons *)) symbol)
(export-always 'resolve-symbol)
(defun resolve-symbol (designator type &optional (packages (list :nyxt :nyxt-user)))
  "Find the symbol (of TYPE) designated by DESIGNATOR in PACKAGE.
PACKAGES should be a list of package designators."
  (sera:and-let* ((designator (string designator))
                  (all-packages (list-all-packages))
                  (subpackages (if (equal packages all-packages)
                                   all-packages
                                   (append
                                    (delete-duplicates (mapcar #'find-package packages))
                                    (mappend #'subpackages packages))))
                  (symbols (case type
                             (:function (package-functions subpackages))
                             (:variable (package-variables subpackages))
                             (:class (package-classes subpackages))
                             (:mode (package-modes subpackages))
                             (:slot (mapcar #'name (package-slots subpackages)))
                             (:command (mapcar #'name (list-commands))))))
    (let* ((results (sera:filter (lambda (sym) (string= designator (symbol-name sym)))
                                 symbols)))
      (when (> (length results) 1)
        (log:warn "Multiple ~a symbols found: ~a" designator results))
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
                      (rcurry #'closer-mop:subclassp class)
                      (modes buffer)
                      :key #'class-of)))
        (when (< 1 (length results))
          ;; TODO: What's the best action on multiple mode match?
          (log:debug "Found multiple matching modes: ~a" results))
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
  "Return a `suggestion' wrapping around MODE. "
  (declare (ignore source input))
  (make-instance 'prompter:suggestion
                 :value mode
                 :attributes `(("Mode" ,(string-downcase (symbol-name mode)))
                               ("Documentation" ,(or (first (sera:lines (documentation mode 'function)))
                                                     ""))
                               ("Package" ,(string-downcase (package-name (symbol-package mode)))))))

(define-class mode-source (prompter:source)
  ((prompter:name "Modes")
   (prompter:multi-selection-p t)
   (prompter:constructor (sort (all-mode-symbols) #'string< :key #'symbol-name))
   (prompter:suggestion-maker 'make-mode-suggestion))
  (:export-class-name-p t)
  (:metaclass user-class))

(defmethod prompter:object-attributes ((mode mode) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,mode)))

(define-class active-mode-source (mode-source)
  ((prompter:name "Active modes")
   (buffers '())
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (delete-duplicates
                            (mapcar
                             #'name
                             (mappend
                              #'modes
                              (uiop:ensure-list (buffers source))))))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class))

(define-class inactive-mode-source (mode-source)
  ((prompter:name "Inactive modes")
   (buffers '())
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (let ((common-modes
                                   (reduce #'intersection
                                           (mapcar (lambda (b)
                                                     (mapcar #'name (modes b)))
                                                   (uiop:ensure-list (buffers source))))))
                             (set-difference (all-mode-symbols) common-modes)))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class))

(define-command enable-modes (&rest args &key
                              (modes nil explicit-modes-p)
                              (buffers (current-buffer) explicit-buffers-p)
                              bypass-auto-rules-p
                              &allow-other-keys)
  "Enable MODES for BUFFERS.
MODES should be a list of mode symbols.
BUFFERS and MODES are automatically coerced into a list.
ARGS are passed to the mode `enable' method.

If BUFFERS is a list, return it.
If it's a single buffer, return it directly (not as a list)."
  (declare (ignorable bypass-auto-rules-p))
  ;; We allow NIL values for MODES and BUFFERS in case they are forms, in which
  ;; case it's handy that this function does not error, it simply does nothing.
  ;; REVIEW: But we wrap commands into `with-protect' for this, don't we?
  (let* ((buffers (if buffers
                      (uiop:ensure-list buffers)
                      (unless explicit-buffers-p
                        (prompt
                         :prompt "Enable mode(s) for buffer(s)"
                         :sources (make-instance 'buffer-source
                                                 :multi-selection-p t
                                                 :return-actions '())))))
         (modes (if modes
                    (uiop:ensure-list modes)
                    (unless explicit-modes-p
                      (prompt
                       :prompt "Enable mode(s)"
                       :sources (make-instance 'inactive-mode-source
                                               :buffers buffers))))))
    (dolist (mode modes)
      (check-type mode mode-symbol))
    (dolist (buffer buffers)
      (check-type buffer buffer))
    (mapcar (lambda (buffer)
              (mapcar (lambda (mode-sym)
                        (apply #'enable (or (find mode-sym (slot-value buffer 'modes) :key #'name)
                                            (make-instance mode-sym :buffer buffer))
                               (alex:remove-from-plist args :buffers :modes)))
                      modes))
            (sera:filter #'modable-buffer-p buffers)))
  buffers)

(define-command disable-modes (&key (modes nil explicit-modes-p)
                               (buffers (current-buffer) explicit-buffers-p) bypass-auto-rules-p
                               &allow-other-keys)
  "Disable MODES for BUFFERS.
MODES should be a list of mode symbols.
BUFFERS and MODES are automatically coerced into a list.

If BUFFERS is a list, return it.
If it's a single buffer, return it directly (not as a list)."
  (let* ((buffers (if buffers
                      (uiop:ensure-list buffers)
                      (unless explicit-buffers-p
                        (prompt
                         :prompt "Enable mode(s) for buffer(s)"
                         :sources (make-instance 'buffer-source
                                                 :multi-selection-p t
                                                 :return-actions '())))))
         (modes (if modes
                    (uiop:ensure-list modes)
                    (unless explicit-modes-p
                      (prompt
                       :prompt "Disable mode(s)"
                       :sources (make-instance 'active-mode-source
                                               :buffers buffers))))))
    (dolist (mode modes)
      (check-type mode mode-symbol))
    (dolist (buffer buffers)
      (check-type buffer buffer))
    (mapcar (lambda (buffer)
              (mapcar (alex:rcurry #'disable :bypass-auto-rules-p bypass-auto-rules-p)
                      (delete nil (mapcar (lambda (mode) (find mode (modes buffer) :key #'name))
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
    (disable-modes :modes modes-to-disable :buffers buffer)
    (enable-modes :modes modes-to-enable :buffers buffer))
  buffer)

(define-command-global reload-with-modes (&optional (buffer (current-buffer)))
  "Reload the buffer with the queried modes.
This bypasses auto-rules.
Auto-rules are re-applied once the page is reloaded."
  (let* ((modes-to-enable (prompt
                           :prompt "Mark modes to enable, unmark to disable"
                           :sources (make-instance 'mode-source
                                                   :marks (mapcar #'sera:class-name-of (modes (current-buffer))))))
         (modes-to-disable (set-difference (all-mode-symbols) modes-to-enable
                                           :test #'string=)))
    (hooks:once-on (request-resource-hook buffer)
        (request-data)
      (setf (bypass-auto-rules-p (buffer request-data)) nil)
      request-data)
    (setf (bypass-auto-rules-p buffer) t)
    (when modes-to-enable
      (disable-modes :modes modes-to-disable :buffers buffer))
    (when modes-to-disable
      (enable-modes :modes (uiop:ensure-list modes-to-enable) :buffer buffer))
    (reload-buffer buffer)))

(export-always 'find-buffer)
(defun find-buffer (mode-symbol)
  "Return first buffer matching MODE-SYMBOL."
  (find-if (lambda (b)
             (find-submode mode-symbol b))
           (buffer-list)))

(export-always 'keymap)
(defmethod keymap ((mode mode))
  "Return the keymap of MODE according to its buffer `keyscheme-map'.
If there is no corresponding keymap, return nil."
  (keymaps:get-keymap (if (buffer mode)
                          (keyscheme (buffer mode))
                          keyscheme:cua)
                      (keyscheme-map mode)))

(defmethod on-signal-notify-uri ((mode mode) url)
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

(defmethod on-signal-button-press ((mode mode) button-key)
  (declare (ignorable button-key))
  nil)

(defmethod url-sources ((mode mode) return-actions)
  (declare (ignore return-actions))
  nil)

(defmethod url-sources :around ((mode mode) return-actions)
  (declare (ignore return-actions))
  (alex:ensure-list (call-next-method)))

(defmethod s-serialization:serializable-slots ((object mode))
  "Discard keymaps which can be quite verbose."
  (delete 'keyscheme-map
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of object)))))
