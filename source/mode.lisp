;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'define-mode)
(defmacro define-mode (name direct-superclasses &body body)
  "Define mode NAME.
When DIRECT-SUPERCLASSES is T, then the mode has no parents.
Otherwise, the mode's parents are ROOT-MODE and DIRECT-SUPERCLASSES.

As a third argument, a documentation-string can be provided.
The last argument (third if no doc-string provided, fourth if one is)
is the direct-slots list.

A mode toggler command is also defined as NAME.
Its arguments are passed to the class instantiation.
Two key arguments have a special meaning beside the slot value of the mode:
- :BUFFER is used to enable or disable the mode in the corresponding buffer.
  This should always be specified in Lisp code since the active buffer, if any,
  might not be the right buffer.
- :ACTIVATE is used to choose whether to enable or disable the mode.
If :ACTIVATE is omitted, the mode is toggled.
The buffer is returned so that mode toggles can be chained.

Example:

\(define-mode my-mode ()
  \"Dummy mode for the custom key bindings in `*my-keymap*'.\"
  ((keymap-schemes (keymap:make-scheme
                    scheme:emacs *my-keymap*
                    scheme:vi-normal *my-keymap*))))"
  (let* ((docstring (if (stringp (first body))
                        (first body)
                        (progn
                          (log:warn "The define-mode definition for ~a doesn't have a documentation string." name)
                          nil)))
         (direct-slots (if (stringp (first body))
                           (cadr body)
                           (first body)))
         (class-args `(,name
                       ,(unless (eq (first direct-superclasses) t)
                          (append direct-superclasses '(root-mode)))
                       ,direct-slots
                       (:export-class-name-p t)
                       (:export-accessor-names-p t)
                       (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))))
         (configurable-class-name (user-class-name name)))
    (when docstring
      (setf class-args (append class-args
                               `((:documentation ,docstring)))))
    (alex:with-gensyms (existing-instance new-mode)
      `(progn
         (define-class ,@class-args)
         (define-user-class ,name)
         ;; TODO: Can we delete the last mode?  What does it mean to have no mode?
         ;; Should probably always have root-mode.
         ,(unless (eq name 'root-mode)
            `(define-command-global ,name (&rest args
                                           &key
                                           ;; TODO: Shall we have a function that
                                           ;; returns the focused buffer?
                                           ;; `focused-buffer'?  `current-buffer*'?
                                           ;; Rename `current-buffer' to
                                           ;; `current-view-buffer' and add
                                           ;; `current-buffer' for this task?
                                           (buffer (or (current-prompt-buffer) (current-buffer)))
                                           (activate t explicit?)
                                           &allow-other-keys)
               ,docstring
               (unless (find 'buffer (mopu:superclasses buffer) :key #'class-name)
                 ;; Warning: (typep buffer 'buffer) would not work for prompt-buffers
                 ;; if the BUFFER class was reassigned after the PROMPT-BUFFER class
                 ;; declaration.
                 (error ,(format nil "Mode command ~a called on non-buffer" name)))
               (let ((,existing-instance (find-mode buffer ',name)))
                 (unless explicit?
                   (setf activate (not ,existing-instance)))
                 (if activate
                     (unless ,existing-instance
                       ;; TODO: Should we move mode to the front when it already exists?
                       (let ((,new-mode (apply #'make-instance ',configurable-class-name
                                               :buffer buffer
                                               args)))
                         (funcall* (constructor ,new-mode) ,new-mode)
                         (push ,new-mode (modes buffer))
                         (hooks:run-hook (enable-hook ,new-mode) ,new-mode)
                         (hooks:run-hook (enable-mode-hook buffer) ,new-mode))
                       (if (and (prompt-buffer-p buffer)
                                (eq (first (active-prompt-buffers (window buffer)))
                                    buffer))
                           (prompt-render-prompt buffer)
                           (print-status))
                       (log:debug "~a enabled." ',name))
                     (when ,existing-instance
                       (hooks:run-hook (disable-hook ,existing-instance) ,existing-instance)
                       (hooks:run-hook (disable-mode-hook buffer) ,existing-instance)
                       (funcall* (destructor ,existing-instance) ,existing-instance)
                       (setf (modes buffer) (delete ,existing-instance
                                                    (modes buffer)))
                       (if (and (prompt-buffer-p buffer)
                                (eq (first (active-prompt-buffers (window buffer)))
                                    buffer))
                           (prompt-render-prompt buffer)
                           (print-status))
                       (log:debug "~a disabled." ',name))))
               buffer))))))

(hooks:define-hook-type mode (function (root-mode)))

(define-mode root-mode (t)
  "All modes inherit from `root-mode'."
  ((buffer nil
           :type (or buffer null))
   (glyph nil :type (or string null)
              :accessor nil
              :documentation "A glyph used to represent this mode, if unset, it
              will be dynamically calculated as the first letters of the mode
              name.")
   (visible-in-status-p
    t
    :documentation "Whether the mode is visible in the status line.")
   (rememberable-p t
                   :documentation "Whether this mode is visible to `auto-mode'.")
   (activate :accessor activate :initarg :activate) ; TODO: This can be used in the future to temporarily turn off modes without destroying the object.
   (constructor nil ; TODO: Make constructor / destructor methods?  Then we can use initialize-instance, etc.
                :type (or function null)
                :documentation
                "A lambda function which initializes the mode upon activation.
It takes the mode as argument.")
   (destructor nil ; TODO: Better name?
               :type (or function null)
               :documentation
               "A lambda function which tears down the mode upon deactivation.
It takes the mode as argument.")
   (enable-hook (make-hook-mode)
                :type hook-mode
                :documentation "This hook is run when enabling the mode.
It takes the mode as argument
It is run before the destructor.")
   (disable-hook (make-hook-mode)
                 :type hook-mode
                 :documentation "This hook is run when disabling the mode.
It takes the mode as argument.
It is run before the destructor.")
   (keymap-scheme (make-hash-table :size 0)
                  :type keymap:scheme)))

(defmethod prompter:object-attributes ((mode root-mode))
  `(("Name" ,(mode-name mode))))

(export-always 'glyph)
(defmethod glyph ((mode root-mode))
  "Return the glyph for a mode, or if unset, return a standard formatted mode."
  (or (slot-value mode 'glyph)
      (format-mode mode)))

(defmethod (setf glyph) (glyph (mode root-mode))
  (setf (slot-value mode 'glyph) glyph))

(defmethod format-mode ((mode root-mode))
  "Produce a string representation of a mode suitable for use in a status
  buffer."
  (str:replace-all "-mode" "" (str:downcase (mode-name mode))))

(export-always 'find-mode)
(defmethod find-mode ((buffer buffer) mode-symbol)
  "Return the mode corresponding to MODE-SYMBOL in active in BUFFER.
Return nil if mode is not found.  MODE-SYMBOL does not have to be namespaced, it
can be 'web-mode as well as 'nyxt/web-mode:web-mode."
  (let ((mode-full-symbol (if (find-class mode-symbol nil)
                              mode-symbol
                              (alex:when-let ((c (mode-command mode-symbol)))
                                (name c)))))
    (when mode-full-symbol
      (find mode-full-symbol
            (modes buffer)
            :key (alex:compose #'class-name #'original-class)))))

(export-always 'find-submode)
(defmethod find-submode ((buffer buffer) mode-symbol)
  "Like `find-mode' but return the first mode in BUFFER that is a sub-mode of MODE-SYMBOL.
It may be MODE-SYMBOL itself."
  (let ((mode-full-symbol (if (find-class mode-symbol nil)
                              mode-symbol
                              (alex:when-let ((c (mode-command mode-symbol)))
                                (name c)))))
    (when mode-full-symbol
      (find-if (lambda (m)
                 (closer-mop:subclassp (class-of m)
                                       (find-class mode-full-symbol)))
               (modes buffer)))))

(export-always 'current-mode)
(defun current-mode (mode-sym)
  "Return mode instance of MODE-SYM in current buffer.
Return NIL if none.
The \"-mode\" suffix is automatically appended to MODE-SYM if missing."
  (find-submode (current-buffer)
                (let ((name (string mode-sym)))
                  (if (str:ends-with-p "-mode" name :ignore-case t)
                      name
                      (intern (str:concat name "-MODE")
                              (symbol-package mode-sym))))))

;; TODO: Find a better way to uniquely identify commands from mode methods.
;; What about symbol properties?  We could use:
;;
;; (setf (get name 'commandp) t)
;;
;; But that doesn't seem to work properly, some commands need to be evaluated
;; twice before they appear in the list.  We could use a class (we used to have
;; a COMMAND class) or intern the symbol into a special package (see `intern'
;; documentation).

(defun mode-list ()
  "Return the list of all namespaced mode symbols."
  (delete-if (complement (lambda (m)
                           (str:suffixp (list (symbol-name m) "-MODE")
                                        "-MODE")))
             (mapcar #'name *command-list*)))

(defun original-class (class-sym)
  "When CLASS-SYM is a user class, return its original class."
  ;; REVIEW: Is the original class always the last one?  What if the user
  ;; decides to mix in another class, e.g. (defclass user-buffer
  ;; (user-buffer buffer unrelated-class))?
  (first (last (mopu:direct-superclasses class-sym))))

(defun mode-command (mode-symbol)
  "Return the mode toggle command.
We loop over `*command-list*' to find the mode command since a mode may be
defined in any package and is unique.

If MODE-SYMBOL is a mode that inherits from another without defining its own
toggle command, return the toggle command of the parent."
  (unless (eq mode-symbol 'root-mode)   ; `root-mode' has not command.
    (or (find (string mode-symbol) *command-list*
              :key (lambda (command) (string (name command)))
              :test #'string=)
        (alex:when-let ((m (find-class mode-symbol nil)))
          (mode-command (class-name (original-class m)))))))

(defun make-mode (mode-symbol buffer)
  ;; (log:debug mode-symbol buffer (mode-command mode-symbol))
  (alex:if-let ((c (mode-command mode-symbol)))
    ;; ":activate t" should not be necessary here since (modes buffer) should be
    ;; empty.
    (funcall (name c) :buffer buffer :activate t)
    (log:warn "Mode command ~a not found." mode-symbol)))

(export-always 'find-buffer)
(defun find-buffer (mode-symbol)
  "Return first buffer matching MODE-SYMBOL."
  (find-if (lambda (b)
             (find-mode b mode-symbol))
           (buffer-list)))

(export-always 'keymap)
(defmethod keymap ((mode root-mode))
  "Return the keymap of MODE according to its buffer keymap scheme.
If there is no corresponding keymap, return nil."
  (keymap:get-keymap (if (buffer mode)
                         (keymap-scheme-name (buffer mode))
                         scheme:cua)
                     (keymap-scheme mode)))

(defmethod on-signal-notify-uri ((mode root-mode) url)
  (set-window-title)
  (print-status)
  url)

(defmethod on-signal-notify-title ((mode root-mode) title)
  (on-signal-notify-uri mode (url (buffer mode)))
  title)

(defmethod on-signal-load-committed ((mode root-mode) url)
  url)

(defmethod on-signal-load-redirected ((mode root-mode) url)
  url)

(defmethod on-signal-load-finished ((mode root-mode) url)
  url)

(defmethod s-serialization:serializable-slots ((object root-mode))
  "Discard keymaps which can be quite verbose."
  (delete 'keymap-scheme
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of object)))))
