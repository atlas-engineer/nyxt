;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)
(nyxt:define-package :nyxt/setting
  (:documentation "Package defining `setting', a user-convenience class to configure Nyxt."))
(in-package :nyxt/setting)

(defvar *settings* (make-hash-table)
  "TODO: Document the function / callback type.")

(defvar *current-instance-p* nil)
(defvar *all-instances-p* nil)
(defvar *new-instances-p* nil)
(defvar *auto-config-p* nil)

(define-class setting ()
  ((name
    ""
    :type string)
   (target-class-name
    t
    :type symbol)
   (current-instance-p
    t
    :type boolean
    :initarg nil
    :reader t
    :writer nil)
   (current-instance
    (constantly nil)
    :type function
    :initarg nil
    :reader t
    :writer nil)
   (all-instances-p
    nil
    :type boolean
    :initarg nil
    :reader t
    :writer nil)
   (all-instances
    (constantly nil)
    :type function
    :initarg nil
    :reader t
    :writer nil)
   (new-instances-p
    t
    :type boolean
    :initarg nil
    :reader t
    :writer nil)
   (auto-config-p
    t
    :type boolean
    :initarg nil
    :reader t
    :writer nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A setting is a convenient way to keep track of and handle
configuration in Nyxt. It can handle updating the target class in
the auto-config file, a set of live instances and all new instances.
See `apply-setting'."))

;; TODO: Allow re-init with shared-initialize?
(defmethod initialize-instance :after ((setting setting) &key)
  (setf (gethash (name setting) *settings*) setting))

(define-class slot-setting (setting)
  ((slot-name
    nil
    :type symbol)
   (new-value
    nil
    :type t))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A slot setting is used for configuring slots in Nyxt by
providing a SLOT-NAME and SLOT-VALUE."))

(define-class generic-setting (setting)
  ((handler
    (alexandria:required-argument 'handler)
    :type function))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A generic setting is used for configuring Nyxt by applying
a HANDLER function to the target class."))

(defmethod extend-configuration ((setting generic-setting)) ; TODO: Better name?
  "Configure SETTING's TARGET-CLASS-NAME for new instances.
See `apply-setting'."
  (hooks:add-hook
   (slot-value (find-class (target-class-name setting)) 'nyxt::customize-hook)
   (make-instance
    'hooks:handler
    :append t
    :fn (lambda (object)
          (declare (ignorable object))
          (funcall (handler setting) object))
    :name (gensym "EXTEND-CONFIGURATION"))))

(defmethod extend-configuration ((setting slot-setting))
  "Configure SETTING's TARGET-CLASS-NAME for new instances.
See `apply-setting'."
  (hooks:add-hook
   (slot-value (find-class (target-class-name setting)) 'nyxt::customize-hook)
   (make-instance
    'hooks:handler
    :append t
    :fn (lambda (object)
          (declare (ignorable object))
          ;; TODO: Fix the writer lookup.
          ;; `slot-definition-writers' only works on direct slot definitions.
          ;; We need to be able to find a slot writer even if it's defined
          ;; in a superclass. This is necessary for `buffer-setting' to work.
          (alex:if-let ((writer (first (closer-mop:slot-definition-writers
                                 (mopu:get-slot-definition (target-class-name setting) (slot-name setting))))))
                (funcall (fdefinition writer) (new-value setting) object)
                (setf (slot-value object (slot-name setting)) (new-value setting))))
    ;; TODO: Use `:place' / `:value'?
    ;; What if we want to stack the changes?
    ;; Also, does that ignore the slot writer?
    :name (gensym "EXTEND-CONFIGURATION"))))

;; TODO: SLOT should be WRITER-NAME (a symbol) instead.
(defmethod apply-setting ((setting setting)
                          &key current-instance instances new-instances-p auto-config-p)
  "Update new instances of SETTING's `target-class-name'."
  (declare (ignore current-instance instances auto-config-p))
  (when (and (new-instances-p setting) new-instances-p)
    (extend-configuration setting)))

(defmethod apply-setting ((setting slot-setting)
                          &key current-instance-p all-instances-p new-instances-p auto-config-p)
  "With CLASS-NAME, CURRENT-INSTANCE and INSTANCES that are not of this class are
automatically filtered out. "
  (declare (ignore new-instances-p))
  (call-next-method)
  (when (and (auto-config-p setting) auto-config-p)
    (nyxt::auto-configure
     :class-name (target-class-name setting)
     :slot (slot-name setting)
     :slot-value (new-value setting)))
  ;; TODO: Factor instance listing?  Do we need it at all?
  ;; For existing instances:
  (let ((instances (delete-duplicates
                    (sera:filter (if (target-class-name setting)
                                     (sera:eqs (target-class-name setting))
                                     #'identity)
                                 (append (when (and current-instance-p
                                                    (current-instance-p setting))
                                           (uiop:ensure-list (funcall (current-instance setting))))
                                         (when (and all-instances-p
                                                    (all-instances-p setting))
                                           (funcall (all-instances setting))))
                                 :key #'sera:class-name-of))))
    (mapc (lambda (instance)
            ;; TODO: Fix the writer lookup.
            ;; See comment in `extend-configuration'.
            (alex:if-let ((writer (first (closer-mop:slot-definition-writers
                                 (mopu:get-slot-definition (target-class-name setting) (slot-name setting))))))
                (funcall (fdefinition writer) (new-value setting) instance)
                (setf (slot-value instance (slot-name setting)) (new-value setting))))
          instances)))

(defmethod apply-setting ((setting generic-setting)
                          &key current-instance-p all-instances-p new-instances-p auto-config-p)
  "With CLASS-NAME, CURRENT-INSTANCE and INSTANCES that are not of this class are
automatically filtered out. "
  (declare (ignore new-instances-p))
  (call-next-method)
  (when auto-config-p
    (apply #'nyxt::auto-configure
           :class-name (target-class-name setting)
           :form (handler setting)))
  ;; For existing instances:
  (let ((instances (delete-duplicates
                    (sera:filter (if (target-class-name setting)
                                     (sera:eqs (target-class-name setting))
                                     #'identity)
                                 (append (when (and current-instance-p
                                                    (current-instance-p setting))
                                           (uiop:ensure-list (funcall (current-instance setting))))
                                         (when (and all-instances-p
                                                    (all-instances-p setting))
                                           (funcall (all-instances setting))))
                                 :key #'sera:class-name-of))))
    (mapc (handler setting) instances)))

(define-class browser-setting (setting)
  ((target-class-name
    'browser
    :type symbol)
   (current-instance
    (lambda () *browser*)
    :type function
    :initarg nil
    :reader t
    :writer nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A browser setting is used for configuring the browser instance."))

(define-class buffer-setting (setting)
  ((target-class-name
    'web-buffer
    :type symbol)
   (all-instances-p
    t
    :type boolean
    :initarg nil
    :reader t
    :writer nil)
   (all-instances
    (lambda () (buffer-list))
    :type function
    :initarg nil
    :reader t
    :writer nil)
   (current-instance
    (lambda () (current-buffer))
    :type function
    :initarg nil
    :reader t
    :writer nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A buffer setting is used for configuring buffers."))

(define-class keystyle (buffer-setting slot-setting)
  ((slot-name 'keyscheme))
  (:documentation "A setting for configuring the keyscheme.
The new value is for example the 'nyxt/emacs-mode:emacs-mode' string."))

(define-class zoom-ratio-default-setting (buffer-setting slot-setting)
  ((slot-name 'zoom-ratio-default))
  (:documentation "A setting for configuring `zoom-ratio-default'.
The new value should be a float."))

(defun find-setting (setting-designator)
  (gethash (intern (symbol-name setting-designator))
           *settings*))

(-> ensure (string) string)
(defun ensure-setting (setting-name)
  "Check whether SETTING-NAME exists  and return it."
  (unless (gethash (alex:make-keyword (string-upcase setting-name)) *settings*)
    (log:warn "Undefined setting ~s" setting-name))
  setting-name)
