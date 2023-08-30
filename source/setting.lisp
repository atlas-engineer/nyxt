;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

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

;; TODO: Simplify and maybe move to a different package.
(defun find-writer (class-name slot-name)
  (some (lambda (c)
          (find-method (fdefinition `(setf ,slot-name))
                       '()
                       (list (sera:find-class-safe t) c)
                       nil))
        (cons (sera:find-class-safe class-name) (mopu:superclasses class-name))))

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
    :fn (lambda (object)
          (declare (ignorable object))
          (funcall (handler setting) object))
    :name (gensym "EXTEND-CONFIGURATION"))
   :append t))

(defmethod extend-configuration ((setting slot-setting))
  "Configure SETTING's TARGET-CLASS-NAME for new instances.
See `apply-setting'."
  (hooks:add-hook
   (slot-value (find-class (target-class-name setting)) 'nyxt::customize-hook)
   (make-instance
    'hooks:handler
    :fn (lambda (object)
          (declare (ignorable object))
          (alex:if-let ((writer (find-writer (target-class-name setting)
                                             (slot-name setting))))
            (funcall (closer-mop:method-generic-function writer) (new-value setting) object)
            (setf (slot-value object (slot-name setting)) (new-value setting))))
    ;; TODO: Use `:place' / `:value'?
    ;; What if we want to stack the changes?
    ;; Also, does that ignore the slot writer?
    :name (gensym "EXTEND-CONFIGURATION"))
   :append t))

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
            (alex:if-let ((writer (find-writer (target-class-name setting)
                                               (slot-name setting))))
              (funcall (closer-mop:method-generic-function writer) (new-value setting) instance)
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

(define-class buffer-slot-setting (buffer-setting slot-setting)
  ((slot-name
    nil
    :type symbol)
   (new-value
    nil
    :type t))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A setting for configuring a buffer slot."))

(define-class browser-slot-setting (browser-setting slot-setting)
  ((slot-name
    nil
    :type symbol)
   (new-value
    nil
    :type t))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A setting for configuring a browser slot."))

(define-class buffer-generic-setting (buffer-setting generic-setting)
  ((handler
    (alexandria:required-argument 'handler)
    :type function))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A setting for applying a HANDLER to buffers."))

(define-class browser-generic-setting (browser-setting generic-setting)
  ((handler
    (alexandria:required-argument 'handler)
    :type function))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A setting for applying a HANDLER to browser."))

(export-always 'apply-generic-setting)
(defun apply-generic-setting (handler generic-setting-class)
  "Configure a class by applying a handler with generic setting."
  (let ((auto-config-p (if-confirm ("Apply this setting on restart?")))
        (new-instances-p (if-confirm ("Apply this setting until the end of this session?")))
        (all-instances-p (if-confirm ("Apply this setting immediately?")))
        (current-instance-p (if-confirm ("Apply this setting to the current instance?"))))
    (when (mopu:subclassp generic-setting-class 'generic-setting)
      (apply-setting (make-instance generic-setting-class
                                    :handler handler)
                     :auto-config-p auto-config-p
                     :new-instances-p new-instances-p
                     :all-instances-p all-instances-p
                     :current-instance-p current-instance-p))))

(export-always 'apply-slot-setting)
(defun apply-slot-setting (slot-name slot-setting-class &optional new-value)
  "Configure a slot by applying a slot setting."
  (let ((new-value (or new-value
                       (read-from-string
                        (prompt1
                         :prompt (format nil "Configure slot value ~a" slot-name)
                         :sources 'prompter:raw-source))))
        (auto-config-p (if-confirm ("Apply this setting on restart?")))
        (new-instances-p (if-confirm ("Apply this setting until the end of this session?")))
        (all-instances-p (if-confirm ("Apply this setting immediately?")))
        (current-instance-p (if-confirm ("Apply this setting to the current instance?"))))
    (when (mopu:subclassp slot-setting-class 'slot-setting)
      (apply-setting (make-instance slot-setting-class
                                    :slot-name slot-name
                                    :new-value new-value)
                     :auto-config-p auto-config-p
                     :new-instances-p new-instances-p
                     :all-instances-p all-instances-p
                     :current-instance-p current-instance-p))))

(defun find-setting (setting-designator)
  (gethash (intern (symbol-name setting-designator))
           *settings*))

(-> ensure (string) string)
(defun ensure-setting (setting-name)
  "Check whether SETTING-NAME exists  and return it."
  (unless (gethash (alex:make-keyword (string-upcase setting-name)) *settings*)
    (log:warn "Undefined setting ~s" setting-name))
  setting-name)
