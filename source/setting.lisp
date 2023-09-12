;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Change `auto-configure' to accept setting instead?

;; TODO: New package?
(in-package :nyxt)
(nyxt:define-package :nyxt/setting
  (:documentation "User-convenience to configure Nyxt."))
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
  (:metaclass closer-mop:funcallable-standard-class)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "XXX:"))

;; TODO: Allow re-init with shared-initilize?
(defmethod initialize-instance :after ((setting setting) &key)
  (setf (gethash (name setting) *settings*) setting))

(define-class slot-setting (setting)
  ((slot-name
    nil
    :type symbol)
   (new-value
    nil
    :type t))
  (:metaclass closer-mop:funcallable-standard-class)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "XXX:"))

(define-class generic-setting (setting)
  ((handler
    (alexandria:required-argument 'handler)
    :type function))
  (:metaclass closer-mop:funcallable-standard-class)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "XXX:"))

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
    :name (gensym "EXTEND-CONFIGURATION"))))

(defmethod extend-configuration ((setting slot-setting))
  "Configure SETTING's TARGET-CLASS-NAME for new instances.
See `apply-setting'."
  (hooks:add-hook
   (slot-value (find-class (target-class-name setting)) 'nyxt::customize-hook)
   (make-instance
    'hooks:handler
    :fn (lambda (object)
          (declare (ignorable object))
          (let* ((writer (first (closer-mop:slot-definition-writers
                                 (mopu:get-slot-definition 'foo 'age)))))
            (if writer
                (funcall (fdefinition writer) (new-value setting) object)
                ;; No writer method found:
                (setf (slot-value object (slot-name setting)) (new-value setting)))))
    ;; TODO: Use `:place' / `:value'?
    ;; What if we want to stack the changes?
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
                    (sera:filter (if (class-name setting)
                                     (sera:eqs (class-name setting))
                                     #'identity)
                                 (append (when (and current-instance-p
                                                    (current-instance-p setting))
                                           (uiop:ensure-list (funcall (current-instance setting))))
                                         (when (and all-instances-p
                                                    (all-instances-p setting))
                                           (funcall (all-instances setting))))
                                 :key #'sera:class-name-of))))
    (mapc (lambda (instance)
            ;; TODO: use writer?  If so, factor code from `extend-configuration'.
            (setf (slot-value instance (slot-name setting))
                  (new-value setting)))
          instances)))

(defmethod apply-setting ((setting generic-setting)
                          &key current-instance-p all-instances-p new-instances-p auto-config-p)
  "With CLASS-NAME, CURRENT-INSTANCE and INSTANCES that are not of this class are
automatically filtered out. "
  (declare (ignore new-instances-p))
  (call-next-method)
  (when auto-config-p
    (apply #'nyxt::auto-configure
           :class-name (class-name setting)
           :form (handler setting)))
  ;; For existing instances:
  (let ((instances (delete-duplicates
                    (sera:filter (if (class-name setting)
                                     (sera:eqs (class-name setting))
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
  (:metaclass closer-mop:funcallable-standard-class)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "XXX:"))

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
  (:metaclass closer-mop:funcallable-standard-class)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "XXX:"))

;; (define-setting (buffer-setting keystyle) (style)
;;   "STYLE is for example the 'nyxt/emacs-mode:emacs-mode' string."
;;   ;; (lambda (input-buffer)
;;   ;;   (setf (keyscheme input-buffer) (symbol-value (intern (string-upcase style)))))
;;   (values
;;    (symbol-value (intern (string-upcase style)))
;;    keyscheme))

;; (defvar keystyle
;;   (make-instance 'buffer-slot-setting
;;                  :slot-name keyscheme
;;                  :slot-value (symbol-value (intern (string-upcase style)))))

(define-class keystyle (buffer-setting slot-setting)
  ((slot-name 'keyscheme))
  (:documentation "The slot-value is for example the 'nyxt/emacs-mode:emacs-mode' string."))

;; (setf (gethash :keystyle *settings*)
;;       (lambda (keystyle &key current-instance-p all-instances-p new-instances-p auto-config-p)
;;         "KEYSTYLE is for example the 'nyxt/emacs-mode:emacs-mode' string."
;;         (unless (uiop:emptyp keystyle)
;;           (apply-configuration
;;            :lambda (lambda (input-buffer)
;;                      (setf (keyscheme input-buffer) (symbol-value (intern (string-upcase  keystyle)))))
;;            :class-name 'input-buffer
;;            :current-instance (when current-instance-p (current-buffer))
;;            :instances (when all-instances-p (buffer-list))
;;            :new-instances-p new-instances-p
;;            :auto-config-p auto-config-p))))

;; (setf (gethash :theme *settings*)
;;       (lambda (theme &key auto-config-p &allow-other-keys)
;;         "THEME is for example the 'theme::+light-theme+' string."
;;         (unless (uiop:emptyp theme)
;;           (apply-configuration
;;            :slot 'theme
;;            :slot-value (symbol-value (intern (string-upcase theme)))
;;            :current-instance *browser*
;;            :auto-config-p auto-config-p))))

;; (setf (gethash :default-new-buffer-url *settings*)
;;       (lambda (url &key new-instances-p auto-config-p &allow-other-keys)
;;         "URL is a string."
;;         (unless (uiop:emptyp url)
;;           (apply-configuration
;;            :slot 'default-new-buffer-url
;;            :slot-value url
;;            :current-instance *browser*
;;            :new-instances-p new-instances-p
;;            :auto-config-p auto-config-p))))

;; (setf (gethash :set-zoom-ratio *settings*)
;;       (lambda (zoom-ratio &key new-instances-p auto-config-p &allow-other-keys)
;;         "URL is a string."
;;         (unless (uiop:emptyp url)
;;           (apply-configuration
;;            :slot 'current-zoom-ratio
;;            :slot-value zoom-ratio
;;            :current-instance (when current-instance-p (current-buffer))
;;            :instances (when all-instances-p (sera:filter #'document-buffer-p (buffer-list)))
;;            :new-instances-p new-instances-p
;;            :auto-config-p auto-config-p))))

(defun find-setting (setting-designator)
  (gethash (intern (symbol-name setting-designator))
           *settings*))

(-> ensure (string) string)
(defun ensure-setting (setting-name)
  "Check whether SETTING-NAME exists  and return it."
  (unless (gethash (alex:make-keyword (string-upcase setting-name)) *settings*)
    (log:warn "Undefined setting ~s" setting-name))
  setting-name)
