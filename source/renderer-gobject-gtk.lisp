;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class gobject-gtk-browser ()
  ((modifier-translator
    'translate-modifiers
    :documentation "Function that returns a list of modifiers
understood by `keymap:make-key'.")
   (gir-gtk (gir:ffi "Gtk"))
   (gir-gdk (gir:ffi "Gdk"))
   (gir-webkit (gir:require-namespace "WebKit2"))
   (web-context
    nil
    :accessor nil
    :export nil
    :documentation "Single instantiation of our custom web context."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

(define-class gobject-gtk-window ()
  ((gtk-object)
   (box-layout)
   (minibuffer-container)
   (minibuffer-view)
   (status-container)
   (message-container)
   (message-view)
   (key-string-buffer))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

(define-class gobject-gtk-buffer ()
  ((gtk-object)
   (proxy-uri (quri:uri ""))
   (proxy-ignored-hosts '())
   (data-manager-path (make-instance 'data-manager-data-path
                                     :dirname (uiop:xdg-cache-home +data-root+))
                      :documentation "Directory in which the
data-manager will store the data separately for each buffer."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

(define-class data-manager-data-path (data-path)
  ((ref :initform "data-manager"))
  (:export-class-name-p t)
  (:accessor-name-transformer #'class*:name-identity))

(defmethod ffi-initialize ((browser gobject-gtk-browser) urls startup-timestamp)
  (log:debug "Initializing Gobject-GTK Interface")
  (gir:invoke ((gir-gtk browser) 'main))
  ;; (finalize browser urls startup-timestamp)
  (log:debug "Interface initialized."))

(defmethod initialize-instance :after ((browser gobject-gtk-browser) &key)
  (gir:invoke ((gir-gtk browser) 'init) nil))

(defmethod initialize-instance :after ((window gobject-gtk-window) &key)
  (with-slots (gtk-object box-layout active-buffer
               minibuffer-container minibuffer-view
               status-buffer status-container
               message-container message-view
               id key-string-buffer) window
    (with-slots (gir-gtk) *browser*
      (setf id (get-unique-window-identifier *browser*))
      (setf gtk-object (gir:invoke (gir-gtk "Window" 'new)
                                   (gir:nget gir-gtk "WindowType" :toplevel)))
      (gir:invoke (gtk-object 'set-default-size) 1024 768)
      (setf box-layout (gir:invoke (gir-gtk "Box" 'new)
                                   (gir:nget gir-gtk "Orientation" :vertical) 0))
      (setf minibuffer-container (gir:invoke (gir-gtk "Box" 'new)
                                             (gir:nget gir-gtk "Orientation" :vertical) 0))
      (setf message-container (gir:invoke (gir-gtk "Box" 'new)
                                          (gir:nget gir-gtk "Orientation" :vertical) 0))
      (setf status-container (gir:invoke (gir-gtk "Box" 'new)
                                         (gir:nget gir-gtk "Orientation" :vertical) 0))
      ;; Create a pseudo text entry widget to render special keys
      (setf key-string-buffer (gir:invoke (gir-gtk "Entry" 'new)))
      ;; Create a temporary active buffer
      (setf active-buffer (make-dummy-buffer))
      ;; Add the views to the box layout and to the window
      (gir:invoke (box-layout 'pack-start) (gtk-object active-buffer) t t 0)
      ;; Message view
      (setf message-view (make-web-view))
      (gir:invoke (box-layout 'pack-end) message-container nil nil 0)
      (gir:invoke (message-container 'pack-start) message-view t t 0)
      (gir:invoke (message-container 'set-size-request) -1 (message-buffer-height window))
      ;; Status area
      (setf status-buffer (make-instance 'user-status-buffer))
      (gir:invoke (box-layout 'pack-end) status-container nil nil 0)
      (gir:invoke (status-container 'pack-start) (gtk-object status-buffer) t t 0)
      (gir:invoke (status-container 'set-size-request) -1 (height status-buffer))
      ;; Minibuffer view
      (setf minibuffer-view (make-web-view))
      (gir:invoke (box-layout 'pack-end) minibuffer-container nil nil 0)
      (gir:invoke (minibuffer-container 'pack-start) minibuffer-view t t 0)
      (gir:invoke (minibuffer-container 'set-size-request) -1 0)
      ;; Add box layout to the view
      (gir:invoke (gtk-object 'add) box-layout)
      (setf (slot-value *browser* 'last-active-window) window)
      (gir:connect gtk-object
                   :key-press-event
                   (lambda (widget event)
                     (declare (ignore widget))
                     (on-signal-key-press-event window event)))
      ;; RM
      (gir::g-signal-connect-data (gir::this-of gtk-object)
                                  "destroy"
                                  (cffi:foreign-symbol-pointer "gtk_main_quit")
                                  (cffi:null-pointer)
                                  (cffi:null-pointer)
                                  0)
      
      (gir:invoke (gtk-object 'show-all)))
    ;; (gobject:g-signal-connect
    ;;  gobject-gtk-object "destroy"
    ;;  (lambda (widget) (declare (ignore widget))
    ;;    (on-signal-destroy window)))
    ;; (gobject:g-signal-connect
    ;;  gobject-gtk-object "window-state-event"
    ;;  (lambda (widget event) (declare (ignore widget))
    ;;    (setf (fullscreen-p window)
    ;;          (find :fullscreen
    ;;                (gdk:gdk-event-window-state-new-window-state event)))
    ;;    nil))
    ))

(defmethod initialize-instance :after ((buffer gobject-gtk-buffer) &key)
  (let ((path (data-manager-path buffer)))
    (setf (data-manager-path buffer)
          (make-instance 'data-manager-data-path
                         :dirname (pathname (str:concat (namestring (dirname path))
                                                        "/nyxt-data-manager-"
                                                        (id buffer))))))
  (ffi-buffer-make buffer))

(defmethod initialize-instance :after ((buffer status-buffer) &key)
  (with-slots (gtk-object) buffer
    (setf gtk-object (make-web-view))))

;; (defmethod web-context ((browser gobject-gtk-browser))
;;   (or (slot-value *browser* 'web-context)
;;       (setf (slot-value *browser* 'web-context) 
;;             (make-instance 'webkit:webkit-web-context)))) ; TASK

(defmethod ffi-within-renderer-thread ((browser gobject-gtk-browser) thunk)
  (declare (ignore browser))
  (funcall thunk))

(defmacro define-ffi-method (name args &body body)
  (let* ((docstring (when (stringp (first body))
                      (prog1
                          (list (first body))
                        (setf body (rest body)))))
         (declares (when (and (listp (first body))
                              (eq 'declare (first (first body))))
                     (prog1
                         (first body)
                       (setf body (rest body))))))
    `(defmethod ,name ,args
       ,@docstring
       ,declares
       (progn
         ,@body))))

(define-ffi-method ffi-kill-browser ((browser gobject-gtk-browser))
  (unless *keep-alive*
    (gir:invoke ((gir-gtk browser) 'main-quit))))

(defun make-web-view (&key context-buffer)
  "Return a web view instance.
When passed a web buffer, create a buffer-local web context.
Such contexts are not needed for internal buffers."
  (declare (ignore context-buffer))
  (let ((view (gir:invoke ((gir-webkit *browser*) "WebView" 'new))))
    (gir:invoke (view 'load_uri) "https://duckduckgo.com")
    view))


;; (define-ffi-method on-signal-destroy ((window gobject-gtk-window))
;;   ;; remove buffer from window to avoid corruption of buffer
;;   (gobject-gtk:gobject-gtk-container-remove (box-layout window) (gobject-gtk-object (active-buffer window)))
;;   (window-delete window))

;; (define-ffi-method ffi-window-delete ((window gobject-gtk-window))
;;   "Delete a window object."
;;   (gobject-gtk:gobject-gtk-widget-destroy (gobject-gtk-object window)))

(define-ffi-method ffi-window-fullscreen ((window gobject-gtk-window))
  (gir:invoke ((gtk-object window) 'fullscreen)))

(define-ffi-method ffi-window-unfullscreen ((window gobject-gtk-window))
  (gir:invoke ((gtk-object window) 'unfullscreen)))

(defun derive-key-string (keyval character)
  "Return string representation of a keyval.
Return nil when key must be discarded, e.g. for modifiers."
  (let ((result
          (match keyval
            ((or "Alt_L" "Super_L" "Control_L" "Shift_L"
                 "Alt_R" "Super_R" "Control_R" "Shift_R"
                 "ISO_Level3_Shift" "Arabic_switch")
             ;; Discard modifiers (they usually have a null character).
             nil)
            ((guard s (str:contains? "KP_" s))
             (str:replace-all "KP_" "keypad" s))
            ("minus" "hyphen")
            (_ (if (or (char<= character #\space)
                       (char= character #\Del))
                   keyval
                   (string character))))))
    (if (< 1 (length result))
        (str:replace-all "_" "" (string-downcase result))
        result)))

(defun translate-modifiers (modifier-state &optional event)
  "Return list of modifiers fit for `keymap:make-key'.
See `gobject-gtk-browser's `modifier-translator' slot."
  (declare (ignore event))
  (let ((modifiers (list)))
    (dolist (mdef '((#b1    "shift")
                    (#b100  "control")
                    (#b1000 "meta")))
      (destructuring-bind (int modifier)
          mdef
        (unless (zerop (logand int modifier-state))
          (push modifier modifiers))))
    modifiers))

(defmethod printable-p ((window gobject-gtk-window) event)
  "Return the printable value of EVENT."
  ;; Generate the result of the current keypress into a dummy
  ;; (invisible) key-string-buffer so that we can collect the printed
  ;; representation of composed keypress, such as dead keys.
  (gir:invoke ((key-string-buffer window) 'im-context-filter-keypress) event)
  (when (<= 1 (gir:invoke ((key-string-buffer window) 'get-text-length)))
     (prog1
         (match (gir:invoke ((key-string-buffer window) 'get-text))
           (" " "space")
           ("-" "hyphen")
           (character character))
       (gir:invoke ((key-string-buffer window) 'set-text) ""))))

(define-ffi-method on-signal-key-press-event ((sender gobject-gtk-window) event)
  (let* ((event (gir::build-struct-ptr (gir:nget (gir-gdk *browser*) "EventKey") event))
         (keycode (gir:field event "hardware_keycode"))
         (keyval (gir:field event "keyval"))
         (keyval-name (gir:invoke ((gir-gdk *browser*) 'keyval-name) keyval))
         (character (code-char (gir:invoke ((gir-gdk *browser*) 'keyval-to-unicode) keyval)))
         (printable-value (printable-p sender event))
         (key-string (or printable-value
                         (derive-key-string keyval-name character)))
         (modifiers (funcall (modifier-translator *browser*)
                             (gir:field event "state")
                             event)))
    (if modifiers
        (log:debug key-string keycode character keyval-name modifiers)
        (log:debug key-string keycode character keyval-name))
    (if key-string
        (progn
          (alex:appendf (key-stack sender)
                        (list (keymap:make-key :code keycode
                                               :value key-string
                                               :modifiers modifiers
                                               :status :pressed)))
          (funcall (input-dispatcher sender) event (active-buffer sender) sender printable-value))
        t)))

;; (define-ffi-method on-signal-button-press-event ((sender gobject-gtk-buffer) event)
;;   (let* ((button (gdk:gdk-event-button-button event))
;;          ;; REVIEW: No need to store X and Y?
;;          ;; (x (gdk:gdk-event-button-x event))
;;          ;; (y (gdk:gdk-event-button-y event))
;;          (window (find sender (window-list) :key #'active-buffer))
;;          (key-string (format nil "button~s" button))
;;          (modifiers (funcall (modifier-translator *browser*)
;;                              (button-event-modifiers event)
;;                              event)))
;;     (when key-string
;;       (alex:appendf (key-stack window)
;;                     (list (keymap:make-key
;;                            :value key-string
;;                            :modifiers modifiers
;;                            :status :pressed)))
;;       (funcall (input-dispatcher window) event sender window nil))))

;; (define-ffi-method on-signal-scroll-event ((sender gobject-gtk-buffer) event)
;;   (let* ((button (match (gdk:gdk-event-scroll-direction event)
;;                    (:up 4)
;;                    (:down 5)
;;                    (:left 6)
;;                    (:right 7)
;;                    (:smooth
;;                     (cond
;;                       ((>= 0 (gdk:gdk-event-scroll-delta-y event))
;;                        4)
;;                       ((< 0 (gdk:gdk-event-scroll-delta-y event))
;;                        5)
;;                       ((>= 0 (gdk:gdk-event-scroll-delta-x event))
;;                        6)
;;                       ((< 0 (gdk:gdk-event-scroll-delta-x event))
;;                        7)))))
;;          (window (find sender (window-list) :key #'active-buffer))
;;          (key-string (format nil "button~s" button))
;;          (modifiers (funcall (modifier-translator *browser*)
;;                              (scroll-event-modifiers event)
;;                              event)))
;;     (when key-string
;;       (alex:appendf (key-stack window)
;;                     (list (keymap:make-key
;;                            :value key-string
;;                            :modifiers modifiers
;;                            :status :pressed)))
;;       (funcall (input-dispatcher window) event sender window nil))))

;; (defun make-data-manager (buffer)
;;   (let ((path (expand-path (data-manager-path buffer))))
;;     (apply #'make-instance `(webkit:webkit-website-data-manager
;;                              ,@(when path `(:base-data-directory ,path))
;;                              :is-ephemeral ,(not path)))))

;; (defun make-context (&optional buffer)
;;   (let* ((context (if (and buffer
;;                            ;; Initial window buffer or replacement/temp buffers
;;                            ;; may have no ID.
;;                            (not (str:emptyp (id buffer))))
;;                       (let ((manager (make-data-manager buffer)))
;;                         (make-instance 'webkit:webkit-web-context
;;                                        :website-data-manager manager))
;;                       (web-context *browser*)))
;;          (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
;;     (when (and buffer
;;                (web-buffer-p buffer)
;;                (expand-path (cookies-path buffer)))
;;       (webkit:webkit-cookie-manager-set-persistent-storage
;;        cookie-manager
;;        (expand-path (cookies-path buffer))
;;        :webkit-cookie-persistent-storage-text)
;;       (set-cookie-policy cookie-manager (default-cookie-policy buffer)))
;;     context))


;; (define-ffi-method ffi-buffer-uri ((buffer gobject-gtk-buffer))
;;   (quri:uri (webkit:webkit-web-view-uri (gobject-gtk-object buffer))))

;; (define-ffi-method ffi-buffer-title ((buffer gobject-gtk-buffer))
;;   (or (webkit:webkit-web-view-title (gobject-gtk-object buffer)) ""))

;; (define-ffi-method on-signal-load-failed-with-tls-errors ((buffer gobject-gtk-buffer) certificate url)
;;   "Return nil to propagate further (i.e. raise load-failed signal), T otherwise."
;;   (let* ((context (webkit:webkit-web-view-web-context (gobject-gtk-object buffer)))
;;          (host (quri:uri-host url)))
;;     (if (and (certificate-exceptions buffer)
;;              (member host (certificate-exceptions buffer) :test #'string=))
;;         (progn
;;           (webkit:webkit-web-context-allow-tls-certificate-for-host
;;            context
;;            (gobject:pointer certificate)
;;            host)
;;           (buffer-load url :buffer buffer)
;;           t)
;;         (progn
;;           (tls-help buffer url)
;;           t))))

;; (define-ffi-method on-signal-decide-policy ((buffer gobject-gtk-buffer) response-policy-decision policy-decision-type-response)
;;   (let ((is-new-window nil) (is-known-type t) (event-type :other)
;;         (navigation-action nil) (navigation-type nil)
;;         (mouse-button nil) (modifiers ())
;;         (url nil) (request nil))
;;     (match policy-decision-type-response
;;       (:webkit-policy-decision-type-navigation-action
;;        (setf navigation-type (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision)))
;;       (:webkit-policy-decision-type-new-window-action
;;        (setf navigation-type (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision))
;;        (setf is-new-window t))
;;       (:webkit-policy-decision-type-response
;;        (setf request (webkit:webkit-response-policy-decision-request response-policy-decision))
;;        (setf is-known-type
;;              (webkit:webkit-response-policy-decision-is-mime-type-supported
;;               response-policy-decision))))
;;     ;; Set Event-Type
;;     (setf event-type
;;           (match navigation-type
;;             (:webkit-navigation-type-link-clicked :link-click)
;;             (:webkit-navigation-type-form-submitted :form-submission)
;;             (:webkit-navigation-type-back-forward :backward-or-forward)
;;             (:webkit-navigation-type-reload :reload)
;;             (:webkit-navigation-type-form-resubmitted :form-resubmission)
;;             (_ :other)))
;;     ;; Get Navigation Parameters from WebKitNavigationAction object
;;     (when navigation-type
;;       (setf navigation-action (webkit:webkit-navigation-policy-decision-get-navigation-action
;;                                response-policy-decision))
;;       (setf request (webkit:webkit-navigation-action-get-request navigation-action))
;;       (setf mouse-button (format nil "button~d"
;;                                  (webkit:webkit-navigation-action-get-mouse-button
;;                                   navigation-action)))
;;       (setf modifiers (funcall (modifier-translator *browser*)
;;                                (webkit:webkit-navigation-action-get-modifiers navigation-action))))
;;     (setf url (quri:uri (webkit:webkit-uri-request-uri request)))
;;     (if (null (hooks:handlers (request-resource-hook buffer)))
;;         (progn
;;           (log:debug "Forward to renderer (no request-resource-hook handlers).")
;;           (webkit:webkit-policy-decision-use response-policy-decision)
;;           nil)
;;         (let ((request-data
;;                 (hooks:run-hook
;;                  (request-resource-hook buffer)
;;                  (hooks:run-hook (pre-request-hook buffer)
;;                                  (make-instance 'request-data
;;                                                 :buffer buffer
;;                                                 :url (quri:copy-uri url)
;;                                                 :keys (unless (uiop:emptyp mouse-button)
;;                                                         (list (keymap:make-key
;;                                                                :value mouse-button
;;                                                                :modifiers modifiers)))
;;                                                 :event-type event-type
;;                                                 :new-window-p is-new-window
;;                                                 :known-type-p is-known-type)))))
;;           (cond
;;             ((null request-data)
;;              (log:debug "Don't forward to renderer (null request data).")
;;              (webkit:webkit-policy-decision-ignore response-policy-decision)
;;              nil)
;;             ((and request-data (quri:uri= url (url request-data)))
;;              (log:debug "Forward to renderer (unchanged URL).")
;;              (webkit:webkit-policy-decision-use response-policy-decision)
;;              nil)
;;             (t
;;              (setf (webkit:webkit-uri-request-uri request) (object-string (url request-data)))
;;              (log:debug "Don't forward to renderer (resource request replaced with ~s)."
;;                         (object-display (url request-data)))
;;              ;; Warning: We must ignore the policy decision _before_ we
;;              ;; start the new load request, or else WebKit will be
;;              ;; confused about which URL to load.
;;              (webkit:webkit-policy-decision-ignore response-policy-decision)
;;              (webkit:webkit-web-view-load-request (gobject-gtk-object buffer) request)
;;              nil))))))

;; (define-ffi-method on-signal-load-changed ((buffer gobject-gtk-buffer) load-event)
;;   (sera:and-let* ((url (webkit:webkit-web-view-uri (gobject-gtk-object buffer)))
;;                   ;; `url' can be nil if buffer didn't have any URL associated
;;                   ;; to the web view, e.g. the start page.
;;                   (url (quri:uri url)))
;;     (cond ((eq load-event :webkit-load-started)
;;            (setf (slot-value buffer 'load-status) :loading)
;;            (print-status nil (get-containing-window-for-buffer buffer *browser*))
;;            (echo "Loading ~s." (object-display url)))
;;           ((eq load-event :webkit-load-redirected) nil)
;;           ;; WARNING: load-committed may be deprecated (reference?).  Prefer load-status and load-finished.
;;           ((eq load-event :webkit-load-committed)
;;            (on-signal-load-committed buffer url))
;;           ((eq load-event :webkit-load-finished)
;;            (setf (slot-value buffer 'load-status) :finished)
;;            (on-signal-load-finished buffer url)
;;            (print-status nil (get-containing-window-for-buffer buffer *browser*))
;;            (echo "Finished loading ~s." (object-display url))))))

;; (define-ffi-method on-signal-mouse-target-changed ((buffer gobject-gtk-buffer) hit-test-result modifiers)
;;   (declare (ignore modifiers))
;;   (match (cond ((webkit:webkit-hit-test-result-link-uri hit-test-result)
;;                 (webkit:webkit-hit-test-result-link-uri hit-test-result))
;;                ((webkit:webkit-hit-test-result-image-uri hit-test-result)
;;                 (webkit:webkit-hit-test-result-image-uri hit-test-result))
;;                ((webkit:webkit-hit-test-result-media-uri hit-test-result)
;;                 (webkit:webkit-hit-test-result-media-uri hit-test-result)))
;;     (nil (print-message "")
;;          (setf (url-at-point buffer) (quri:uri "")))
;;     (url (print-message (str:concat "→ " (quri:url-decode url :lenient t)))
;;          (setf (url-at-point buffer) (quri:uri url)))))

(define-ffi-method ffi-window-make ((browser gobject-gtk-browser))
  "Make a window."
  (make-instance 'user-window))

;; (define-ffi-method ffi-window-to-foreground ((window gobject-gtk-window))
;;   "Show window in foreground."
;;   (gobject-gtk:gobject-gtk-window-present (gobject-gtk-object window))
;;   (setf (slot-value *browser* 'last-active-window) window))

(define-ffi-method ffi-window-set-title ((window gobject-gtk-window) title)
  "Set the title for a window."
  (gir:invoke ((gtk-object window) 'set-title) title))

;; (define-ffi-method ffi-window-active ((browser gobject-gtk-browser))
;;   "Return the window object for the currently active window."
;;   (setf (slot-value browser 'last-active-window)
;;         (or (find-if #'gobject-gtk:gobject-gtk-window-is-active (window-list) :key #'gobject-gtk-object)
;;             (first (window-list))
;;             (slot-value browser 'last-active-window))))

;; (define-ffi-method ffi-window-set-active-buffer ((window gobject-gtk-window) (buffer gobject-gtk-buffer))
;;   "Set BROWSER's WINDOW buffer to BUFFER. "
;;   (gobject-gtk:gobject-gtk-container-remove (box-layout window) (gobject-gtk-object (active-buffer window)))
;;   (gobject-gtk:gobject-gtk-box-pack-start (box-layout window) (gobject-gtk-object buffer) :expand t)
;;   (gobject-gtk:gobject-gtk-widget-show (gobject-gtk-object buffer))
;;   buffer)

(define-ffi-method ffi-window-set-minibuffer-height ((window gobject-gtk-window) height)
  (gir:invoke ((minibuffer-container window) 'set-size-request) -1 height))

(define-ffi-method ffi-window-get-status-buffer-height ((window gobject-gtk-window))
  (gir:property (status-container window) 'height-request))

(define-ffi-method ffi-window-set-status-buffer-height ((window gobject-gtk-window) height)
  (gir:invoke ((status-container window) 'set-size-request) -1 height))

(define-ffi-method ffi-window-get-message-buffer-height ((window gobject-gtk-window))
  (gir:property (message-container window) 'height-request))



(define-ffi-method ffi-buffer-make ((buffer gobject-gtk-buffer))
  "Initialize BUFFER's GOBJECT-GTK web view."
  (setf (gtk-object buffer) 
        (gir:invoke ((gir-webkit *browser*) "WebView" 'new)))
  (gir:invoke ((gtk-object buffer) 'load_uri) "https://example.com")
  ;; (ffi-buffer-enable-smooth-scrolling buffer t)
  ;; (gobject:g-signal-connect
  ;;  (gobject-gtk-object buffer) "decide-policy"
  ;;  (lambda (web-view response-policy-decision policy-decision-type-response)
  ;;    (declare (ignore web-view))
  ;;    (on-signal-decide-policy buffer response-policy-decision policy-decision-type-response)))
  ;; (gobject:g-signal-connect
  ;;  (gobject-gtk-object buffer) "load-changed"
  ;;  (lambda (web-view load-event)
  ;;    (declare (ignore web-view))
  ;;    (on-signal-load-changed buffer load-event)))
  ;; (gobject:g-signal-connect
  ;;  (gobject-gtk-object buffer) "mouse-target-changed"
  ;;  (lambda (web-view hit-test-result modifiers)
  ;;    (declare (ignore web-view))
  ;;    (on-signal-mouse-target-changed buffer hit-test-result modifiers)))
  ;; ;; Mouse events are captured by the web view first, so we must intercept them here.
  ;; (gobject:g-signal-connect
  ;;  (gobject-gtk-object buffer) "button-press-event"
  ;;  (lambda (web-view event) (declare (ignore web-view))
  ;;    (on-signal-button-press-event buffer event)))
  ;; (gobject:g-signal-connect
  ;;  (gobject-gtk-object buffer) "scroll-event"
  ;;  (lambda (web-view event) (declare (ignore web-view))
  ;;    (on-signal-scroll-event buffer event)))
  ;; ;; TLS certificate handling
  ;; (gobject:g-signal-connect
  ;;  (gobject-gtk-object buffer) "load-failed-with-tls-errors"
  ;;  (lambda (web-view failing-uri certificate errors)
  ;;    (declare (ignore web-view errors))
  ;;    (on-signal-load-failed-with-tls-errors buffer certificate (quri:uri failing-uri))))
  ;; (gobject:g-signal-connect
  ;;  (gobject-gtk-object buffer) "notify::uri"
  ;;  (lambda (web-view param-spec)
  ;;    (declare (ignore web-view param-spec))
  ;;    (on-signal-notify-uri buffer nil)))
  ;; (gobject:g-signal-connect
  ;;  (gobject-gtk-object buffer) "notify::title"
  ;;  (lambda (web-view param-spec)
  ;;    (declare (ignore web-view param-spec))
  ;;    (on-signal-notify-title buffer nil)))
  buffer)

;; (define-ffi-method ffi-buffer-delete ((buffer gobject-gtk-buffer))
;;   (gobject-gtk:gobject-gtk-widget-destroy (gobject-gtk-object buffer)))

;; (define-ffi-method ffi-buffer-load ((buffer gobject-gtk-buffer) uri)
;;   "Load URI in BUFFER.
;; An optimization technique is to make use of the renderer history cache.
;; For WebKit, if the URL matches an entry in the webkit-history then we fetch the
;; page from the cache.

;; We don't use the cache if URI matches BUFFER's URL since this means the user
;; requested a reload."
;;   ;; (declare (type quri:uri uri))
;;   (let* ((history (webkit-history buffer))
;;          (entry (or (find uri history :test #'quri:uri= :key #'webkit-history-entry-uri)
;;                     (find uri history :test #'quri:uri= :key #'webkit-history-entry-original-uri))))
;;     (if (and entry (not (quri:uri= uri (url buffer))))
;;         (progn
;;           (log:debug "Load URL from history entry ~a" entry)
;;           (load-webkit-history-entry buffer entry))
;;         (webkit:webkit-web-view-load-uri (gobject-gtk-object buffer) (object-string uri)))))

;; (defmethod ffi-buffer-evaluate-javascript ((buffer gobject-gtk-buffer) javascript)
;;   (%within-renderer-thread
;;    (lambda (&optional channel)
;;      (webkit2:webkit-web-view-evaluate-javascript
;;       (gobject-gtk-object buffer)
;;       javascript
;;       (if channel
;;           (lambda (result)
;;             (calispel:! channel result))
;;           #'identity)
;;       #'javascript-error-handler))))

;; (defmethod ffi-buffer-evaluate-javascript-async ((buffer gobject-gtk-buffer) javascript)
;;   (%within-renderer-thread-async
;;    (lambda ()
;;      (webkit2:webkit-web-view-evaluate-javascript
;;       (gobject-gtk-object buffer)
;;       javascript
;;       nil
;;       #'javascript-error-handler))))

;; (define-ffi-method ffi-minibuffer-evaluate-javascript ((window gobject-gtk-window) javascript)
;;   (webkit2:webkit-web-view-evaluate-javascript (minibuffer-view window) javascript))

;; (define-ffi-method ffi-buffer-enable-javascript ((buffer gobject-gtk-buffer) value)
;;   (setf (webkit:webkit-settings-enable-javascript
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer)))
;;         value))

;; (define-ffi-method ffi-buffer-enable-javascript-markup ((buffer gobject-gtk-buffer) value)
;;   (setf (webkit:webkit-settings-enable-javascript-markup
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer)))
;;         value))

;; (define-ffi-method ffi-buffer-enable-smooth-scrolling ((buffer gobject-gtk-buffer) value)
;;   (setf (webkit:webkit-settings-enable-smooth-scrolling
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer)))
;;         value))

;; #+webkit2-media
;; (define-ffi-method ffi-buffer-enable-media ((buffer gobject-gtk-buffer) value)
;;   (setf (webkit:webkit-settings-enable-media
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer)))
;;         value))

;; (define-ffi-method ffi-buffer-auto-load-image ((buffer gobject-gtk-buffer) value)
;;   (setf (webkit:webkit-settings-auto-load-images
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer)))
;;         value))

;; #+webkit2-mute
;; (defmethod ffi-buffer-enable-sound ((buffer gobject-gtk-buffer) value)
;;   (webkit:webkit-web-view-set-is-muted (gobject-gtk-object buffer) value))

;; (define-ffi-method ffi-buffer-user-agent ((buffer gobject-gtk-buffer) value)
;;   (setf (webkit:webkit-settings-user-agent
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer)))
;;         value))

;; (define-ffi-method ffi-buffer-webgl-enabled-p ((buffer gobject-gtk-buffer))
;;   (webkit:webkit-settings-enable-webgl
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer))))

;; (define-ffi-method ffi-buffer-enable-webgl ((buffer gobject-gtk-buffer) value)
;;   (setf (webkit:webkit-settings-enable-webgl
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer)))
;;         value))

;; (define-ffi-method ffi-buffer-set-proxy ((buffer gobject-gtk-buffer)
;;                                  &optional (proxy-uri (quri:uri ""))
;;                                    (ignore-hosts (list nil)))
;;   "Redirect network connections of BUFFER to proxy server PROXY-URI.
;; Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
;; For the user-level interface, see `proxy-mode'.

;; Note: WebKit supports three proxy 'modes': default (the system proxy),
;; custom (the specified proxy) and none."
;;   (declare (type quri:uri proxy-uri))
;;   (setf (proxy-uri buffer) proxy-uri)
;;   (setf (proxy-ignored-hosts buffer) ignore-hosts)
;;   (let* ((context (webkit:webkit-web-view-web-context (gobject-gtk-object buffer)))
;;          (settings (cffi:null-pointer))
;;          (mode :webkit-network-proxy-mode-no-proxy)
;;          (ignore-hosts (cffi:foreign-alloc :string
;;                                            :initial-contents ignore-hosts
;;                                            :null-terminated-p t)))
;;     (unless (url-empty-p proxy-uri)
;;       (setf mode :webkit-network-proxy-mode-custom)
;;       (setf settings
;;             (webkit:webkit-network-proxy-settings-new
;;              (quri:render-uri proxy-uri)
;;              ignore-hosts)))
;;     (cffi:foreign-free ignore-hosts)
;;     (webkit:webkit-web-context-set-network-proxy-settings
;;      context mode settings)))

;; (define-ffi-method ffi-buffer-get-proxy ((buffer gobject-gtk-buffer))
;;   "Return the proxy URI and list of ignored hosts (a list of strings) as second value."
;;   (the (values (or quri:uri null) list-of-strings)
;;        (values (proxy-uri buffer)
;;                (proxy-ignored-hosts buffer))))

;; (define-ffi-method ffi-generate-input-event ((window gobject-gtk-window) event)
;;   ;; The "send_event" field is used to mark the event as an "unconsumed"
;;   ;; keypress.  The distinction allows us to avoid looping indefinitely.
;;   (etypecase event
;;     (gdk:gdk-event-button
;;      (setf (gdk:gdk-event-button-send-event event) t))
;;     (gdk:gdk-event-key
;;      (setf (gdk:gdk-event-key-send-event event) t))
;;     (gdk:gdk-event-scroll
;;      (setf (gdk:gdk-event-scroll-send-event event) t)))
;;   (gobject-gtk:gobject-gtk-main-do-event event))

(define-ffi-method ffi-generated-input-event-p ((window gobject-gtk-window) event)
  nil
  ;(gdk:gdk-event-send-event event)
  )

;; (define-ffi-method ffi-inspector-show ((buffer gobject-gtk-buffer))
;;   (setf (webkit:webkit-settings-enable-developer-extras
;;          (webkit:webkit-web-view-get-settings (gobject-gtk-object buffer)))
;;         t)
;;   (webkit:webkit-web-inspector-show
;;    (webkit:webkit-web-view-get-inspector (gobject-gtk-object buffer))))

;; (define-ffi-method ffi-print-status ((window gobject-gtk-window) text)
;;   (let ((text (markup:markup
;;                (:head (:style (style (status-buffer window))))
;;                (:body (markup:raw text)))))
;;     (with-slots (status-buffer) window
;;       (webkit2:webkit-web-view-evaluate-javascript
;;        (gobject-gtk-object (status-buffer window))
;;        (ps:ps (setf (ps:@ document Body |innerHTML|) ; TODO: Rename all "Body" to "body".
;;                     (ps:lisp text)))))))

;; (define-ffi-method ffi-print-message ((window gobject-gtk-window) text)
;;   (let ((text (markup:markup
;;                (:head (:style (message-buffer-style window)))
;;                (:body (markup:raw text)))))
;;     (with-slots (message-view) window
;;       (webkit2:webkit-web-view-evaluate-javascript
;;        (message-view window)
;;        (ps:ps (setf (ps:@ document Body |innerHTML|)
;;                     (ps:lisp text)))))))

;; (define-ffi-method ffi-display-uri (text)
;;   (webkit:webkit-uri-for-display text))

;; (declaim (ftype (function (webkit:webkit-cookie-manager cookie-policy)) set-cookie-policy))
;; (defun set-cookie-policy (cookie-manager cookie-policy)
;;   (webkit:webkit-cookie-manager-set-accept-policy
;;    cookie-manager
;;    (match cookie-policy
;;      (:accept :webkit-cookie-policy-accept-always)
;;      (:never :webkit-cookie-policy-accept-never)
;;      (:no-third-party :webkit-cookie-policy-accept-no-third-party))))

;; (define-ffi-method ffi-buffer-cookie-policy ((buffer gobject-gtk-buffer) value)
;;   "VALUE is one of`:always', `:never' or `:no-third-party'."
;;   (let* ((context (webkit:webkit-web-view-web-context (gobject-gtk-object buffer)))
;;          (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
;;     (set-cookie-policy cookie-manager value)
;;     buffer))

;; (defmethod ffi-set-preferred-languages ((buffer gobject-gtk-buffer)
;;                                         language-list)
;;   "Set the list of preferred languages in the HTTP header \"Accept-Language:\".
;; LANGUAGE is a list of strings like '(\"en_US\" \"fr_FR\")."
;;   (let ((langs (cffi:foreign-alloc :string
;;                                    :initial-contents language-list
;;                                    :null-terminated-p t)))
;;     (webkit:webkit-web-context-set-preferred-languages
;;      (webkit:webkit-web-view-web-context (gobject-gtk-object buffer))
;;      langs)))

;; (defstruct webkit-history-entry
;;   title
;;   uri
;;   original-uri
;;   gobject-gtk-object)

;; (define-ffi-method webkit-history ((buffer gobject-gtk-buffer))
;;   "Return a list of `webkit-history-entry's for the current buffer.
;; Oldest entries come last.

;; This represents the history as remembered by WebKit.  Note that it is linear so
;; it does not map 1:1 with Nyxt's history tree.  Nonetheless it allows us to make
;; use of the WebKit history case for the current branch.  See `ffi-buffer-load'.

;; As a second value, return the current buffer index starting from 0."
;;   (let* ((bf-list (webkit:webkit-web-view-get-back-forward-list (gobject-gtk-object buffer)))
;;          (length (webkit:webkit-back-forward-list-get-length bf-list))
;;          (current (webkit:webkit-back-forward-list-get-current-item bf-list))
;;          (history-list nil)
;;          (current-index 0))
;;     ;; The back-forward list is both negatively and positibely indexed.  Seems
;;     ;; that we can't easily know the first index nor the last one.  So let's
;;     ;; iterate over the length backwards and forwards to make sure we get all
;;     ;; elements in order.
;;     (loop for i from (- length) to length
;;           for item = (webkit:webkit-back-forward-list-get-nth-item bf-list i)
;;           when (eq item current)
;;             do (setf current-index (- length (length history-list))) ; Index from 0.
;;           when item
;;             do (push (make-webkit-history-entry
;;                       :title (webkit:webkit-back-forward-list-item-get-title item)
;;                       :uri (quri:uri (webkit:webkit-back-forward-list-item-get-uri item))
;;                       :original-uri (quri:uri (webkit:webkit-back-forward-list-item-get-original-uri item))
;;                       :gobject-gtk-object item)
;;                      history-list))
;;     (values history-list current-index)))

;; (defmethod load-webkit-history-entry ((buffer gobject-gtk-buffer) history-entry)
;;   (webkit:webkit-web-view-go-to-back-forward-list-item
;;    (gobject-gtk-object buffer)
;;    (webkit-history-entry-gobject-gtk-object history-entry)))


(define-user-class window (gobject-gtk-window))
(define-user-class buffer (gobject-gtk-buffer))
(define-user-class browser (gobject-gtk-browser))
