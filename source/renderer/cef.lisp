;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/renderer/cef
  (:documentation "CEF renderer."))
(in-package :nyxt/renderer/cef)

(define-class cef-renderer (renderer)
  ((name "CEF"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Chromium Embedded Framework renderer class."))

(setf nyxt::*renderer* (make-instance 'cef-renderer))
(pushnew :nyxt-cef *features*)

(defmethod renderer-thread-p ((renderer cef-renderer) &optional (thread (bt:current-thread)))
  (and (cef:on-ui-thread-p)
       (eq thread (bt:current-thread))))

(defmethod install ((renderer cef-renderer))
  (flet ((set-superclasses (renderer-class-sym+superclasses)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class (first renderer-class-sym+superclasses)
                                     :direct-superclasses (rest renderer-class-sym+superclasses)
                                     :metaclass 'interface-class))))
    (mapc #'set-superclasses '((renderer-browser cef-browser)
                               (renderer-scheme cef-scheme)
                               (renderer-window cef-window)
                               (renderer-buffer cef-buffer)
                               (nyxt/mode/download:renderer-download cef-download)
                               (nyxt/mode/user-script:renderer-user-style cef-user-style)
                               (nyxt/mode/user-script:renderer-user-script cef-user-script)))))

(defmethod uninstall ((renderer cef-renderer))
  (flet ((remove-superclasses (renderer-class-sym)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class renderer-class-sym
                                     :direct-superclasses '()
                                     :metaclass 'interface-class))))
    (mapc #'remove-superclasses '(renderer-browser
                                  renderer-scheme
                                  renderer-window
                                  renderer-buffer
                                  nyxt/mode/user-script:renderer-user-style
                                  nyxt/mode/user-script:renderer-user-script))))

(defvar +scheme-privileges+
  "{standard:true,secure:true,corsEnabled:true,supportFetchAPI:true}"
  "Privilege string for Nyxt's internal schemes.
`standard' lets CEF parse the URLs. `lisp' pages also need CORS and fetch.")

(define-class cef-scheme (cef:protocol)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "CEF scheme class."))

(defmethod initialize-instance :after ((scheme cef-scheme) &key)
  (setf (slot-value scheme 'cef:scheme-name)
        (name scheme)))

(defmethod ffi-register-custom-scheme ((scheme cef-scheme))
  (cef:handle-callback
   scheme
   (lambda (url)
     (handler-case
         (if (callback scheme)
             (multiple-value-bind (body mime status headers reason)
                 (funcall (callback scheme) url)
               ;; An empty document has no element for `html-write' to fill.
               (if (and (stringp body) (string= body ""))
                   (values "<html><head></head><body></body></html>"
                           (or mime "text/html") status headers reason)
                   (values body mime status headers reason)))
             (values "" "text/plain" 404 nil "Not Found"))
       (condition (condition)
         (when (error-callback scheme)
           (funcall (error-callback scheme) condition))
         (error-help "Scheme error" (princ-to-string condition)))))))

(define-class cef-browser (cef:interface)
  ((adblocking-enabled-p
    t
    :documentation "When non-nil, load uBlock Origin's default lists and block matching requests."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "CEF browser class."))

(defmethod ffi-initialize ((browser cef-browser) urls startup-timestamp)
  (declare (ignore urls startup-timestamp))
  (log:debug "Initializing CEF Interface")
  (let ((interface (make-instance 'cef:interface)))
    (setf cef:*interface* interface)
    ;; Scheme names and privilege bits are fixed at CEF startup. Handlers are
    ;; attached to the same objects once the UI thread is in its message loop.
    (setf (cef:protocols interface)
          (loop for name being the hash-key of nyxt::*schemes*
                collect (make-instance 'cef:protocol
                                       :scheme-name name
                                       :privileges +scheme-privileges+)))
    ;; CEF 150's Wayland backend crashes when Vulkan is the GL implementation.
    (when (and (uiop:getenv "WAYLAND_DISPLAY")
               (not (some (lambda (option)
                            (and (stringp option)
                                 (search "--ozone-platform" option)))
                          (cef:launch-options interface))))
      (setf (cef:launch-options interface)
            (append (cef:launch-options interface)
                    '("--ozone-platform=x11"))))
    (cef:launch interface)
    (when (adblocking-enabled-p browser)
      (cef:default-block (make-instance 'cef:adblocker)))
    (maphash (lambda (scheme-name callbacks)
               (ffi-register-custom-scheme
                (make-instance 'scheme
                               :name scheme-name
                               :callback (first callbacks)
                               :error-callback (second callbacks))))
             nyxt::*schemes*)
    (let ((session (cef:default-session interface)))
      (cef:add-listener session :download-item-updated
                        (lambda (session item)
                          (declare (ignore session))
                          (download-item-updated item)))))
  (call-next-method)
  (unless nyxt::*run-from-repl-p*
    (cef:join)
    (uiop:quit (nyxt:exit-code browser) #+bsd nil)))

(defmethod ffi-kill-browser ((browser cef-browser))
  (declare (ignore browser))
  (cef:terminate))

(define-class cef-download ()
  ((remote-object))
  (:documentation "CEF download class."))

(defmethod update-status ((download cef-download))
  (setf (nyxt/mode/download:status download)
        (match (cef:state (remote-object download))
          ("completed" :finished)
          ("progressing" :loading)
          ("cancelled" :canceled)
          ("interrupted" :failed))))

(defun download-item-updated (download-item)
  (let ((download (find download-item (downloads *browser*) :key #'remote-object)))
    (if download
        (progn
          (setf (url download) (cef:url (remote-object download))
                (nyxt/mode/download:bytes-downloaded download)
                (cef:received-bytes (remote-object download))
                (nyxt/mode/download:completion-percentage download)
                (float (cef:percent-complete (remote-object download)) 1.0)
                (nyxt/mode/download:destination-path download)
                (uiop:parse-native-namestring
                 (or (cef:save-path (remote-object download)) "")))
          (update-status download))
        (let ((download (make-instance 'nyxt/mode/download:download
                                       :remote-object download-item
                                       :url (cef:url download-item))))
          (push download (downloads *browser*))
          (setf (nyxt/mode/download::cancel-function download)
                (lambda ()
                  (cef:cancel (remote-object download))))
          (update-status download)))))

(define-class cef-user-style ()
  ((style-id
    nil
    :documentation "Identifier returned by `cef:insert-css'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "CEF user style sheet."))

(define-class cef-user-script ()
  ((script-id
    nil
    :documentation "Identifier returned by `cef:add-user-script'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "CEF user script. Scripts run in the page world."))

(define-class cef-buffer (cef:view)
  ((modifier-plist
    '(:shift "shift"
      :control "control"
      :alt "meta"
      :meta "super")
    :type list
    :documentation "A map between CEF's and Nyxt's terminology for modifier keys.
Note that by changing the default value, modifier keys can be remapped.")
   (set-height
    nil
    :documentation "The height the buffer has been requested to be set to.
It does not represent the current height as reported by the renderer, or
the default height.")
   (listeners-p
    nil
    :documentation "Whether input and load listeners have been registered.")
   (cef-proxy-url
    (quri:uri "")
    :documentation "Proxy URL last applied to this buffer.")
   (cef-proxy-ignored-hosts
    '()
    :documentation "Hosts that bypass the proxy.")
   (smooth-scrolling-p
    t
    :documentation "Remembered smooth-scrolling setting. CEF has no matching preference.")
   (media-enabled-p
    t
    :documentation "Remembered media setting. CEF has no matching preference."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "CEF buffer class."))

(defun panel-buffer-p (buffer)
  "Status, message, and prompt views share the window with the page view."
  (or (status-buffer-p buffer)
      (message-buffer-p buffer)
      (prompt-buffer-p buffer)))

(defmethod initialize-instance :around ((buffer cef-buffer) &rest initargs &key &allow-other-keys)
  ;; Every view is Alloy. A Chrome window hosts one Chrome BrowserView and
  ;; any number of Alloy ones. A second Chrome page opens as its own
  ;; Chromium window, and that view cannot be moved into this window.
  (apply #'call-next-method buffer :alloy-p t initargs))

(defmethod customize-instance :after ((buffer cef-buffer) &key &allow-other-keys)
  (ffi-buffer-initialize-foreign-object buffer))
;; The native view is already created at about:blank. Loading that URL again
;; before the view is in a window re-enters CEF and exits the message loop.

(defmethod initialize-window-open-handler ((buffer cef-buffer))
  ;; target=_blank and window.open become a Nyxt buffer. The popup itself is
  ;; cancelled on the UI thread; the buffer is created afterwards so view
  ;; creation is not nested inside OnBeforePopup.
  (cef:override-window-open-handler
   (cef:web-contents buffer)
   (lambda (object data)
     (declare (ignore object))
     (let ((url (assoc-value data :url)))
       (run-thread "window-open"
         (make-buffer-focus :url (quri:uri url)))))))

(defun event-modifiers (event)
  (delete nil (list (when (assoc-value event :shift) :shift)
                    (when (assoc-value event :control) :control)
                    (when (assoc-value event :alt) :alt)
                    (when (assoc-value event :meta) :meta))))

(defun event-url (data)
  (quri:uri (or (assoc-value data :url) "")))

(defmethod initialize-listeners ((buffer cef-buffer))
  (cef:add-listener buffer :before-input-event
                    (lambda (buffer event)
                      (on-signal-key-press-event buffer event)))
  (cef:add-listener buffer :button-press
                    (lambda (buffer event)
                      (on-signal-button-press-event buffer event)))
  (cef:add-listener buffer :scroll
                    (lambda (buffer event)
                      (on-signal-scroll-event buffer event)))
  (when (web-buffer-p buffer)
    (cef:add-listener buffer :hover
                      (lambda (buffer data)
                        (on-signal-hover buffer data)))
    (cef:add-listener buffer :did-start-loading
                      (lambda (buffer data)
                        (setf (nyxt::status buffer) :loading)
                        (let ((url (event-url data)))
                          (run-thread "load-started"
                            (on-signal-load-started buffer url)))))
    (cef:add-listener buffer :did-redirect-navigation
                      (lambda (buffer data)
                        (let ((url (event-url data)))
                          (run-thread "load-redirected"
                            (setf (url buffer) url)
                            (on-signal-load-redirected buffer url)))))
    (cef:add-listener buffer :did-finish-load
                      (lambda (buffer data)
                        (setf (nyxt::status buffer) :finished)
                        (let ((url (event-url data)))
                          (run-thread "load-finished"
                            (setf (url buffer) url)
                            (on-signal-load-finished buffer url (ffi-buffer-title buffer))))))
    (cef:add-listener buffer :did-fail-load
                      (lambda (buffer data)
                        ;; ERR_ABORTED (-3) is a navigation that was replaced.
                        (unless (eql (assoc-value data :error-code) -3)
                          (setf (nyxt::status buffer) :failed)
                          (let ((url (event-url data)))
                            (run-thread "load-failed"
                              (on-signal-load-failed buffer url))))))
    (cef:add-listener buffer :address-change
                      (lambda (buffer data)
                        (let ((url (event-url data)))
                          (run-thread "notify-uri"
                            (setf (url buffer) url)
                            (on-signal-notify-uri buffer url)))))
    (cef:add-listener buffer :page-title-updated
                      (lambda (buffer data)
                        (let ((title (or (assoc-value data :title) "")))
                          (run-thread "notify-title"
                            (on-signal-notify-title buffer title)))))
    (cef:add-listener buffer :fullscreen
                      (lambda (buffer data)
                        (declare (ignore buffer))
                        (run-thread "fullscreen"
                          (if (assoc-value data :fullscreen)
                              (progn
                                (ffi-window-fullscreen (current-window) :user-event-p nil)
                                (disable-message-buffer (current-window))
                                (disable-status-buffer (current-window)))
                              (progn
                                (ffi-window-unfullscreen (current-window) :user-event-p nil)
                                (enable-message-buffer (current-window))
                                (enable-status-buffer (current-window)))))))
    (unless (member (type-of buffer) '(status-buffer message-buffer prompt-buffer))
      (cef:add-listener buffer :context-menu
                        (lambda (buffer data)
                          (declare (ignore data))
                          (populate-context-menu buffer)))
      (cef:add-listener buffer :context-menu-command
                        (lambda (buffer data)
                          (declare (ignore buffer))
                          (run-context-menu-command data))))))

(defmethod ffi-buffer-initialize-foreign-object ((buffer cef-buffer))
  (let ((url (if (and (slot-boundp buffer 'url)
                      (url buffer)
                      (not (url-empty-p (url buffer))))
                 (render-url (url buffer))
                 "about:blank")))
    (cef:ensure-view buffer :url url))
  (unless (listeners-p buffer)
    (initialize-listeners buffer)
    (initialize-window-open-handler buffer)
    (setf (listeners-p buffer) t))
  (when (and *browser* (web-buffer-p buffer))
    (setf (ffi-buffer-smooth-scrolling-enabled-p buffer) (smooth-scrolling buffer))
    (setf (ffi-buffer-cookie-policy buffer) (default-cookie-policy *browser*)))
  buffer)

(defmethod ffi-buffer-delete ((buffer cef-buffer))
  (when (cef:web-contents buffer)
    (if (and (window buffer) (typep (window buffer) 'cef-window))
        (cef:remove-view (window buffer) buffer :kill-view-p t)
        (cef:kill buffer)))
  (setf (listeners-p buffer) nil))

(defmethod ffi-buffer-url ((buffer cef-buffer))
  (quri:uri (cef:get-url buffer)))

(defmethod ffi-buffer-title ((buffer cef-buffer))
  (cef:get-title buffer))

(defmethod ffi-buffer-load ((buffer cef-buffer) url)
  (when (web-buffer-p buffer)
    (setf (nyxt::status buffer) :loading))
  (cef:load-url buffer (render-url url)))

(defmethod ffi-buffer-reload ((buffer cef-buffer))
  (cef:reload buffer)
  buffer)

(defmethod ffi-buffer-load-alternate-html ((buffer cef-buffer) html-content content-url url)
  (declare (ignore content-url url))
  (cef:load-html buffer html-content))

(defmethod ffi-buffer-zoom-ratio ((buffer cef-buffer))
  (cef:get-zoom-factor (cef:web-contents buffer)))

(defmethod (setf ffi-buffer-zoom-ratio) (value (buffer cef-buffer))
  (if (and (floatp value) (plusp value))
      (cef:set-zoom-factor (cef:web-contents buffer) value)
      (echo-warning "Zoom ratio must be a positive floating point number.")))

(defmethod ffi-buffer-evaluate-javascript ((buffer cef-buffer) javascript
                                           &optional world-name)
  (declare (ignore world-name))
  (if-let ((contents (cef:web-contents buffer)))
    (cef:execute-javascript-synchronous contents javascript)
    (warn "CEF buffer ~a has no web view; skipping JavaScript." buffer)))

(defmethod ffi-buffer-evaluate-javascript-async ((buffer cef-buffer) javascript
                                                 &optional world-name)
  (declare (ignore world-name))
  (if-let ((contents (cef:web-contents buffer)))
    (cef:execute-javascript contents javascript)
    (warn "CEF buffer ~a has no web view; skipping JavaScript." buffer)))

(defmethod ffi-inspector-show ((buffer cef-buffer))
  (cef:open-dev-tools buffer))

(defmethod ffi-focused-p ((buffer cef-buffer))
  (cef:is-focused buffer))

(defmethod ffi-buffer-copy ((buffer cef-buffer) &optional (text nil text-provided-p))
  (if text-provided-p
      (setf (clipboard-text) text)
      (let ((selection (nyxt::buffer-selection-text buffer)))
        ;; Copy runs first so the page sees the event. CEF does not export a
        ;; clipboard of its own, so the text is published here afterwards.
        (cef:copy (cef:web-contents buffer))
        (if (and (stringp selection) (not (uiop:emptyp selection)))
            (setf (clipboard-text) selection)
            (clipboard-text)))))

(defmethod ffi-buffer-paste ((buffer cef-buffer) &optional (text nil text-provided-p))
  (let ((text (if text-provided-p text (clipboard-text))))
    (if text
        (cef:insert-text (cef:web-contents buffer) text)
        (cef:paste (cef:web-contents buffer)))
    text))

(defmethod ffi-buffer-cut ((buffer cef-buffer))
  (let ((selection (nyxt::buffer-selection-text buffer)))
    (cef:cut (cef:web-contents buffer))
    (when (and (stringp selection) (not (uiop:emptyp selection)))
      (setf (clipboard-text) selection))
    selection))

(defmethod ffi-buffer-select-all ((buffer cef-buffer))
  (cef:select-all (cef:web-contents buffer)))

(defmethod ffi-buffer-undo ((buffer cef-buffer))
  (cef:undo (cef:web-contents buffer)))

(defmethod ffi-buffer-redo ((buffer cef-buffer))
  (cef:redo (cef:web-contents buffer)))

(defmethod ffi-buffer-navigate-backwards ((buffer cef-buffer))
  (cef:go-back (cef:web-contents buffer)))

(defmethod ffi-buffer-navigate-forwards ((buffer cef-buffer))
  (cef:go-forward (cef:web-contents buffer)))

(defun update-active-buffer-bounds (window delta)
  "Recalculate the bounds of the active window to compensate for changes in the
height of the status/prompt/message buffer."
  (when (active-buffer window)
    (let ((window-bounds (cef:get-bounds window)))
      (cef:set-bounds (active-buffer window)
                      :x 0
                      :y 0
                      :width (assoc-value window-bounds :width)
                      :height (- (assoc-value window-bounds :height)
                                 (+ delta
                                    (ffi-height (status-buffer window))
                                    (ffi-height (message-buffer window))))))))

(defmethod ffi-height ((buffer cef-buffer))
  (assoc-value (cef:get-bounds buffer) :height))

(defmethod (setf ffi-height) ((height integer) (buffer cef-buffer))
  (setf (set-height buffer) height)
  (if (panel-buffer-p buffer)
      ;; A preferred height of 0 is an empty size, which the box layout
      ;; ignores, so the bar stays on screen. `set-bar-height' hides it.
      (cef:set-bar-height buffer height)
      (let ((bounds (cef:get-bounds buffer)))
        (cef:set-bounds buffer
                        :x (assoc-value bounds :x)
                        :y (assoc-value bounds :y)
                        :width (assoc-value bounds :width)
                        :height height)
        (when (window buffer)
          (update-active-buffer-bounds (window buffer) 0)))))

(defmethod ffi-focus-buffer ((buffer cef-buffer))
  (cef:focus buffer)
  buffer)

(defmethod (setf ffi-height) ((height integer) (prompt-buffer prompt-buffer))
  (with-slots (window) prompt-buffer
    (setf (set-height prompt-buffer) height)
    (cef:set-bar-height prompt-buffer height)
    ;; Child 0 is the page. Insert the prompt under it once. Attaching can
    ;; replace the document the skeleton just wrote, so draw it again after
    ;; the view is actually in the window and before the input is read back.
    (unless (find prompt-buffer (cef:views window))
      (cef:add-view-at window prompt-buffer 1)
      (nyxt::prompt-render-skeleton prompt-buffer)
      (nyxt::prompt-render-focus prompt-buffer))))

(defmethod ffi-width ((buffer cef-buffer))
  (assoc-value (cef:get-bounds buffer) :width))

(defmethod (setf ffi-width) (width (buffer cef-buffer))
  (let ((bounds (cef:get-bounds buffer)))
    (cef:set-bounds buffer
                    :x (assoc-value bounds :x)
                    :y (assoc-value bounds :y)
                    :width width
                    :height (assoc-value bounds :height))))

(defmethod ffi-buffer-sound-enabled-p ((buffer cef-buffer))
  (not (cef:muted-p (cef:web-contents buffer))))
(defmethod (setf ffi-buffer-sound-enabled-p) (value (buffer cef-buffer))
  (cef:set-audio-muted (cef:web-contents buffer) (not value)))

(defmethod ffi-buffer-download ((buffer cef-buffer) url)
  (cef:download-url (cef:web-contents buffer) url))

(defmethod ffi-buffer-add-user-style ((buffer cef-buffer) (style cef-user-style))
  (when-let ((code (nyxt/mode/user-script:code style)))
    (setf (style-id style)
          (cef:insert-css (cef:web-contents buffer) code)))
  style)

(defmethod ffi-buffer-remove-user-style ((buffer cef-buffer) (style cef-user-style))
  (when (style-id style)
    (cef:remove-inserted-css (cef:web-contents buffer) (style-id style))
    (setf (style-id style) nil)))

(defun user-script-body (script)
  "Wrap SCRIPT so include and exclude patterns are honored in the page world.
CEF's isolated worlds are not available through this bridge."
  (let ((include (or (nyxt/mode/user-script:include script)
                     '("http://*/*" "https://*/*")))
        (exclude (or (nyxt/mode/user-script:exclude script) '()))
        (code (nyxt/mode/user-script:code script)))
    (format nil "(function(){
var href = location.href;
var include = ~a;
var exclude = ~a;
function match(pattern) {
  var source = '^';
  for (var i = 0; i < pattern.length; i++) {
    var c = pattern.charAt(i);
    if (c === '*') source += '.*';
    else if ('\\\\.^$+?()[]{}|'.indexOf(c) >= 0) source += '\\\\' + c;
    else source += c;
  }
  return new RegExp(source + '$').test(href);
}
if (include.length && !include.some(match)) return;
if (exclude.some(match)) return;
~a
})();"
            (cl-json:encode-json-to-string include)
            (cl-json:encode-json-to-string exclude)
            code)))

(defmethod ffi-buffer-add-user-script ((buffer cef-buffer) (script cef-user-script))
  (if-let ((code (nyxt/mode/user-script:code script)))
    (setf (script-id script)
          (cef:add-user-script (cef:web-contents buffer)
                               (user-script-body script)
                               :run-at (nyxt/mode/user-script:run-at script)
                               :all-frames-p (nyxt/mode/user-script:all-frames-p script)))
    (echo-warning "User script ~a is empty." script))
  script)

(defmethod ffi-buffer-remove-user-script ((buffer cef-buffer) (script cef-user-script))
  (when (script-id script)
    (cef:remove-user-script (cef:web-contents buffer) (script-id script))
    (setf (script-id script) nil)))

(defmethod ffi-buffer-javascript-enabled-p ((buffer cef-buffer))
  (cef:javascript-enabled-p (cef:web-contents buffer)))
(defmethod (setf ffi-buffer-javascript-enabled-p) (value (buffer cef-buffer))
  (cef:set-javascript-enabled (cef:web-contents buffer) value))

(defmethod ffi-buffer-javascript-markup-enabled-p ((buffer cef-buffer))
  (ffi-buffer-javascript-enabled-p buffer))
(defmethod (setf ffi-buffer-javascript-markup-enabled-p) (value (buffer cef-buffer))
  (setf (ffi-buffer-javascript-enabled-p buffer) value))

(defmethod ffi-buffer-smooth-scrolling-enabled-p ((buffer cef-buffer))
  (smooth-scrolling-p buffer))
(defmethod (setf ffi-buffer-smooth-scrolling-enabled-p) (value (buffer cef-buffer))
  (setf (smooth-scrolling-p buffer) value))

(defmethod ffi-buffer-media-enabled-p ((buffer cef-buffer))
  (media-enabled-p buffer))
(defmethod (setf ffi-buffer-media-enabled-p) (value (buffer cef-buffer))
  (setf (media-enabled-p buffer) value))

(defmethod ffi-buffer-webgl-enabled-p ((buffer cef-buffer))
  (cef:webgl-enabled-p (cef:web-contents buffer)))
(defmethod (setf ffi-buffer-webgl-enabled-p) (value (buffer cef-buffer))
  (cef:set-webgl-enabled (cef:web-contents buffer) value))

(defmethod ffi-buffer-auto-load-image-enabled-p ((buffer cef-buffer))
  (cef:images-enabled-p (cef:web-contents buffer)))
(defmethod (setf ffi-buffer-auto-load-image-enabled-p) (value (buffer cef-buffer))
  (cef:set-images-enabled (cef:web-contents buffer) value))

(defmethod ffi-buffer-proxy ((buffer cef-buffer))
  (values (cef-proxy-url buffer)
          (cef-proxy-ignored-hosts buffer)))

(defmethod (setf ffi-buffer-proxy) (proxy-specifier (buffer cef-buffer))
  (let* ((spec (alex:ensure-list proxy-specifier))
         (proxy-url (or (first spec) (quri:uri "")))
         (ignore-hosts (or (second spec) '())))
    (setf (cef-proxy-url buffer) proxy-url
          (cef-proxy-ignored-hosts buffer) ignore-hosts)
    (cef:set-proxy (cef:web-contents buffer)
                   (if (url-empty-p proxy-url) "" (render-url proxy-url))
                   (format nil "~{~a~^,~}" ignore-hosts))
    proxy-specifier))

(defmethod ffi-buffer-cookie-policy ((buffer cef-buffer))
  (cef:cookie-policy (cef:web-contents buffer)))
(defmethod (setf ffi-buffer-cookie-policy) (value (buffer cef-buffer))
  (cef:set-cookie-policy (cef:web-contents buffer) value))

(define-class cef-window (cef:window)
  ((current-view)
   (panel-views-p
    nil
    :documentation "Whether the status and message views have been added.
They are added after the page view: the first child of the window has to be
the Chrome-style page."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "CEF window class."))

(defun place-panel-views (window)
  "Stack the status bar under the page and the message bar under that.
Each bar is a fixed height. The status document uses vh, which tracks the
view, so a window-sized view makes the text fill the screen."
  (let ((message-buffer (message-buffer window))
        (status-buffer (status-buffer window)))
    (cef:set-bar-height status-buffer
                        (or (set-height status-buffer) (height status-buffer) 0))
    (cef:set-bar-height message-buffer
                        (or (set-height message-buffer) (height message-buffer) 0))
    (cef:add-view window status-buffer)
    (cef:add-view window message-buffer)))

(defmethod initialize-instance :after ((window cef-window) &key)
  (let ((message-buffer (message-buffer window))
        (status-buffer (status-buffer window)))
    (setf (set-height message-buffer) (height message-buffer))
    (setf (set-height status-buffer) (height status-buffer))
    (cef:add-listener
     window :close
     (lambda (window data)
       (declare (ignore data))
       (when (current-view window)
         (let ((id (id (current-view window))))
           (run-thread "window-close"
             (nyxt::buffer-delete id))))))))

(defmethod ffi-window-delete ((window cef-window))
  (when (current-view window)
    (cef:remove-view window (current-view window) :kill-view-p nil))
  (cef:kill window))

(defmethod ffi-window-to-foreground ((window cef-window))
  (cef:focus window)
  (call-next-method))

(defmethod ffi-window-title ((window cef-window))
  (cef:get-title window))
(defmethod (setf ffi-window-title) (title (window cef-window))
  (cef:set-title window title))

(defmethod ffi-window-set-buffer ((window cef-window)
                                  (buffer cef-buffer)
                                  &key (focus t))
  (when (current-view window)
    (cef:remove-view window (current-view window) :kill-view-p nil))
  ;; Index 0 keeps the page above the bars when the buffer changes.
  (cef:add-view-at window buffer 0)
  (unless (panel-views-p window)
    (place-panel-views window)
    (setf (panel-views-p window) t)
    (nyxt::print-status (status-buffer window))
    (ffi-print-message (message-buffer window) "Ready."))
  (when focus (cef:focus buffer))
  (setf (current-view window) buffer)
  buffer)

(defmethod ffi-height ((window cef-window))
  (assoc-value (cef:get-bounds window) :height))

(defmethod ffi-width ((window cef-window))
  (assoc-value (cef:get-bounds window) :width))

(defmethod ffi-window-fullscreen ((window cef-window) &key &allow-other-keys)
  (cef:fullscreen window))

(defmethod ffi-window-unfullscreen ((window cef-window) &key &allow-other-keys)
  (cef:unfullscreen window))

(defmethod ffi-window-maximize ((window cef-window) &key &allow-other-keys)
  (cef:maximize window))

(defmethod ffi-window-unmaximize ((window cef-window) &key &allow-other-keys)
  (cef:unmaximize window))

(defmethod ffi-window-active ((browser cef-browser))
  (or (find-if #'cef:is-focused (window-list))
      (call-next-method)))

(defmethod input-modifier-translator ((buffer cef-buffer) input-event-modifier-state)
  "Return a list of modifier keys understood by `keymaps:make-key'."
  (when-let ((state input-event-modifier-state))
    (mapcar (lambda (modifier) (getf (modifier-plist buffer) modifier)) state)))

(defun translate-code-string (code-string)
  "Return string representation of a keyval.
Return nil when key must be discarded, e.g. for modifiers."
  (match code-string
    ((or "ControlLeft" "ControlRight"
         "ShiftLeft" "ShiftRight"
         "MetaLeft" "MetaRight"
         "AltLeft" "AltRight")
     nil)
    ("Minus" "hyphen")
    ("Equal" "=")
    ("Space" "space")
    ("Enter" "return")
    ("Escape" "escape")
    ("Tab" "tab")
    ("Comma" ",")
    ("Period" ".")
    ("Slash" "/")
    ("Semicolon" ";")
    ("Quote" "'")
    ("BracketLeft" "[")
    ("BracketRight" "]")
    ("Backslash" "\\")
    ("Backquote" "`")
    ("Backspace" "backspace")
    ("ArrowUp" "up")
    ("ArrowDown" "down")
    ("ArrowRight" "right")
    ("ArrowLeft" "left")
    ("PageUp" "pageup")
    ("PageDown" "pagedown")
    ("Home" "home")
    ("End" "end")
    ("F1" "f1")
    ("F2" "f2")
    ("F3" "f3")
    ("F4" "f4")
    ("F5" "f5")
    ("F6" "f6")
    ("F7" "f7")
    ("F8" "f8")
    ("F9" "f9")
    ("F10" "f10")
    ("F11" "f11")
    ("F12" "f12")
    ((simple-string #\K #\e #\y key-value) (string-downcase (string key-value)))
    ((simple-string #\D #\i #\g #\i #\t digit-value) (string digit-value))
    (_ code-string)))

(defmethod on-signal-key-press-event ((sender cef-buffer) event)
  (when (string= "keyDown" (assoc-value event :type))
    (let ((modifiers (event-modifiers event))
          (key-string (translate-code-string
                       (cef:windows-key-code-to-code
                        (assoc-value event :windows-key-code)))))
      (flet ((key ()
               (keymaps:make-key :value key-string
                                 :modifiers (input-modifier-translator sender modifiers)
                                 :status :pressed)))
        (when key-string
          (alex:appendf (key-stack sender) (list (key)))
          (run-thread "on-signal-key-press" (on-signal-key-press sender (key)))
          (dispatch-input-event event sender))))))

(defmethod on-signal-button-press-event ((sender cef-buffer) event)
  (let ((key-string (format nil "button~a" (assoc-value event :button)))
        (modifiers (input-modifier-translator sender (event-modifiers event)))
        (buffer (or (current-prompt-buffer) sender)))
    (flet ((key ()
             (keymaps:make-key :value key-string
                               :modifiers modifiers
                               :status :pressed)))
      (run-thread "on-signal-button-press"
        (on-signal-button-press buffer (key)))
      (when key-string
        (alex:appendf (key-stack buffer) (list (key)))
        (dispatch-input-event event sender)))))

(defmethod on-signal-scroll-event ((sender cef-buffer) event)
  (let ((key-string (format nil "button~a" (assoc-value event :button)))
        (modifiers (input-modifier-translator sender (event-modifiers event))))
    (when key-string
      (alex:appendf (key-stack sender)
                    (list (keymaps:make-key :value key-string
                                            :modifiers modifiers
                                            :status :pressed)))
      (dispatch-input-event event sender))))

(defun on-signal-hover (buffer data)
  (let ((url (or (assoc-value data :url) "")))
    (run-thread "hover"
      (if (uiop:emptyp url)
          (progn
            (nyxt::print-message "")
            (setf (url-at-point buffer) (quri:uri "")))
          (progn
            (nyxt::print-message (str:concat "→ " url))
            (setf (url-at-point buffer) (quri:uri url)))))))

(defvar *context-menu-commands-by-id* (make-hash-table)
  "Command id assigned while a context menu is open, to the command it runs.")

(defvar +menu-id-user-first+ 26500
  "CEF MENU_ID_USER_FIRST. Lower ids belong to Chromium.")

(defun populate-context-menu (buffer)
  "Append Nyxt commands to the context menu open on the UI thread."
  (clrhash *context-menu-commands-by-id*)
  (let ((next-id +menu-id-user-first+)
        (accessible-commands
          (mapcar #'name
                  (nyxt::list-commands
                   :global-p t
                   :mode-symbols (mapcar #'sera:class-name-of
                                         (sera:filter #'enabled-p (modes buffer)))))))
    (flet ((add (label command)
             (cef:menu-add-item next-id label)
             (setf (gethash next-id *context-menu-commands-by-id*) command)
             (incf next-id)))
      (maphash (lambda (label function)
                 (cond
                   ((or (and (command-p function)
                             (member (name function) accessible-commands))
                        (functionp function))
                    (add label function))
                   ((listp function)
                    (cef:menu-add-separator)
                    (loop for (command command-label) in function
                          do (add (format nil "~a: ~a" label command-label) command)))))
               nyxt::*context-menu-commands*))))

(defun run-context-menu-command (data)
  (let* ((raw (assoc-value data :command-id))
         (command-id (if (realp raw) (round raw) raw))
         (command (gethash command-id *context-menu-commands-by-id*)))
    (when command
      (if (command-p command)
          (nyxt::run-async command)
          (run-thread "context-menu-command" (funcall command))))))
