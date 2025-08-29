;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/renderer/electron
  (:documentation "Electron renderer."))
(in-package :nyxt/renderer/electron)

(define-class electron-renderer (renderer)
  ((name "Electron"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Electron renderer class."))

(setf nyxt::*renderer* (make-instance 'electron-renderer))
(pushnew :nyxt-electron *features*)

(defmethod install ((renderer electron-renderer))
  (flet ((set-superclasses (renderer-class-sym+superclasses)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class (first renderer-class-sym+superclasses)
                                     :direct-superclasses (rest renderer-class-sym+superclasses)
                                     :metaclass 'interface-class))))
    (mapc #'set-superclasses '((renderer-browser electron-browser)
                               (renderer-scheme electron-scheme)
                               (renderer-window electron-window)
                               (renderer-buffer electron-buffer)
                               (nyxt/mode/download:renderer-download electron-download)))))

(defmethod uninstall ((renderer electron-renderer))
  (flet ((remove-superclasses (renderer-class-sym)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class renderer-class-sym
                                     :direct-superclasses '()
                                     :metaclass 'interface-class))))
    (mapc #'remove-superclasses '(renderer-browser
                                  renderer-scheme
                                  renderer-window
                                  renderer-buffer))))


(define-class electron-download ()
  ((remote-object))
  (:documentation "Electron download class."))

(defmethod update-status ((electron-download electron-download))
  (setf (nyxt/mode/download:status electron-download)
        (match (electron:state (remote-object electron-download))
          ("completed" :finished)
          ("progressing" :loading)
          ("cancelled" :canceled)
          ("interrupted" :failed))))

(define-class electron-scheme (electron:protocol)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron scheme class."))

(defmethod initialize-instance :after ((scheme electron-scheme) &key)
  ;; Set the scheme name from child (`scheme') to parent class
  ;; (`electron-scheme').
  (setf (slot-value scheme 'electron:scheme-name)
        (name scheme)))

(defmethod ffi-register-custom-scheme ((scheme electron-scheme))
  (electron:handle-callback scheme (callback scheme)))

(define-class electron-browser (electron:interface)
  ((adblocking-enabled-p t))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron browser class."))

(defmethod ffi-initialize ((browser electron-browser) urls startup-timestamp)
  (declare (ignore urls startup-timestamp))
  (log:debug "Initializing Electron Interface")
  (setf electron:*interface* (make-instance 'electron:interface))
  ;; Schemes' privileges (security settings) need to be set before launch.
  (setf (electron:protocols electron:*interface*)
        (list (make-instance 'electron:protocol
                             :scheme-name "nyxt"
                             :privileges "{}")
              (make-instance 'electron:protocol
                             :scheme-name "nyxt-resource"
                             :privileges "{secure:true}")
              (make-instance 'electron:protocol
                             :scheme-name "lisp"
                             :privileges "{supportFetchAPI:true,corsEnabled:true}")
              (make-instance 'electron:protocol
                             :scheme-name "view-source"
                             :privileges "{}")
              (make-instance 'electron:protocol
                             :scheme-name "gopher"
                             :privileges "{}")
              (make-instance 'electron:protocol
                             :scheme-name "gemini"
                             :privileges "{}")))
  (setf (electron:launch-options electron:*interface*)
        (cl-ppcre:split "\\s+"  (getf *options* :electron-opts)))
  (electron:launch electron:*interface*)
  (when (adblocking-enabled-p browser)
    (let ((adblocker (make-instance 'electron:adblocker-electron)))
      (electron:default-block adblocker)))
  (maphash (lambda (scheme-name callbacks)
             (ffi-register-custom-scheme (make-instance 'scheme
                                                        :name scheme-name
                                                        :callback (first callbacks)
                                                        :error-callback (second callbacks))))
           nyxt::*schemes*)
  (let ((session (electron:default-session electron:*interface*)))
    (electron:add-listener session :download-item-updated
                           (lambda (session item)
                             (declare (ignore session))
                             (download-item-updated item))))
  (call-next-method)
  (unless nyxt::*run-from-repl-p*
    (uiop:wait-process (electron:process electron:*interface*))
    (uiop:quit (nyxt:exit-code browser) #+bsd nil)))

#+sbcl (pushnew 'electron:terminate sb-ext:*exit-hooks*)

(defun download-item-updated (download-item)
  (let ((download (find download-item (downloads *browser*) :key #'remote-object)))
    (if download
        (progn
          (setf (url download)
                (electron:url (remote-object download))
                (nyxt/mode/download:bytes-downloaded download)
                (electron:received-bytes (remote-object download))
                (nyxt/mode/download:completion-percentage download)
                (electron:percent-complete (remote-object download))
                (nyxt/mode/download:destination-path download)
                (electron:save-path (remote-object download)))
          (update-status download))
        (progn
          (let ((download (make-instance 'nyxt/mode/download:download
                                         :remote-object download-item
                                         :url (electron:url download-item))))
            (push download (downloads *browser*))
            (setf (nyxt/mode/download::cancel-function download)
                  (lambda ()
                    (electron:cancel (remote-object download)))))))))

(defmethod ffi-kill-browser ((browser electron-browser))
  (declare (ignore browser))
  (electron:terminate))

(define-class electron-buffer (electron:view)
  ((electron:options
    ""
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "A string that specifies the buffer's behavior.")
   (modifier-plist
    '(:shift "shift"
      :control "control"
      :alt "meta"
      :meta "super")
    :type list
    :documentation "A map between Electron's and Nyxt's terminology for modifier keys.
Note that by changing the default value, modifier keys can be remapped.")
   (set-height
    :documentation "The height the buffer has been requested to be set to.
It does not represent the current height as reported by the renderer, or
the default height."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron buffer class."))

(defmethod customize-instance :after ((buffer electron-buffer)
                                      &key &allow-other-keys)
  ;; Otherwise the HTML document won't be set via JS.
  (when (member (type-of buffer) '(status-buffer message-buffer prompt-buffer))
    (electron:load-url buffer "about:blank"))
  (initialize-listeners buffer)
  (initialize-window-open-handler buffer))

(defmethod initialize-window-open-handler ((buffer electron-buffer))
  ;; When following a link with target="_blank" or through JS window.open,
  ;; we don't want to let electron open a new window, instead we want to
  ;; put it into a new buffer
  (electron:override-window-open-handler
   (electron:web-contents buffer)
   (lambda (details)
     (let ((url (assoc-value details :url)))
       (make-buffer-focus :url (quri:uri url))))))

(defmethod initialize-listeners ((buffer electron-buffer))
  (electron:add-listener buffer :before-input-event
                         (lambda (buffer event)
                           (on-signal-key-press-event buffer event)))
  (when (web-buffer-p buffer)
    (electron:add-listener (electron:web-contents buffer) :did-start-loading
                           (lambda (_) (declare (ignore _))
                             (setf (nyxt::status buffer) :loading)
                             (on-signal-load-started buffer (ffi-buffer-url buffer))))
    (electron:add-listener (electron:web-contents buffer) :did-redirect-navigation
                           (lambda (_) (declare (ignore _))
                             (let ((url (ffi-buffer-url buffer)))
                               (setf (slot-value buffer 'url) url)
                               (on-signal-load-redirected buffer url))))
    (electron:add-listener (electron:web-contents buffer) :did-finish-load
                           (lambda (_) (declare (ignore _))
                             (setf (nyxt::status buffer) :finished)
                             (let ((url (ffi-buffer-url buffer))
                                   (title (ffi-buffer-title buffer)))
                               (setf (url buffer) url)
                               (on-signal-load-finished buffer url title))))
    (electron:add-listener (electron:web-contents buffer) :page-title-updated
                           (lambda (_) (declare (ignore _))
                             (on-signal-notify-title buffer (ffi-buffer-title buffer))))
    (unless (member (type-of buffer) '(status-buffer message-buffer prompt-buffer))
      (electron:add-listener
       (electron:web-contents buffer) :context-menu
       (lambda (object params)
         (declare (ignore object))
         (print params)
         (format nil  "[{label: 'Backward', click: () => {~a.goBack()}},
                        {label: 'Forward', click: () => {~a.goForward()}},
                        {label: 'Reload', click: () => {~a.reload()}},
                       ]"
                 (electron:remote-symbol (electron:web-contents buffer))
                 (electron:remote-symbol (electron:web-contents buffer))
                 (electron:remote-symbol (electron:web-contents buffer))))))))

(defmethod ffi-buffer-initialize-foreign-object ((buffer electron-buffer))
  (electron::message
   buffer
   (format nil "~a = new WebContentsView(~a)"
           (electron:remote-symbol buffer) (electron:options buffer)))
  (initialize-listeners buffer)
  buffer)

(defmethod ffi-buffer-delete ((buffer electron-buffer))
  (electron:remove-view (current-window) buffer :kill-view-p t))

(defmethod ffi-buffer-url ((buffer electron-buffer))
  (quri:uri (electron:get-url buffer)))

(defmethod ffi-buffer-title ((buffer electron-buffer))
  (electron:get-title buffer))

(defmethod ffi-buffer-load ((buffer electron-buffer) url)
  (electron:load-url buffer url))

(defmethod ffi-buffer-reload ((buffer electron-buffer))
  (electron:reload (electron:web-contents buffer))
  buffer)

(defmethod ffi-buffer-zoom-ratio ((buffer electron-buffer))
  (electron:get-zoom-factor (electron:web-contents buffer)))

(defmethod (setf ffi-buffer-zoom-ratio) (value (buffer electron-buffer))
  (if (and (floatp value) (plusp value))
      (electron:set-zoom-factor (electron:web-contents buffer) value)
      (echo-warning "Zoom ratio must be a positive floating point number.")))

(defmethod ffi-buffer-evaluate-javascript ((buffer electron-buffer) javascript
                                           &optional world-name)
  ;; TODO world-name is used in user-script mode.
  (declare (ignore world-name))
  (electron:execute-javascript-synchronous (electron:web-contents buffer) javascript))

(defmethod ffi-buffer-evaluate-javascript-async ((buffer electron-buffer) javascript
                                                 &optional world-name)
  (declare (ignore world-name))
  (electron:execute-javascript (electron:web-contents buffer) javascript))

(defmethod ffi-inspector-show ((buffer electron-buffer))
  (electron:open-dev-tools buffer))

(defmethod ffi-focused-p ((buffer electron-buffer))
  (electron:is-focused buffer))

;; ffi-buffer-load-alternate-html handles bogus URLs (https://bogusfoo.com/).
;; (defmethod ffi-buffer-load-alternate-html ((buffer electron-buffer) html-content content-url url))

(defmethod ffi-buffer-copy ((buffer electron-buffer) &optional (text nil text-provided-p))
  (if text-provided-p
      (trivial-clipboard:text text)
      (progn
        (electron:copy (electron:web-contents buffer))
        (trivial-clipboard:content))))

(defmethod ffi-buffer-paste ((buffer electron-buffer) &optional (text nil text-provided-p))
  (if text-provided-p
      (electron:insert-text (electron:web-contents buffer) text)
      (electron:paste (electron:web-contents buffer))))

(defmethod ffi-buffer-cut ((buffer electron-buffer))
  (electron:cut (electron:web-contents buffer))
  (trivial-clipboard:text))

(defmethod ffi-buffer-select-all ((buffer electron-buffer))
  (electron:select-all (electron:web-contents buffer)))

(defmethod ffi-buffer-undo ((buffer electron-buffer))
  ;; There is no way to check if an undo operation is possible. There exists a
  ;; `context-menu' event that when invoked can check whether `canUndo' exists
  ;; within the `editFLags' of the renderer, but we cannot manually trigger this
  ;; event.
  (electron:undo (electron:web-contents buffer)))

(defmethod ffi-buffer-redo ((buffer electron-buffer))
  ;; There is no way to check if a redo operation is possible. There exists a
  ;; `context-menu' event that when invoked can check whether `canRedo' exists
  ;; within the `editFLags' of the renderer, but we cannot manually trigger this
  ;; event.
  (electron:redo (electron:web-contents buffer)))

;; TODO
;; (defmethod ffi-buffer-cookie-policy ((buffer electron-buffer)))

(defun update-active-buffer-bounds (window delta)
  "Recalculate the bounds of the active window to compensate for changes in the
height of the status/prompt/message buffer."
  (let ((window-bounds (electron:get-bounds window)))
    (electron:set-bounds (active-buffer window)
                         :x 0
                         :y 0
                         :width (assoc-value window-bounds :width)
                         :height (- (assoc-value window-bounds :height)
                                    (+ delta
                                       (ffi-height (status-buffer window))
                                       (ffi-height (message-buffer window)))))))

(defmethod ffi-height ((buffer electron-buffer))
  (assoc-value (electron:get-bounds buffer) :height))

(defmethod (setf ffi-height) ((height integer) (buffer electron-buffer))
  (setf (set-height buffer) height)
  (let ((bounds (electron:get-bounds buffer)))
    (electron:set-bounds buffer
                         :x (assoc-value bounds :x)
                         :y (assoc-value bounds :y)
                         :width (assoc-value bounds :width)
                         :height height))
  (update-active-buffer-bounds (window buffer) 0))

(defmethod ffi-focus-buffer ((buffer electron-buffer))
  (electron:focus buffer)
  buffer)

(defmethod (setf ffi-height) ((height integer) (prompt-buffer prompt-buffer))
  (with-slots (window) prompt-buffer
    (update-active-buffer-bounds window height)
    (electron:add-bounded-view window
                               prompt-buffer
                               :window-bounds-alist-var bounds
                               :x 0
                               :y (- (assoc-value bounds :height)
                                     (+ height
                                        (set-height (status-buffer window))
                                        (set-height (message-buffer window))))
                               :width (assoc-value bounds :width)
                               :height height)))

(defmethod ffi-width ((buffer electron-buffer))
  (assoc-value (electron:get-bounds buffer) :height))

(defmethod (setf ffi-width) (width (buffer electron-buffer))
  (let ((bounds (electron:get-bounds buffer)))
    (electron:set-bounds buffer
                         :x (assoc-value bounds :x)
                         :y (assoc-value bounds :y)
                         :width width
                         :height (assoc-value bounds :height))))

(defmethod ffi-buffer-sound-enabled-p ((buffer electron-buffer))
  (not (electron:muted-p (electron:web-contents buffer))))
(defmethod (setf ffi-buffer-sound-enabled-p) (value (buffer electron-buffer))
  (electron:set-audio-muted (electron:web-contents buffer) (not value)))

(defmethod ffi-buffer-download ((buffer electron-buffer) url)
  (electron:download-url (electron:web-contents buffer) url))

;; TODO Support user-script mode.
;; (defmethod ffi-buffer-add-user-style ((buffer electron-buffer) style))
;; (defmethod ffi-buffer-remove-user-style ((buffer electron-buffer) style))
;; (defmethod ffi-buffer-add-user-script ((buffer electron-buffer) script))
;; (defmethod ffi-buffer-remove-user-script ((buffer electron-buffer) script))


;; TODO: Implement image / javascript disabling by deleting/recreating the view
;; with the appropriate WebPreferences. It is not possible to enable/disable
;; WebPreferences in real-time due to limitations in Electron.
(defmethod ffi-buffer-auto-load-image-enabled-p ((buffer electron-buffer))
  (echo "Disabling images not supported by Electron back-end.")
  (error "Disabling images not supported by Electron back-end."))
(defmethod (setf ffi-buffer-auto-load-image-enabled-p) (value (buffer electron-buffer))
  (declare (ignore buffer value))
  (echo "Disabling images not supported by Electron back-end.")
  (error "Disabling images not supported by Electron back-end."))

(defmethod ffi-buffer-javascript-markup-enabled-p ((buffer electron-buffer))
  (echo "Disabling JavaScript not supported by Electron back-end.")
  (error "Disabling JavaScript not supported by Electron back-end."))
(defmethod (setf ffi-buffer-javascript-markup-enabled-p) (value (buffer electron-buffer))
  (declare (ignore buffer value))
  (echo "Disabling JavaScript not supported by Electron back-end.")
  (error "Disabling JavaScript not supported by Electron back-end."))

(defmethod ffi-buffer-webgl-enabled-p ((buffer electron-buffer))
  (echo "Disabling WebGL not supported by Electron back-end.")
  (error "Disabling WebGL not supported by Electron back-end."))
(defmethod (setf ffi-buffer-webgl-enabled-p) (value (buffer electron-buffer))
  (declare (ignore buffer value))
  (echo "Disabling WebGL not supported by Electron back-end.")
  (error "Disabling WebGL not supported by Electron back-end."))

(defmethod ffi-buffer-proxy ((buffer electron-buffer))
  (echo "Setting Proxy per buffer not supported by the Electron back-end.")
  (error "Setting Proxy per buffer not supported by the Electron back-end."))
(defmethod (setf ffi-buffer-proxy) (value (buffer electron-buffer))
  (declare (ignore buffer value))
  (echo "Setting Proxy per buffer not supported by the Electron back-end.")
  (error "Setting Proxy per buffer not supported by the Electron back-end."))

(define-class electron-window (electron:window)
  ((electron:options
    "{autoHideMenuBar: true,
      width: 1600,
      height: 1200}")
   (current-view))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron window class."))

(defmethod initialize-instance :after ((window electron-window) &key)
  (electron:remove-menu window)
  (let ((message-buffer (message-buffer window))
        (status-buffer (status-buffer window)))
    (setf (set-height message-buffer) (height message-buffer))
    (setf (set-height status-buffer) (height status-buffer))
    (electron:add-bounded-view window
                               message-buffer
                               :window-bounds-alist-var bounds
                               :x 0
                               :y (- (assoc-value bounds :height)
                                     (set-height message-buffer))
                               :width (assoc-value bounds :width)
                               :height (set-height message-buffer))
    (electron:add-bounded-view window
                               status-buffer
                               :window-bounds-alist-var bounds
                               :x 0
                               :y (- (assoc-value bounds :height)
                                     (+ (set-height status-buffer)
                                        (set-height message-buffer)))
                               :width (assoc-value bounds :width)
                               :height (set-height status-buffer))
    ;; TODO: Fix buffer deletion. We CANNOT hook on close to remove the view
    ;; from the window because it is too late. Ergo, we must manually delete the
    ;; currently viewed buffer without trying to mutate it via JS.
    (electron:add-listener
     window :close
     (lambda (_) (declare (ignore _))
       (when (current-view window)
         (nyxt::buffer-delete (id (current-view window))))))))

(defmethod ffi-window-delete ((window electron-window))
  (when (current-view window)
    (electron:remove-view window (current-view window) :kill-view-p nil))
  (electron:kill window))

(defmethod ffi-window-title ((window electron-window))
  (electron:get-title window))
(defmethod (setf ffi-window-title) (title (window electron-window))
  (electron:set-title window title))

(defmethod ffi-window-set-buffer ((window electron-window)
                                  (buffer electron-buffer)
                                  &key (focus t))
  (when (current-view window)
    (electron:remove-view window (current-view window) :kill-view-p nil))
  (electron:add-bounded-view window
                             buffer
                             :window-bounds-alist-var bounds
                             :x 0
                             :y 0
                             :width (assoc-value bounds :width)
                             :height (- (assoc-value bounds :height)
                                        (+ (ffi-height (status-buffer window))
                                           (ffi-height (message-buffer window))
                                           (if-let ((prompt (current-prompt-buffer)))
                                             (ffi-height prompt)
                                             0))))
  (when focus (electron:focus buffer))
  (setf (current-view window) buffer)
  buffer)

(defmethod ffi-height ((window electron-window))
  (assoc-value (electron:get-bounds window) :height))

(defmethod ffi-width ((window electron-window))
  (assoc-value (electron:get-bounds window) :width))

(defmethod ffi-window-fullscreen ((window electron-window) &key &allow-other-keys)
  (electron:fullscreen window))

(defmethod ffi-window-unfullscreen ((window electron-window) &key &allow-other-keys)
  (electron:unfullscreen window))

(defmethod ffi-window-maximize ((window electron-window) &key &allow-other-keys)
  (electron:maximize window))

(defmethod ffi-window-unmaximize ((window electron-window) &key &allow-other-keys)
  (electron:unmaximize window))

(defmethod ffi-window-active ((browser electron-browser))
  (or (find-if #'electron:is-focused (window-list))
      (call-next-method)))

;; Input handling

(defmethod input-modifier-translator ((buffer electron-buffer) input-event-modifier-state)
  "Return a list of modifier keys understood by `keymaps:make-key'."
  (when-let ((state input-event-modifier-state))
    (mapcar (lambda (modifier) (getf (modifier-plist buffer) modifier)) state)))

(defun translate-key-string (key-string)
  "Return string representation of a keyval.

Downcased GDK names will be prioritized where possible, e.g. \"Enter\" will
return \"return\", and \"BrightnessDown\" \"monbrightnessdown\".

Shifted characters will return their downcased equivalent, e.g. \"A\" will
return \"a\".

Names without a GDK equivalent will be returned as-is, e.g. \"MediaPlayPause\"
returns \"MediaPlayPause\" even though \"MediaPlay\" returns \"audioplay\" and
\"MediaPause\" returns \"audiopause\".

Return nil when key must be discarded, e.g. for modifiers."
  ;; Compatibility layer between GDK keycode names and those of Browsers.
  ;; E.g. We might remap "Foo" to "bar" (GDK_KEY_BAR).
  ;;
  ;; See
  ;; - https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key
  ;; - https://gitlab.gnome.org/GNOME/gtk/-/blob/main/gdk/gdkkeysyms.h
  ;; - https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
  (match key-string
    ;; We handle these first as they're assumed to be hit frequently.
    ((simple-string #\Space) "space")
    ((simple-string key) (string-downcase key))
    ;; Other whitespace keys
    ("Enter" "return")
    ("Tab" "tab")
    ;; Navigation keys
    ("ArrowDown" "down")
    ("ArrowLeft" "left")
    ("ArrowRight" "right")
    ("ArrowUp" "up")
    ("End" "end")
    ("Home" "home")
    ("PageDown" "pagedown")
    ("PageUp" "pageup")
    ;; Editing keys
    ("Backspace" "backspace")
    ("Clear" "clear")
    ("Copy" "copy")
    ("CrSel" "cursorselect")
    ("Cut" "cut")
    ("delete" "delete")
    ("EreaseEof" "exselect") ; GDK doesn't differantiate between VK_EREOF and VK_EXSEL
    ("ExSel" "exselect")
    ("Insert" "insert")
    ("Paste" "paste")
    ("Redo" "redo")
    ("Undo" "undo")
    ;; UI keys
    ("Again" "redo")
    ("Attn" "attn")
    ("Cancel" "cancel")
    ("ContextMenu" "menu")
    ("Escape" "escape")
    ("Execute" "execute")
    ("Find" "find")
    ("Help" "help")
    ("Pause" "pause")
    ("Play" "play")
    ("Select" "select")
    ("ZoomIn" "zoomin")
    ("ZoomOut" "zoomout")
    ;; Device keys
    ("BrightnessDown" "monbrightnessdown")
    ("BrightnessUp" "monbrightnessup")
    ("Eject" "eject")
    ("LogOff" "logoff")
    ;; "PowerOff" is ambiguous. Both GDK_KEY_PowerDown and GDK_KEY_PowerOff is
    ;; listed. We map it to "poweroff", and add an additional "PowerDown" in
    ;; case it is sometimes reported.
    ("PowerOff" "poweroff")
    ("PowerDown" "powerdown")
    ;; "PrintScreen" is ambiguous. It is says to be GDK_KEY_2370_PrintScreen,
    ;; GDK_KEY_Print and GDK_KEY_Sys_Req. Emacs reports "print", but the browser
    ;; also has a "Print" which should map to GDK_KEY_Print, so We report it as
    ;; "printscreen" instead.
    ("PrintScreen" "printscreen")
    ("Hibernate" "hibernate")
    ;; "Standby" is ambiguous. It says both GDK_KEY_Standby, GDK_KEY_Suspend and
    ;; GDK_KEY_Sleep. We map it to sleep as the keycode is named KEYCODE_SLEEP.
    ("Standby" "sleep")
    ("WakeUp" "wakeup")
    ;; IME and composition keys
    ;; Not sure how these should be handled, so we ignore them for now.
    ("AllCandidates" "multiplecandidate")
    ;; "Alphanumeric" is ambiguous. It is reported both as GDK_KEY_Eisu_Shift
    ;; and GDK_KEY_Eisu_toggle.
    ("Alphanumeric" "eisu_toggle")
    ("CodeInput" "codeinput")
    ("Compose" "multi_key")
    ("Convert" "henkan")
    ;; For dead keys, we should inspect compositionupdate
    ;; https://developer.mozilla.org/en-US/docs/Web/API/Element/compositionupdate_event
    ("Dead" "Dead")
    ("GroupFirst" "iso_first_group")
    ("GroupLast" "iso_last_group")
    ("GroupNext" "iso_next_group")
    ("GroupPrevious" "iso_prev_group")
    ;; "ModeChange" is ambiguous. It says both GDK_KEY_Mode_switch and GDK_KEY_script_switch
    ("ModeChange" "mode_switch")
    ("NonConvert" "muhenkan")
    ("PreviousCandidate" "previouscandidate")
    ("SingleCandidate" "singlecandidate")
    ;; Korean keyboards only
    ;; Not sure how these should be handled, so we ignore them for now.
    ("HangulMode" "hangul")
    ("HanjaMode" "hangul_hanja")
    ("JunjaMode" "hangul_jeonja")
    ;; Japanese keyboards only
    ;; Not sure how these should be handled, so we ignore them for now.
    ("Eisu" "eisu_toggle")
    ("Hankaku" "hankaku")
    ("Hiragana" "hiragana")
    ("HiraganaKatakana" "hiragana_katakana")
    ;; "KanaMode" is ambiguous. It says GDK_KEY_Kana_Lock and GDK_KEY_Shift
    ("KanaMode" "kana_lock")
    ("KanjiMode" "kanji")
    ("Katakana" "katakana")
    ("Romaji" "romaji")
    ("Zenkaku" "zenkaku")
    ("ZenkakuHankaku" "zenkaku_hankaku")
    ;; Function keys
    ;; Some of these has both a GDK_KEY_Fx and GDK_KEY_KP_Fx mapping.
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
    ("F13" "f13")
    ("F14" "f14")
    ("F15" "f15")
    ("F16" "f16")
    ("F17" "f17")
    ("F18" "f18")
    ("F19" "f19")
    ("F20" "f20")
    ;; Multimedia keys
    ("MediaFastForward" "audioforward")
    ("MediaPause" "audiopause")
    ("MediaPlay" "audioplay")
    ("MediaRecord" "audiorecord")
    ("MediaRewind" "audiorewind")
    ("MediaStop" "audiostop")
    ("MediaTrackNext" "audionext")
    ("MediaTrackPrevious" "audioprev")
    ("AudioVolumeDown" "audiolowervolume")
    ("AudioVolumeMute" "audiomute")
    ("AudioVolumeUp" "audioraisevolume")
    ("MicrophoneVolumeMute" "audiomicmute")
    ;; Media controller keys
    ("Dimmer" "brightnessadjust")
    ("MediaAudioTrack" "audiocycletrack")
    ("RandomToggle" "audiorandomplay")
    ("SplitScreenToggle" "splitscreen")
    ("Subtitle" "subtitle")
    ("VideoModeNext" "next_vmode")
    ;; Document keys
    ("Close" "close")
    ("New" "new")
    ("Open" "open")
    ;; See also note for "PrintScreen"
    ("Print" "print")
    ("Save" "save")
    ("SpellCheck" "spell")
    ("MailForward" "mailforward")
    ("MailReply" "reply")
    ("MailSend" "send")
    ;; Application selector keys
    ("LaunchCalculator" "calculator")
    ("LaunchCalendar" "calendar")
    ("LaunchMail" "mail")
    ;; "LaunchMediaPlayer" is ambiguous. It says GDK_KEY_CD, GDK_KEY_Video and
    ;; GDK_KEY_AudioMedia. We choose the last as it has the name media in it
    ;; which other mappings use.
    ("LaunchMediaPlayer" "audiomedia")
    ("LaunchMyComputer" "mycomputer")
    ("LaunchPhone" "phone")
    ("LaunchScreenSaver" "screensaver")
    ("LaunchSpreadsheet" "excel")
    ("LaunchWebBrowser" "www")
    ("LaunchWebCam" "webcam")
    ("LaunchWordProcessor" "word")
    ("LaunchApplication1" "launch0")
    ("LaunchApplication2" "launch1")
    ("LaunchApplication3" "launch2")
    ("LaunchApplication4" "launch3")
    ("LaunchApplication5" "launch4")
    ("LaunchApplication6" "launch5")
    ("LaunchApplication7" "launch6")
    ("LaunchApplication8" "launch7")
    ("LaunchApplication9" "launch8")
    ("LaunchApplication10" "launch9")
    ("LaunchApplication11" "launcha")
    ("LaunchApplication12" "launchb")
    ("LaunchApplication13" "launchc")
    ("LaunchApplication14" "launchd")
    ("LaunchApplication15" "launche")
    ("LaunchApplication16" "launchf")
    ;; Browser control keys
    ("BrowserBack" "back")
    ("BrowserFavorites" "favorites")
    ("BrowserForward" "forward")
    ("BrowserHome" "homepage")
    ("BrowserRefresh" "refresh")
    ("BrowserSearch" "search")
    ("BrowserStop" "stop")
    ;; Numeric keypad keys
    ;; NOTE: Digits on the keypad is mapped to the digits rather KP_0 etc.
    ("Decimal" "kp_decimal")
    ("Multiply" "kp_multiply")
    ("Add" "kp_add")
    ("Clear" "clear")
    ("Divide" "kp_divide")
    ("Subtract" "kp_subtract")
    ("Separator" "kp_separator")
    ;; Modifier keys
    ((or "Alt"
         "AltGraph"
         "Control"
         "Fn"
         "Hyper"
         "Meta"
         "Shift"
         "Super"
         "Symbol")
     nil)
    ("Unidentified" nil)
    (_ key-string)))

(defmethod on-signal-key-press-event ((sender electron-buffer) event)
  (when (string= "keyDown" (assoc-value event :type))
    (let ((modifiers (delete nil (list (when (assoc-value event :shift) :shift)
                                       (when (assoc-value event :control) :control)
                                       (when (assoc-value event :alt) :alt)
                                       (when (assoc-value event :meta) :meta))))
          (key-string (translate-key-string (assoc-value event :key))))
      (flet ((key () (keymaps:make-key :value key-string
                                       :modifiers (input-modifier-translator sender modifiers)
                                       :status :pressed)))
        (when key-string
          (alex:appendf (key-stack sender)
                        (list (key)))
          (run-thread "on-signal-key-press" (on-signal-key-press sender (key)))
          (dispatch-input-event event sender))))))

;; TODO on-signal-* methods
