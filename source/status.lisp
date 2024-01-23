;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class status-buffer (input-buffer)
  ((window
    nil
    :type (maybe window)
    :documentation "The `window' to which the status buffer is attached.")
   (height
    24
    :type integer
    :writer nil
    :reader height
    :export t
    :documentation "The height of the status buffer in pixels.")
   (glyph-mode-presentation-p
    nil
    :documentation "Display the modes as a list of glyphs.")
   (display-tabs-by-last-access-p
    nil
    :documentation "Whether tabs are dynamically ordered by last access time.")
   (glyph-left (gethash "left.svg" *static-data*))
   (glyph-right (gethash "right.svg" *static-data*))
   (glyph-reload (gethash "reload.svg" *static-data*))
   (glyph-lambda (gethash "lambda.svg" *static-data*))
   (style (theme:themed-css (theme *browser*)
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "400" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Regular.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "400" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Italic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "100" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Thin.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "100" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ThinItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "200" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraLight.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "200" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraLightItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "300" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Light.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "300" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-LightItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "500" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Medium.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "500" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-MediumItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "600" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-SemiBold.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "600" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-SemiBoldItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "700" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Bold.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "700" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-BoldItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "800" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraBold.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "800" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraBoldItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "900" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Black.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "900" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-BlackItalic.woff") "format('woff')")
            `(:font-face :font-family "dejavu sans mono" :src ,(format nil "url('nyxt-resource:~a')" "DejaVuSansMono.ttf") "format('ttf')")
            `(body
              :font-family ,theme:font-family
              :line-height "100vh"
              :font-size "14px"
              :padding 0
              :margin 0)
            `(.loader
              :border-width "2px"
              :border-style "solid"
              :border-color "transparent"
              :border-top-color ,theme:action
              :border-left-color ,theme:action
              :border-radius "50%"
              :display "inline-block"
              :width "7px"
              :height "7px"
              :animation "spin 1s linear infinite")
            `("@keyframes spin"
              ("0%" :transform "rotate(0deg)")
              ("100%" :transform "rotate(360deg)"))
            `(".arrow-right"
              :clip-path "polygon(0 0, calc(100% - 7px) 0, 100% calc(50% - 1px), 100% 50%, 100% calc(50% + 1px), calc(100% - 7px) 100%, 0 100%)"
              :margin-right "-7px")
            `(".arrow-left"
              :clip-path "polygon(7px 0, 100% 0, 100% 100%, 7px 100%, 0px calc(50% + 1px), 0% 50%, 0px calc(50% - 1px))"
              :margin-left "-7px")
            `("#container"
              :display "flex"
              :justify-content "space-between"
              :overflow-y "hidden")
            `("#controls"
              :background-color ,theme:secondary
              :color ,theme:on-secondary
              :overflow "hidden"
              :white-space "nowrap"
              :z-index "3"
              :flex-basis "78px"
              :display "flex")
            `("#controls > button"
              :margin-right "-3px"
              :max-width "20px"
              :height "100%"
              :aspect-ratio "1/1")
            `("#url"
              :background-color ,theme:primary
              :color ,theme:on-primary
              :font-size "60vh"
              :min-width "100px"
              :text-overflow "ellipsis"
              :overflow-x "hidden"
              :white-space "nowrap"
              :padding-right "7px"
              :padding-left "15px"
              :z-index "2"
              :flex-grow "3"
              :flex-shrink "2"
              :flex-basis "144px")
            `("#url button"
              :text-align "left"
              :width "100%")
            `("#tabs"
              :background-color ,theme:secondary
              :color ,theme:on-secondary
              :line-height "95vh"
              :font-size "60vh"
              :min-width "100px"
              :white-space "nowrap"
              :overflow-x "scroll"
              :text-align "left"
              :padding-left "3px"
              :padding-right "20px"
              :z-index "1"
              :flex-grow "10"
              :flex-shrink "4"
              :flex-basis "144px")
            `("#tabs::-webkit-scrollbar"
              :display "none")
            `(".tab"
              :background-color ,theme:background-
              :color ,theme:on-background
              :display "inline-block"
              :margin-top "1px"
              :padding-left "18px"
              :padding-right "18px"
              :margin-right "-1px"
              :margin-left "-4px"
              :text-decoration "transparent"
              :border "transparent"
              :border-radius "1px"
              :font "inherit"
              :outline "inherit"
              :clip-path "polygon(calc(100% - 7px) 0, 100% calc(50% - 1px), 100% 50%, 100% calc(50% + 1px), calc(100% - 7px) 100%, 0% 100%, 7px calc(50% + 1px), 7px 50%, 7px calc(50% - 1px),  0% 0%)")
            `("#modes"
              :background-color ,theme:primary
              :color ,theme:on-primary
              :font-size "60vh"
              :text-align "right"
              :padding-left "7px"
              :padding-right "5px"
              :overflow-x "scroll"
              :white-space "nowrap"
              :z-index "2")
            `("#modes::-webkit-scrollbar"
              :display "none")
            `(button
              :background "transparent"
              :color "inherit"
              :text-decoration "transparent"
              :border "transparent"
              :border-radius "0.2em"
              :padding 0
              :font "inherit"
              :outline "inherit")
            `((:and (:or .button .tab "#url") :hover)
              :cursor "pointer"
              :background-color ,theme:action
              :color ,theme:on-action)
            `((:and (:or .button .tab) :active)
              :background-color ,theme:action-
              :color ,theme:on-action)
            `(.selected-tab
              :background-color ,theme:background+
              :color ,theme:on-background))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class))

(defmethod (setf height) (value (status-buffer status-buffer))
  (setf (ffi-height status-buffer) value)
  (setf (slot-value status-buffer 'height) value))

(export-always 'mode-status)
(defgeneric mode-status (status mode)
  (:method ((status status-buffer) (mode mode))
    (if (glyph-mode-presentation-p status)
        (glyph mode)
        (princ-to-string mode)))
  (:documentation "Return a MODE `mode' string description for the STATUS `status-buffer'.
Upon returning NIL, the mode is not displayed."))

(defun sort-modes-for-status (modes)
  "Return visible modes in MODES, with `nyxt/mode/keyscheme:keyscheme-mode' placed
first."
  (multiple-value-bind (keyscheme-mode other-modes)
      (sera:partition #'nyxt/mode/keyscheme::keyscheme-mode-p
                      (sera:filter #'visible-in-status-p modes))
    (append keyscheme-mode other-modes)))

(export-always 'format-status-modes)
(define-generic format-status-modes ((status status-buffer))
  "Render the enabled modes to HTML string.
Any `nyxt/mode/keyscheme:keyscheme-mode' is placed first.

This leverages `mode-status' which can be specialized for individual modes.
Augment this with `style' of STATUS, if necessary."
  (let ((buffer (current-buffer (window status))))
    (if (modable-buffer-p buffer)
        (let ((sorted-modes (sort-modes-for-status (modes buffer))))
          (spinneret:with-html-string
            (when (nosave-buffer-p buffer) (:span "⚠ nosave"))
            (:nbutton
              :buffer status
              :text "✚"
              :title (modes-string buffer)
              '(nyxt:toggle-modes))
            (loop for mode in sorted-modes
                  collect
                  (let ((mode mode))
                    (alex:when-let ((formatted-mode (mode-status status mode)))
                      (:nbutton
                        :buffer status
                        :text formatted-mode
                        :title (format nil "Describe ~a" mode)
                        `(describe-class :class (quote ,(name mode)))))))))
        "")))

(defun modes-string (buffer)
  (when (modable-buffer-p buffer)
    (format nil "~{~a~^~%~}" (append '("Enabled modes:")
                                     (mapcar #'princ-to-string (modes buffer))))))

(export-always 'format-status-buttons)
(define-generic format-status-buttons ((status status-buffer))
  "Render interactive buttons to HTML string.
Augment this with `style' of STATUS, if necessary."
  (spinneret:with-html-string
    (:nbutton
      :buffer status
      :text (:raw (glyph-left status))
      :title "History-Backwards"
      '(nyxt/mode/history:history-backwards))
    (:nbutton
      :buffer status
      :text (:raw (glyph-right status))
      :title "History-Forwards"
      '(nyxt/mode/history:history-forwards))
    (:nbutton
      :buffer status
      :id "reload"
      :text (:raw (glyph-reload status))
      :title "Reload"
      '(nyxt:reload-current-buffer))
    (:nbutton
      :buffer status
      :text (:raw (glyph-lambda status))
      :title "Execute-Command Menu"
      '(nyxt:execute-command))))

(export-always 'format-status-load-status)
(define-generic format-status-load-status ((status status-buffer))
  "Render the load status to HTML string.
By default, renders a spinning loading ring when loading a URL.
Augment this with `style' of STATUS, if necessary."
  (let ((buffer (current-buffer (window status))))
    (spinneret:with-html-string
      (:div :class (if (and (web-buffer-p buffer)
                            (eq (slot-value buffer 'status) :loading))
                       "loader" "")))))

(export-always 'format-status-url)
(define-generic format-status-url ((status status-buffer))
  "Formats the currently open URL for the STATUS buffer.

MODIFY AT YOUR OWN RISK! The current implementation goes a great way to make the
display safe, in particular to prevent IDN-spoofing by showing the Unicode
domains as punicode (the Unicode aesthetic domain is shown in parentheses after
the URL.)

Augment this with `style' of STATUS, if necessary."
  ;; However this function changes, it should always:
  ;; - Prevent IDN/Unicode-spoofing.
  ;; - Clearly show subdomains (except maybe "trivial" ones) to prevent
  ;;   subdomain takeover attacks.
  ;; - Retain a clear display of which protocol/scheme is used to discourage
  ;;   e.g. trusting HTTP websites.
  (or
   (sera:and-let* ((buffer (current-buffer (window status)))
                   (content (multiple-value-bind (aesthetic safe)
                                (render-url (url buffer))
                              (uiop:strcat
                               (if safe
                                   (format nil "~a (~a)" safe aesthetic)
                                   aesthetic)
                               (when (title buffer)
                                 (str:concat " — " (title buffer)))
                               (when (find (url buffer) (remove buffer (buffer-list))
                                           :test #'url-equal :key #'url)
                                 (format nil " (buffer ~a)" (id buffer)))))))
     (spinneret:with-html-string
       (:nbutton
         :buffer status
         :text content
         :title content
         '(nyxt:set-url))))
   ""))

(export-always 'format-status-tabs)
(define-generic format-status-tabs ((status status-buffer))
  "Render the open buffers to HTML string suitable for STATUS.
Augment this with `style' of STATUS, if necessary."
  ;; FIXME: remove nil here because early on startup some buffers can be NIL
  ;; (why?)  and we have to clean them out. Debug the startup sequence (in
  ;; particular the (setf buffers) :after handler) and remove this.
  (let* ((buffers (remove
                   nil (if (display-tabs-by-last-access-p status)
                           (sort-by-time (buffer-list))
                           (reverse (buffer-list)))))
         (domain-deduplicated-urls (remove-duplicates
                                    (mapcar #'url buffers)
                                    :test #'string=
                                    :key #'quri:uri-domain)))
    (spinneret:with-html-string
      (loop for url in domain-deduplicated-urls
            collect
            ;; FIXME: Removing NIL buffers here too, the same reason as above.
            (let* ((internal-buffers (remove-if-not #'internal-url-p buffers :key #'url))
                   (domain (quri:uri-domain url))
                   (tab-display-text (if (internal-url-p url)
                                         "internal"
                                         domain))
                   (url url)
                   ;; Current buffer might be NIL too.
                   (current (current-buffer (window status))))
              (:span
               :class (if (and current
                               (string= (quri:uri-domain (url current))
                                        (quri:uri-domain url)))
                          "selected-tab tab"
                          "tab")
               :onclick (ps:ps
                          (if (or (= (ps:chain window event which) 2)
                                  (= (ps:chain window event which) 4))
                              (nyxt/ps:lisp-eval
                               (:title "delete-tab-group"
                                :buffer status)
                               (let ((buffers-to-delete
                                       (if (internal-url-p url)
                                           internal-buffers
                                           (sera:filter (match-domain domain) buffers))))
                                 (prompt
                                  :prompt "Delete buffer(s)"
                                  :sources (make-instance 'buffer-source
                                                          :constructor buffers-to-delete
                                                          :marks buffers-to-delete
                                                          :actions-on-return (list (lambda-mapped-command buffer-delete))))))
                              (nyxt/ps:lisp-eval
                               (:title "select-tab-group"
                                :buffer status)
                               (if (internal-url-p url)
                                   (prompt
                                    :prompt "Switch to buffer with internal page"
                                    :sources (make-instance 'buffer-source
                                                            :constructor internal-buffers))
                                   (nyxt::switch-buffer-or-query-domain domain)))))
               tab-display-text))))))

(export-always 'format-status)
(define-generic format-status ((status status-buffer))
  "Return a string corresponding to the body of the HTML document of STATUS.

To override all that is displayed on STATUS, redefine this method.  To partially
override it, redefine methods such as `format-status-url' or
`format-status-modes'."
  (let* ((buffer (current-buffer (window status))))
    (spinneret:with-html-string
      (:div :id "container"
            (:div :id "controls" :class "arrow-right"
                  (:raw (format-status-buttons status)))
            (:div :id "url" :class "arrow-right"
                  (:raw
                   (format-status-load-status status)
                   (format-status-url status)))
            (:div :id "tabs"
                  (:raw
                   (format-status-tabs status)))
            (:div :id "modes" :class "arrow-left"
                  :title (modes-string buffer)
                  (:raw
                   (format-status-modes status)))))))

(defvar *setf-handlers* (sera:dict)
  "A hash-table mapping (CLASS SLOT) pairs to hash-tables mapping OBJECT to HANDLER.
OBJECT may be any value.
HANDLER is a function of argument the CLASS instance.

See `define-setf-handler'.")

(export-always 'define-setf-handler)
(defmacro define-setf-handler (class-name slot bound-object handler) ; TODO: SLOT or WRITER?
  "When (setf (SLOT-WRITER CLASS-INSTANCE) VALUE) is called,
all handlers corresponding to (CLASS SLOT) are evaluated with CLASS-INSTANCE as
argument.

There is a unique HANDLER per BOUND-OBJECT.
When BOUND-OBJECT is garbage-collected, the corresponding handler is automatically removed."
  (alex:with-gensyms (key)
    (alex:once-only (bound-object)
      `(progn
         (let ((,key (list (find-class ',class-name) ',slot)))
           (handler-bind ((warning (if (gethash ,key *setf-handlers*)
                                       #'muffle-warning ; To avoid warning on redefinition.
                                       #'identity)))
             (setf (gethash
                    ,bound-object
                    (alex:ensure-gethash ,key *setf-handlers*
                                         (tg:make-weak-hash-table :test 'equal :weakness :key)))
                   ,handler)
             (defmethod (setf ,slot) :after (value (,class-name ,class-name))
               (declare (ignorable value))
               (dolist (handler (alex:hash-table-values (gethash ,key *setf-handlers*)))
                 (funcall* handler ,class-name)))))))))

(defmethod customize-instance :after ((status-buffer status-buffer) &key)
  "Add setf-handlers calling `print-status' on:
- `buffer' `modes',
- `document-buffer' `url',
- `document-buffer' `title',
- `window' `active-buffer'.

See also `define-setf-handler'."
  ;; We need to watch both the buffer `modes' slot and the status of modes,
  ;; since the mode list does not change when a mode gets disabled.
  (define-setf-handler modable-buffer modes status-buffer
    (lambda (buffer)
      (when (window status-buffer)
        (when (eq buffer (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler mode enabled-p status-buffer
    (lambda (mode)
      (when (window status-buffer)
        (when (eq (buffer mode) (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler document-buffer url status-buffer
    (lambda (buffer)
      (when (window status-buffer)
        (when (eq buffer (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler document-buffer title status-buffer
    (lambda (buffer)
      (when (window status-buffer)
        (when (eq buffer (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler window active-buffer status-buffer
    (lambda (window)
      (when (eq window (window status-buffer))
        (print-status (window status-buffer)))))
  (define-setf-handler network-buffer status status-buffer
    (lambda (buffer)
      (when (window status-buffer)
        (when (eq buffer (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler browser buffers status-buffer
    (lambda (browser)
      (declare (ignore browser))
      (mapc #'print-status (window-list)))))
