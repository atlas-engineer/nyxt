;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class status-buffer (input-buffer)
  ((window
    nil
    :type (maybe window)
    :documentation "The `window' to which the status buffer is attached.")
   (height
    36
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
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "400" :src "url('nyxt-resource:PublicSans-Regular.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "400" :src "url('nyxt-resource:PublicSans-Italic.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "100" :src "url('nyxt-resource:PublicSans-Thin.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "100" :src "url('nyxt-resource:PublicSans-ThinItalic.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "200" :src "url('nyxt-resource:PublicSans-ExtraLight.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "200" :src "url('nyxt-resource:PublicSans-ExtraLightItalic.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "300" :src "url('nyxt-resource:PublicSans-Light.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "300" :src "url('nyxt-resource:PublicSans-LightItalic.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "500" :src "url('nyxt-resource:PublicSans-Medium.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "500" :src "url('nyxt-resource:PublicSans-MediumItalic.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "600" :src "url('nyxt-resource:PublicSans-SemiBold.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "600" :src "url('nyxt-resource:PublicSans-SemiBoldItalic.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "700" :src "url('nyxt-resource:PublicSans-Bold.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "700" :src "url('nyxt-resource:PublicSans-BoldItalic.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "800" :src "url('nyxt-resource:PublicSans-ExtraBold.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "800" :src "url('nyxt-resource:PublicSans-ExtraBoldItalic.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "normal" :font-weight "900" :src "url('nyxt-resource:PublicSans-Black.woff')" "format('woff')")
            '(:font-face :font-family "public sans" :font-style "italic" :font-weight "900" :src "url('nyxt-resource:PublicSans-BlackItalic.woff')" "format('woff')")
            '(:font-face :font-family "dejavu sans mono" :src "url('nyxt-resource:DejaVuSansMono.ttf')" "format('ttf')")
            `(body
              :background-color ,theme:secondary-color+
              :color ,theme:on-secondary-color
              :font-family ,theme:font-family
              :line-height "100vh"
              :font-size "40vh"
              :white-space "nowrap"
              :padding 0
              :margin 0)
            '("#container"
              :display "flex"
              :justify-content "space-between"
              :overflow-y "hidden")
            `("#controls"
              :z-index "3"
              :flex-basis "90px"
              :display "flex")
            '("#controls > button"
              :margin-right "-3px"
              :max-width "24px"
              :height "100%"
              :aspect-ratio "1/1")
            `("#url"
              :background-color ,theme:background-color-
              :color ,theme:on-background-color
              :min-width "100px"
              :line-height "75vh"
              :margin "4px"
              :padding-left "8px"
              :border-radius "4px"
              :z-index "2"
              :flex-grow "3"
              :flex-shrink "2"
              :overflow "hidden"
              :flex-basis "144px")
            '("#url button"
              :text-align "left"
              :width "100%")
            `("#tabs"
              :line-height "75vh"
              :min-width "100px"
              :text-align "left"
              :padding-left "3px"
              :z-index "1"
              :flex-grow "10"
              :flex-shrink "4"
              :flex-basis "144px")
            '("#tabs::-webkit-scrollbar"
              :display "none")
            `(".tab"
              :border-radius "4px"
              :color ,theme:on-secondary-color
              :display "inline-block"
              :padding-left "18px"
              :padding-right "18px"
              :margin "4px"
              :text-decoration "transparent"
              :font "inherit"
              :outline "inherit")
            `("#modes"
              :border-radius "4px"
              :background-color ,theme:secondary-color
              :color ,theme:on-background-color
              :padding-left "4px"
              :text-align "right"
              :z-index "2"
              :padding-right "3px")
            '("#modes > button"
              :border-radius "0"
              :padding-left "3px"
              :padding-right "3px")
            '("#reload"
              :margin-top "1px")
            '("#modes::-webkit-scrollbar"
              :display "none")
            '(button
              :background "transparent"
              :color "inherit"
              :text-decoration "transparent"
              :border "transparent"
              :border-radius "2px"
              :padding 0
              :font "inherit"
              :outline "inherit")
            `((:and (:or .button .tab "#url") :hover)
              :cursor "pointer"
              :background-color ,theme:action-color
              :color ,theme:on-action-color)
            `((:and (:or .button .tab) :active)
              :background-color ,theme:action-color-
              :color ,theme:on-action-color)
            `(.selected-tab
              :background-color ,theme:background-color+
              :color ,theme:on-background-color))))
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
        (str:downcase (name mode))))
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
(defmethod format-status-modes ((status status-buffer))
  "Render the enabled modes to HTML string.
Any `nyxt/mode/keyscheme:keyscheme-mode' is placed first.

This leverages `mode-status' which can be specialized for individual modes."
  (let ((buffer (active-buffer (window status))))
    (if (modable-buffer-p buffer)
        (spinneret:with-html-string
          (:nbutton
            :buffer status
            :text "±"
            :title (modes-string buffer)
            '(nyxt:toggle-modes))
          (loop for mode in (sort-modes-for-status (enabled-modes buffer))
                collect
                (let ((mode mode))
                  (when-let ((formatted-mode (mode-status status mode)))
                    (:nbutton
                      :buffer status
                      :text formatted-mode
                      :title (format nil "Describe ~a" mode)
                      `(describe-class :class (quote ,(name mode))))))))
        "")))

(defun modes-string (buffer)
  (when (modable-buffer-p buffer)
    (format nil "~{~a~^~%~}" (append '("Enabled modes:")
                                     (mapcar #'princ-to-string (enabled-modes buffer))))))

(export-always 'format-status-buttons)
(defmethod format-status-buttons ((status status-buffer))
  "Render interactive buttons to HTML string."
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
(defmethod format-status-load-status ((status status-buffer))
  "Render the load status to HTML string.
By default, renders a hourglass when loading a URL."
  (let ((buffer (active-buffer (window status))))
    (if (and (web-buffer-p buffer)
             (eq (slot-value buffer 'status) :loading))
        "⧖ "
        "")))

(export-always 'format-status-url)
(defmethod format-status-url ((status status-buffer))
  "Format the current URL for the STATUS buffer."
  (let* ((buffer (active-buffer (window status)))
         (url (url buffer))
         (url-display (cond ((quri:uri-https-p url)
                             (quri:uri-host url))
                            ((quri:uri-http-p url)
                             (format nil "! ~a" (render-url url)))
                            (t (render-url url)))))
    (spinneret:with-html-string
      (:nbutton :buffer status :text url-display :title (title buffer)
        '(nyxt:set-url)))))

(export-always 'format-status-tabs)
(defmethod format-status-tabs ((status status-buffer))
  "Render the open buffers to HTML string suitable for STATUS."
  (let* ((buffers (if (display-tabs-by-last-access-p status)
                      (sort-by-time (buffer-list))
                      (reverse (buffer-list))))
         (domain-deduplicated-urls (remove-duplicates (mapcar #'url buffers)
                                                      :test #'string=
                                                      :key #'quri:uri-domain)))
    (spinneret:with-html-string
      (loop for url in domain-deduplicated-urls
            collect
            (let* ((internal-buffers (internal-buffer-list))
                   (domain (quri:uri-domain url))
                   (tab-display-text (if (internal-url-p url) "internal" domain))
                   (url url)
                   (current-buffer (active-buffer (window status))))
              (:span
               :class (if (string= (quri:uri-domain (url current-buffer))
                                   (quri:uri-domain url))
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
                                   (switch-buffer-or-query-domain domain)))))
               tab-display-text))))))

(export-always 'format-status)
(defmethod format-status ((status status-buffer))
  "Return a string corresponding to the body of the HTML document of STATUS.

To override all that is displayed on STATUS, redefine this method.  To partially
override it, redefine methods such as `format-status-url' or
`format-status-modes'."
  (let* ((buffer (active-buffer (window status))))
    (spinneret:with-html-string
      (:div :id "container"
            #-darwin
            (:div :id "controls"
                  (:raw (format-status-buttons status)))
            (:div :id "url"
                  (:raw
                   (format-status-load-status status)
                   (format-status-url status)))
            (:div :id "tabs"
                  (:raw
                   (format-status-tabs status)))
            (:div :id "modes"
                  :title (modes-string buffer)
                  (:raw
                   (format-status-modes status)))))))

(defvar *setf-handlers* (sera:dict)
  "A hash-table mapping (CLASS SLOT) pairs to (OBJECT HANDLER) pairs.
OBJECT is an instance of CLASS.
HANDLER is a function that takes a CLASS instance as argument.

See `define-setf-handler'.")

(export-always 'define-setf-handler)
(defmacro define-setf-handler (class-name slot bound-object handler)
  "When (setf (SLOT-WRITER CLASS-INSTANCE) VALUE) is called,
all handlers corresponding to (CLASS SLOT) are evaluated with CLASS-INSTANCE as
argument.

There is a unique HANDLER per BOUND-OBJECT.
When BOUND-OBJECT is garbage-collected, the corresponding handler is automatically removed."
  (alex:with-gensyms (key)
    `(let ((,key (list (find-class ',class-name) ',slot)))
       (handler-bind ((warning (if (gethash ,key *setf-handlers*)
                                   #'muffle-warning ; To avoid warning on redefinition.
                                   #'identity)))
         (setf (gethash ,bound-object
                        (alex:ensure-gethash ,key
                                             *setf-handlers*
                                             (tg:make-weak-hash-table :test 'equal
                                                                      :weakness :key)))
               ,handler)
         (defmethod (setf ,slot) :after (value (,class-name ,class-name))
           (declare (ignorable value))
           (dolist (handler (alex:hash-table-values (gethash ,key *setf-handlers*)))
             (funcall* handler ,class-name)))))))

(defmethod customize-instance :after ((status-buffer status-buffer) &key)
  "Add handlers to redraw STATUS-BUFFER.
See `define-setf-handler'."
  (with-slots (window) status-buffer
    ;; Watching slots `modes' and `enabled-p' since the former isn't mutated
    ;; when a mode is disabled.
    (define-setf-handler modable-buffer modes status-buffer
      (lambda (buffer) (when (eq buffer (active-buffer window)) (print-status window))))
    (define-setf-handler mode enabled-p status-buffer
      (lambda (mode) (when (eq (buffer mode) (active-buffer window)) (print-status window))))
    (define-setf-handler document-buffer url status-buffer
      (lambda (buffer) (when (eq buffer (active-buffer window)) (print-status window))))
    (define-setf-handler window active-buffer status-buffer
      (lambda (win) (when (eq win window) (print-status window))))
    (define-setf-handler network-buffer status status-buffer
      (lambda (buffer) (when (eq buffer (active-buffer window)) (print-status window))))
    (define-setf-handler browser buffers status-buffer
      (lambda (_) (declare (ignore _)) (mapc #'print-status (window-list))))))
