;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class status-buffer (input-buffer)
  ((window
    nil
    :type (maybe window)
    :documentation "The `window' to which the status buffer is attached.")
   (height
    40
    :type integer
    :writer nil
    :reader height
    :export t
    :documentation "The height of the status buffer in pixels.")
   (glyph-mode-presentation-p
    nil
    :documentation "Display the modes as a list of glyphs.")
   (glyph-left (gethash "left.svg" *static-data*))
   (glyph-right (gethash "right.svg" *static-data*))
   (glyph-reload (gethash "reload.svg" *static-data*))
   (glyph-lambda (gethash "lambda.svg" *static-data*))
   (glyph-plus (gethash "plus.svg" *static-data*))
   (style
    (theme:themed-css (theme *browser*)
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "400" :src "url('nyxt-resource:PublicSans-Regular.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "400" :src "url('nyxt-resource:PublicSans-Italic.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "100" :src "url('nyxt-resource:PublicSans-Thin.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "100" :src "url('nyxt-resource:PublicSans-ThinItalic.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "200" :src "url('nyxt-resource:PublicSans-ExtraLight.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "200" :src "url('nyxt-resource:PublicSans-ExtraLightItalic.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "300" :src "url('nyxt-resource:PublicSans-Light.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "300" :src "url('nyxt-resource:PublicSans-LightItalic.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "500" :src "url('nyxt-resource:PublicSans-Medium.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "500" :src "url('nyxt-resource:PublicSans-MediumItalic.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "600" :src "url('nyxt-resource:PublicSans-SemiBold.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "600" :src "url('nyxt-resource:PublicSans-SemiBoldItalic.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "700" :src "url('nyxt-resource:PublicSans-Bold.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "700" :src "url('nyxt-resource:PublicSans-BoldItalic.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "800" :src "url('nyxt-resource:PublicSans-ExtraBold.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "800" :src "url('nyxt-resource:PublicSans-ExtraBoldItalic.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "normal" :font-weight
        "900" :src "url('nyxt-resource:PublicSans-Black.woff')"
        "format('woff')")
      '(:font-face :font-family "public sans" :font-style "italic" :font-weight
        "900" :src "url('nyxt-resource:PublicSans-BlackItalic.woff')"
        "format('woff')")
      '(:font-face :font-family "dejavu sans mono" :src
        "url('nyxt-resource:DejaVuSansMono.ttf')" "format('ttf')")
      '(html
        :height "100%"
        :overflow "hidden")
      `(body
        :display "flex"
        :align-items "center"
        :height "100%"
        :overflow "hidden"
        :box-sizing "border-box"
        :background-color ,(format nil "color-mix(in srgb, ~a 55%, ~a)"
                                  theme:secondary-color+
                                  theme:background-color)
        :color ,theme:on-secondary-color
        :font-family ,theme:font-family
        :font-size "13px"
        :line-height "16px"
        :white-space "nowrap"
        :user-select "none"
        :padding 0
        :margin 0)
      '(*
        :box-sizing "border-box")
      '("#container"
        :display "flex"
        :align-items "center"
        :flex "auto"
        :min-width "0"
        :height "28px"
        :gap "12px"
        :padding "0 8px"
        :overflow "hidden")
      '("#controls"
        :display "flex"
        :align-items "center"
        :gap "5px"
        :flex "none")
      `("#controls button, #new-buffer"
        :display "inline-flex"
        :align-items "center"
        :justify-content "center"
        :width "28px"
        :height "28px"
        :margin 0
        :padding 0
        :overflow "hidden"
        :border-radius "7px"
        :color ,theme:on-secondary-color
        :cursor "pointer")
      '("#controls svg, #new-buffer svg"
        :display "block"
        :flex "none"
        :height "24px"
        :width "auto")
      `("#url"
        :display "flex"
        :align-items "center"
        :gap "5px"
        :flex "1 1 144px"
        :min-width "122px"
        :height "28px"
        :margin 0
        :padding "0 12px"
        :border-radius "7px"
        :background-color ,theme:background-color+
        :color ,theme:on-background-color
        :overflow "hidden"
        :cursor "pointer")
      `("#url > span"
        :flex "none"
        :font-size "13px"
        :font-weight "400"
        :line-height "16px"
        :color ,theme:on-background-color)
      '("#url > span:empty"
        :display "none")
      '("#url button"
        :flex "auto"
        :min-width "0"
        :height "28px"
        :margin 0
        :padding 0
        :overflow "hidden"
        :text-overflow "ellipsis"
        :text-align "left"
        :background-color "transparent"
        :border-radius "0"
        :font-size "13px"
        :font-weight "400"
        :line-height "28px"
        :cursor "pointer")
      '("#buffers"
        :display "flex"
        :align-items "center"
        :gap "5px"
        :flex "1 1 auto"
        :min-width "0"
        :overflow "hidden")
      '("#tabs"
        :display "flex"
        :align-items "center"
        :gap "5px"
        :flex "0 1 auto"
        :min-width "40px"
        :overflow-x "auto")
      '("#tabs::-webkit-scrollbar"
        :display "none")
      `(".tab"
        :flex "none"
        :height "28px"
        :margin 0
        :padding "0 12px"
        :border-radius "7px"
        :font-size "13px"
        :font-weight "400"
        :line-height "28px"
        :text-align "center"
        :white-space "nowrap"
        :color ,theme:on-secondary-color
        :background-color "transparent"
        :cursor "pointer")
      `("#modes"
        :display "flex"
        :align-items "center"
        :flex "none"
        :height "28px"
        :margin 0
        :padding 0
        :overflow-x "auto"
        :background-color "transparent"
        :color ,theme:on-secondary-color
        :font-size "13px"
        :font-weight "400"
        :line-height "28px")
      '("#modes::-webkit-scrollbar"
        :display "none")
      '("#modes button"
        :height "28px"
        :margin 0
        :padding "0 5px"
        :border-radius "7px"
        :font-size "13px"
        :font-weight "400"
        :line-height "28px"
        :color "inherit"
        :background-color "transparent"
        :cursor "pointer")
      '(button
        :appearance "none"
        :background-color "transparent"
        :color "inherit"
        :text-decoration "none"
        :border "none"
        :border-radius "7px"
        :margin 0
        :padding 0
        :font "inherit"
        :outline "none")
      `("#controls button:hover, #new-buffer:hover, #modes button:hover"
        :background-color ,theme:secondary-color)
      `(".tab:hover"
        :background-color ,(format nil "color-mix(in srgb, ~a 60%, ~a)"
                                   theme:secondary-color
                                   theme:background-color))
      `("#url:hover"
        :background-color ,theme:background-color)
      `("button:focus-visible"
        :outline "2px solid"
        :outline-color ,theme:action-color
        :outline-offset "0")
      `(.selected-tab
        :background-color ,(format nil "color-mix(in srgb, ~a 60%, ~a)"
                                   theme:secondary-color
                                   theme:background-color)
        :color ,theme:on-secondary-color
        :font-weight "400")
      `(".selected-tab:hover"
        :background-color ,(format nil "color-mix(in srgb, ~a 60%, ~a)"
                                   theme:secondary-color
                                   theme:background-color)
        :color ,theme:on-secondary-color))))
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
  (:documentation "Return a MODE `mode' string description for the STATUS
 `status-buffer'. Upon returning NIL, the mode is not displayed."))

(defun sort-modes-for-status (modes)
  "Return visible modes in MODES, with `nyxt/mode/keyscheme:keyscheme-mode'
 placed first."
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
    (spinneret:with-html
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
                  `(describe-class :class (quote ,(name mode))))))))))

(defmethod modes-string ((buffer buffer))
  (format nil "~{~a~^~%~}" (append '("Enabled modes:")
                                   (mapcar #'princ-to-string
                                           (enabled-modes buffer)))))

(export-always 'format-status-buttons)
(defmethod format-status-buttons ((status status-buffer))
  "Render interactive buttons to HTML string."
  (spinneret:with-html
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
    (spinneret:with-html
      (:span
       (if (and (web-buffer-p buffer)
                (eq (slot-value buffer 'status) :loading))
           "⧖ "
           "")))))

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
    (spinneret:with-html
      (:nbutton :buffer status :text url-display :title (title buffer)
        '(nyxt:set-url)))))

(export-always 'format-status-tabs)
(defun format-internal-tab-display-text (url)
  "Return display text for internal URL tabs.
The text is the page name. A standard parser may store that name in the host,
or in the path under `page'."
  (let ((scheme (quri:uri-scheme url))
        (name (internal-page-name url)))
    (if name
        (format nil "~a:~a" scheme (symbol->param-name name))
        (let* ((path (or (quri:uri-path url) "/"))
               (host (quri:uri-host url))
               (path-display (if (and (member :nyxt-cef-status-host-as-path *features*)
                                      (or (null (quri:uri-path url))
                                          (string= (quri:uri-path url) "/"))
                                      host
                                      (not (uiop:emptyp host)))
                                 host
                                 path)))
          (format nil "~a:~a" scheme path-display)))))

(defmethod format-status-tabs ((status status-buffer))
  "Render the open buffers to HTML string suitable for STATUS."
  (let* ((buffers (reverse (buffer-list)))
         (current-buffer (active-buffer (window status))))
    (spinneret:with-html
      (loop for buffer in buffers
            collect
            (let* ((buffer buffer)
                   (url (url buffer))
                   (domain (quri:uri-domain url))
                   (tab-display-text (if (internal-url-p url)
                                         (format-internal-tab-display-text url)
                                         domain)))
              (:span
               :class (if (eq current-buffer buffer)
                          "selected-tab tab"
                          "tab")
               :title (if (uiop:emptyp (title buffer))
                          tab-display-text
                          (title buffer))
               :onclick (ps:ps
                          (nyxt/ps:lisp-eval
                           (:title "select-tab" :buffer status)
                           (set-current-buffer buffer)))
               tab-display-text))))))

(defmethod update-status-modes ((status status-buffer))
  "Update ONLY the modes to avoid redrawing the whole status buffer."
  (ps-eval :async t :buffer status
    (setf (ps:@ (nyxt/ps:qs document "#modes") |innerHTML|)
          (ps:lisp
           (spinneret:with-html-string (format-status-modes status))))))

(defmethod update-status-tabs ((status status-buffer))
  "Update ONLY the status tabs to avoid redrawing the whole status buffer."
  (ps-eval :async t :buffer status
    (setf (ps:@ (nyxt/ps:qs document "#tabs") |innerHTML|)
          (ps:lisp
           (spinneret:with-html-string (format-status-tabs status))))))

(defmethod update-status-url ((status status-buffer))
  "Update ONLY the URL to avoid redrawing the whole status buffer."
  (ps-eval :async t :buffer status
    (setf (ps:@ (nyxt/ps:qs document "#url") |innerHTML|)
          (ps:lisp
           (spinneret:with-html-string (format-status-url status))))))

(export-always 'format-status-new-buffer)
(defmethod format-status-new-buffer ((status status-buffer))
  "Render the new-buffer button to HTML string."
  (spinneret:with-html
    (:nbutton
      :buffer status
      :id "new-buffer"
      :text (:raw (glyph-plus status))
      :title "New buffer"
      '(nyxt:make-buffer-focus))))

(export-always 'format-status)
(defmethod format-status ((status status-buffer))
  "Return a string corresponding to the body of the HTML document of STATUS.

To override all that is displayed on STATUS, redefine this method. To partially
override it, redefine methods such as `format-status-url',
`format-status-tabs', or `format-status-new-buffer'."
  (let* ((buffer (active-buffer (window status))))
    (spinneret:with-html-string
      (:div :id "container"
            (:div :id "controls"
                  (format-status-buttons status))
            (:div :id "url"
                  (format-status-load-status status)
                  (format-status-url status))
            (:div :id "buffers"
                  (:div :id "tabs"
                        (format-status-tabs status))
                  (format-status-new-buffer status))
            (:div :id "modes"
                  :title (modes-string buffer)
                  (format-status-modes status))))))

(defmethod show-selected-tab ((status status-buffer))
  "Scroll the selected tab into view."
  (ps-eval :async t :buffer status
    (let ((selected-tab
            (ps:aref
             (ps:chain document
                       (get-elements-by-class-name "selected-tab")) 0)))
      (if selected-tab
          (ps:chain selected-tab
                    (scroll-into-view (ps:create inline "center"
                                                 behavior "smooth")))))))

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

When BOUND-OBJECT is garbage-collected, the corresponding handler is
automatically removed."
  (alex:with-gensyms (key)
    `(let ((,key (list (find-class ',class-name) ',slot)))
       (handler-bind ((warning (if (gethash ,key *setf-handlers*)
                                   #'muffle-warning
                                   #'identity)))
         (setf (gethash ,bound-object
                        (alex:ensure-gethash
                         ,key
                         *setf-handlers*
                         (tg:make-weak-hash-table :test 'equal
                                                  :weakness :key)))
               ,handler)
         (defmethod (setf ,slot) :after (value (,class-name ,class-name))
           (declare (ignorable value))
           (dolist (handler (alex:hash-table-values
                             (gethash ,key *setf-handlers*)))
             (funcall* handler ,class-name)))))))

(defmethod print-status ((status-buffer status-buffer))
  (ffi-print-status status-buffer (format-status status-buffer))
  (show-selected-tab status-buffer))

(defmethod customize-instance :after ((status-buffer status-buffer) &key)
  "Add handlers to redraw STATUS-BUFFER.
See `define-setf-handler'."
  (with-slots (window) status-buffer
    (define-setf-handler modable-buffer modes status-buffer
      (lambda (buffer) (when (eq buffer (active-buffer window))
                         (print-status status-buffer))))
    (define-setf-handler mode enabled-p status-buffer
      (lambda (mode) (when (eq (buffer mode) (active-buffer window))
                       (print-status status-buffer))))
    (define-setf-handler document-buffer url status-buffer
      (lambda (_) (declare (ignore _))
        (print-status status-buffer)))
    (define-setf-handler window active-buffer status-buffer
      (lambda (_) (declare (ignore _))
        (update-status-url status-buffer)
        (update-status-modes status-buffer)
        (update-status-tabs status-buffer)
        (show-selected-tab status-buffer)))
    (define-setf-handler network-buffer status status-buffer
      (lambda (buffer) (when (eq buffer (active-buffer window))
                         (print-status status-buffer))))
    (define-setf-handler browser buffers status-buffer
      (lambda (_) (declare (ignore _))
        (print-status status-buffer)))))
