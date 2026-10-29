;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(eval-always
  (define-class prompt-buffer
      (network-buffer input-buffer modable-buffer prompter:prompter)
    ((window
      nil
      :type (or null window)
      :export nil
      :documentation "The window in which the prompt buffer is showing.")
     (height
      :default
      :type (or keyword integer)
      :writer nil
      :reader height
      :export t
      :documentation "The height occupied by the prompt buffer.
The options are:
- `:default', which sets it to a third of the window's height;
- an integer, which corresponds to the height in pixels.")
     (prompter:history
      (prompt-buffer-generic-history *browser*)
      ;; Both set to nil since it overrides the default value.
      :accessor nil
      :export nil
      :documentation
      "Override `prompter:history' to use input history globally.")
     (invisible-input-p
      nil
      :documentation "Whether to replace input by a placeholder character.
This is useful to conceal passwords.")
     (hide-suggestion-count-p
      nil
      :documentation "Whether to hide the number of suggestions.
Affects both the prompt and its sources.")
     (composite-source-p
      nil
      :documentation "Whether to prepend a `composite-source' to `sources'.
The composite source lists the top `prompter:suggestions-per-source' suggestions
from each source, in source order, so a result can be chosen without moving
through every source.
`set-url' enables this. Other prompts leave it off.")
     (max-suggestions
      0
      :export nil
      :documentation "Maximum number of total suggestions that were listed at
some point.")
     (style
      (theme:themed-css (theme *browser*)
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "400" :src
          "url('nyxt-resource:PublicSans-Regular.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "400" :src
          "url('nyxt-resource:PublicSans-Italic.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "100" :src
          "url('nyxt-resource:PublicSans-Thin.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "100" :src
          "url('nyxt-resource:PublicSans-ThinItalic.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "200" :src
          "url('nyxt-resource:PublicSans-ExtraLight.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "200" :src
          "url('nyxt-resource:PublicSans-ExtraLightItalic.woff')"
          "format('woff')")
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "300" :src
          "url('nyxt-resource:PublicSans-Light.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "300" :src
          "url('nyxt-resource:PublicSans-LightItalic.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "500" :src
          "url('nyxt-resource:PublicSans-Medium.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "500" :src
          "url('nyxt-resource:PublicSans-MediumItalic.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "600" :src
          "url('nyxt-resource:PublicSans-SemiBold.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "600" :src
          "url('nyxt-resource:PublicSans-SemiBoldItalic.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "700" :src
          "url('nyxt-resource:PublicSans-Bold.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "700" :src
          "url('nyxt-resource:PublicSans-BoldItalic.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "800" :src
          "url('nyxt-resource:PublicSans-ExtraBold.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "800" :src
          "url('nyxt-resource:PublicSans-ExtraBoldItalic.woff')"
          "format('woff')")
        '(:font-face :font-family "public sans" :font-style "normal"
          :font-weight "900" :src
          "url('nyxt-resource:PublicSans-Black.woff')" "format('woff')")
        '(:font-face :font-family "public sans" :font-style "italic"
          :font-weight "900" :src
          "url('nyxt-resource:PublicSans-BlackItalic.woff')" "format('woff')")
        '(:font-face :font-family "dejavu sans mono" :src
          "url('nyxt-resource:DejaVuSansMono.ttf')" "format('ttf')")
        '(*
          :box-sizing "border-box")
        '(html
          :height "100%")
        `(body
          :height "100%"
          :overflow "hidden"
          :background-color ,theme:background-color
          :color ,theme:on-background-color
          :font-family ,theme:font-family
          :font-size "13px"
          :font-weight "400"
          :line-height "16px"
          :margin "0")
        '("#root"
          :height "100%"
          :display "grid"
          :grid-template-rows "auto 1fr")
        `("#prompt-area"
          :display "grid"
          :grid-template-columns "auto auto minmax(0, 1fr) auto auto"
          :align-items "center"
          :height "40px"
          :gap "8px"
          :padding "0 8px"
          :background-color ,(format nil "color-mix(in srgb, ~a 55%, ~a)"
                                     theme:secondary-color+
                                     theme:background-color)
          :color ,theme:on-secondary-color
          :box-shadow ,(format nil "inset 0 -1px 0 ~a" theme:background-color-))
        '("#prompt"
          :max-width "40ch"
          :overflow "hidden"
          :white-space "nowrap"
          :text-overflow "ellipsis"
          :font-size "13px"
          :font-weight "400"
          :line-height "28px")
        `("#prompt-extra"
          :font-family ,theme:monospace-font-family
          :font-size "13px"
          :font-weight "400"
          :line-height "28px"
          :white-space "nowrap")
        '("#prompt-modes"
          :max-width "24ch"
          :overflow "hidden"
          :white-space "nowrap"
          :text-overflow "ellipsis"
          :font-size "13px"
          :font-weight "400"
          :line-height "28px")
        '("#close-button"
          :display "flex"
          :align-items "center"
          :justify-content "center")
        '("#close-button button"
          :display "inline-flex"
          :align-items "center"
          :justify-content "center"
          :width "28px"
          :height "28px"
          :border-radius "7px"
          :font-size "17px"
          :font-weight "400"
          :line-height "1")
        '(button
          :appearance "none"
          :background-color "transparent"
          :color "inherit"
          :text-decoration "none"
          :border "none"
          :border-radius "7px"
          :margin "0"
          :padding "0"
          :font "inherit"
          :outline "none"
          :cursor "pointer")
        `("#input"
          :height "28px"
          :width "100%"
          :margin "0"
          :padding "6px 12px"
          :border "none"
          :border-radius "7px"
          :outline "none"
          :background-color ,theme:background-color+
          :color ,theme:on-background-color
          :font-family ,theme:font-family
          :font-size "13px"
          :font-weight "400"
          :line-height "16px"
          :caret-color ,theme:on-background-color)
        `("#input::selection"
          :background-color ,theme:action-color
          :color ,(let ((rgb (cl-colors-ng:as-rgb theme:on-background-color)))
                    (cl-colors-ng:print-hex
                     (cl-colors-ng:rgb (- 1 (cl-colors-ng:rgb-red rgb))
                                       (- 1 (cl-colors-ng:rgb-green rgb))
                                       (- 1 (cl-colors-ng:rgb-blue rgb)))
                     :print-alpha nil)))
        `("button:focus-visible"
          :outline "2px solid"
          :outline-color ,theme:action-color
          :outline-offset "0")
        `("#close-button button:hover"
          :background-color ,theme:secondary-color)
        `("#suggestions"
          :overflow "hidden"
          :padding-top "7px"
          :color ,theme:on-background-color)
        '(".source"
          :padding "0 8px")
        '(".source + .source"
          :margin-top "37px")
        `(".source-name"
          :display "flex"
          :align-items "center"
          :justify-content "space-between"
          :gap "5px"
          :height "28px"
          :color ,theme:on-background-color
          :font-size "13px"
          :font-weight "400"
          :line-height "28px")
        '(".source-name > div"
          :display "flex"
          :align-items "center"
          :gap "5px"
          :min-width "0")
        '(".source-name > div:first-child"
          :flex "1"
          :overflow "hidden")
        `(".source-name button"
          :display "inline-flex"
          :align-items "center"
          :justify-content "center"
          :flex "none"
          :width "28px"
          :height "28px"
          :border-radius "7px"
          :color ,theme:on-background-color)
        `(".source-name button:hover"
          :background-color ,theme:background-color-)
        '("#next-source, #previous-source"
          :width "8px")
        '("#previous-source"
          :margin-left "-3px")
        '("#next-source svg, #previous-source svg"
          :display "block"
          :width "8px"
          :height "5px")
        `("#toggle-attributes"
          :color ,theme:primary-color)
        '("#toggle-attributes .attribute-delta"
          :display "inline-block"
          :font-size "13px"
          :line-height "1"
          :transform "scaleY(0.65)"
          :transform-origin "center")
        `(".suggestion-and-mark-count"
          :flex "none"
          :font-family ,theme:monospace-font-family
          :font-size "11px"
          :line-height "16px"
          :color ,theme:primary-color)
        `(".source-content"
          :width "100%"
          :margin "0"
          :border "none"
          :border-collapse "separate"
          :border-spacing "0"
          :table-layout "fixed"
          :font-size "13px"
          :font-weight "400"
          :line-height "16px")
        `(".source-content th, .source-content td"
          :border "none"
          :overflow "hidden"
          :text-overflow "ellipsis"
          :white-space "nowrap"
          :text-align "left"
          :vertical-align "middle")
        `(".source-content th"
          :height "24px"
          :padding "0 12px"
          :background-color "transparent"
          :color ,theme:primary-color
          :font-size "11px"
          :font-weight "600"
          :line-height "16px"
          :box-shadow ,(format nil "inset 0 -1px 0 ~a" theme:background-color-))
        `(".source-content td"
          :height "28px"
          :padding "0 12px"
          :color ,theme:on-background-color
          :font-weight "400"
          :box-shadow ,(format nil "inset 0 -1px 0 ~a" theme:background-color-))
        '(".source-content th:first-child, .source-content td:first-child"
          :border-radius "7px 0 0 7px")
        '(".source-content th:last-child, .source-content td:last-child"
          :border-radius "0 7px 7px 0")
        '(".source-content th:only-child, .source-content td:only-child"
          :border-radius "7px")
        '(".source-content .mark"
          :width "28px"
          :padding "0"
          :overflow "visible"
          :text-align "center"
          :text-overflow "clip")
        `(".source-content .mark input"
          :display "inline-block"
          :width "13px"
          :height "13px"
          :margin "0"
          :vertical-align "middle"
          :accent-color ,theme:action-color)
        `(".source-content tr:hover td"
          :background-color ,theme:background-color-
          :box-shadow "none"
          :cursor "pointer")
        `("#selection td"
          :background-color ,(format nil "color-mix(in srgb, ~a 60%, ~a)"
                                     theme:secondary-color
                                     theme:background-color)
          :color ,theme:on-background-color
          :font-weight "400"
          :box-shadow "none")
        '("#selection td:first-child"
          :border-radius "5px 0 0 5px")
        '("#selection td:last-child"
          :border-radius "0 5px 5px 0")
        '("#selection td:only-child"
          :border-radius "5px")
        `(".marked td"
          :background-color ,(format nil "color-mix(in srgb, ~a 35%, ~a)"
                                     theme:secondary-color
                                     theme:background-color)
          :color ,theme:on-background-color
          :font-weight "400")
        `(".selected"
          :background-color ,(format nil "color-mix(in srgb, ~a 60%, ~a)"
                                     theme:secondary-color
                                     theme:background-color)
          :color ,theme:on-background-color
          :font-weight "400"))
      :documentation "The CSS applied to prompt buffer."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:export-predicate-name-p t)
    (:documentation "The prompt buffer is the interface for user interactions.
Each prompt spawns a new object: this makes it possible to nest prompts, such as
invoking `prompt-buffer:history'.

See `prompt' for how to invoke prompts.")
    (:metaclass user-class)))

(define-class composite-source (prompter:composite-source)
  ()
  (:metaclass user-class)
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:documentation "Prompt source listing the top suggestions from each other source.
How many are taken from each source is `prompter:suggestions-per-source', 1 by
default. Configure it with `define-configuration' on this class.
Each suggestion shows the originating source's default columns, with no column
headings. Every row has as many columns as the source with the most, so the
columns line up. The columns cannot be edited from here.
Selecting a suggestion runs the actions of the source it came from.
`set-url' prepends this source; other prompts enable it with `composite-source-p'."))

(defun ensure-prompt-source (specifier)
  "Return a `prompter:source' instance for SPECIFIER."
  (cond
    ((typep specifier 'prompter:source) specifier)
    ((and (symbolp specifier)
          (find-class specifier nil)
          (subtypep specifier 'prompter:source))
     (make-instance specifier))
    (t (error "Bad source specifier ~s." specifier))))

(defun add-composite-source (sources)
  "Return SOURCES with a `composite-source' of those sources prepended.
SOURCES may contain source instances or source class names."
  (let ((instances (mapcar #'ensure-prompt-source (uiop:ensure-list sources))))
    (cons (make-instance 'composite-source :sources instances)
          instances)))

(defmethod initialize-instance :around
    ((prompt-buffer prompt-buffer)
     &rest args
     &key composite-source-p (sources nil sources-p)
     &allow-other-keys)
  (if (and composite-source-p sources-p)
      (apply #'call-next-method
             prompt-buffer
             :sources (add-composite-source sources)
             (alexandria:remove-from-plist args :sources))
      (call-next-method)))

(defmethod customize-instance :after ((prompt-buffer prompt-buffer)
                                      &key extra-modes &allow-other-keys)
  (hooks:run-hook (prompt-buffer-make-hook *browser*) prompt-buffer)
  (enable-modes* (append (default-modes prompt-buffer)
                         (uiop:ensure-list extra-modes))
                 prompt-buffer))

(defmethod (setf height) (value (prompt-buffer prompt-buffer))
  (setf (ffi-height prompt-buffer)
        (case value
          (:default (round (/ (ffi-height (window prompt-buffer)) 4)))
          (t value)))
  (setf (slot-value prompt-buffer 'height) value))

(export-always 'current-source)
(defun current-source (&optional (prompt-buffer (current-prompt-buffer)))
  "Current PROMPT-BUFFER `prompter:source'.
If PROMPT-BUFFER is not provided, use `current-prompt-buffer'."
  (prompter:current-source prompt-buffer))

(export-always 'current-suggestion-value)
(defun current-suggestion-value
    (&optional (prompt-buffer (current-prompt-buffer)))
  "Return selected PROMPT-BUFFER `prompter:suggestion' `prompter:value'.
Return `prompter:source' as second value.
To access the suggestion instead, see `prompter:%current-suggestion'."
  (multiple-value-bind (suggestion source)
      (prompter:%current-suggestion prompt-buffer)
    (values (when suggestion (prompter:value suggestion)) source)))

(defmethod show-prompt-buffer ((prompt-buffer prompt-buffer))
  (with-slots (window) prompt-buffer
    (push prompt-buffer (active-prompt-buffers window))
    (lparallel.queue:push-queue prompt-buffer (prompt-buffer-ready-channel window)))
  ;; Height attachment can replace the document, so draw the skeleton after
  ;; the view is in the window. On CEF a write before that has no HTML element.
  (setf (height prompt-buffer) (slot-value prompt-buffer 'height))
  (prompt-render-skeleton prompt-buffer)
  (prompt-render-focus prompt-buffer)
  (ffi-focus-buffer prompt-buffer)
  (update-prompt-input prompt-buffer)
  (hooks:run-hook (prompt-buffer-ready-hook *browser*) prompt-buffer))

(defmethod hide-prompt-buffer ((prompt-buffer prompt-buffer))
  "Hide PROMPT-BUFFER and display the next active one, if any."
  (with-slots (window) prompt-buffer
    (alex:deletef (active-prompt-buffers window) prompt-buffer)
    ;; The channel values are irrelevant, so is the element order:
    (lparallel.queue:try-pop-queue (prompt-buffer-ready-channel window) :timeout 0)
    (ffi-buffer-delete prompt-buffer)
    (if (active-prompt-buffers window)
        (show-prompt-buffer (first (active-prompt-buffers window)))
        (ffi-window-set-buffer window (active-buffer window) :focus t))))

(defun suggestion-and-mark-count (prompt-buffer suggestions marks &key enable-marks-p)
  "Return a formatted string with counts of marks (if any) and suggestions."
  (alex:maxf (max-suggestions prompt-buffer)
             (length suggestions))
  (unless (hide-suggestion-count-p prompt-buffer)
    (let ((marks-str (when (or marks enable-marks-p)
                       (format nil "~2,' d/" (length marks)))))
      (format nil "[~a~3,' d]" (or marks-str "") (length suggestions)))))

(defmethod render-prompt ((prompt-buffer prompt-buffer))
  (ps-eval :async t :buffer prompt-buffer
    (setf (ps:@ (nyxt/ps:qs document "#prompt-extra") |innerHTML|)
          (ps:lisp
           (suggestion-and-mark-count
            prompt-buffer
            (prompter:all-suggestions prompt-buffer)
            (prompter:all-marks prompt-buffer)
            :enable-marks-p (some #'prompter:enable-marks-p
                                  (prompter:sources prompt-buffer)))))
    (setf (ps:@ (nyxt/ps:qs document "#prompt-modes") |innerHTML|)
          (ps:lisp
           (str:join
            " "
            (mapcar (curry #'mode-status
                           (status-buffer (current-window)))
                    (sort-modes-for-status (enabled-modes prompt-buffer))))))))

(defmethod attribute-widths ((source prompter:source))
  "Return the widths of SOURCE's attribute columns (as ratios)."
  ;; In a proportion a:b, a is the "mean" and b is the "extreme".
  (let* ((means
           (mapcar (lambda (attr) (getf (third attr) ':width))
                   (prompter:active-attributes
                    (first (prompter:suggestions source))
                    :source source)))
         (extreme (ignore-errors (reduce #'+ means))))
    (if extreme
        (mapcar (lambda (ratio) (/ ratio extreme)) means)
        (let ((len (length means)))
          (log:debug "Fallback uniform width distribution: no allocation on ~a."
                     source)
          (make-list len :initial-element (/ 1 len))))))

(defun attribute-width-ratios (attributes)
  "Return column width ratios for ATTRIBUTES, or a uniform split."
  (let* ((widths (mapcar (lambda (attribute) (getf (third attribute) :width))
                         attributes))
         (total (ignore-errors (reduce #'+ widths))))
    (if total
        (mapcar (lambda (width) (/ width total)) widths)
        (let ((len (length attributes)))
          (make-list len :initial-element (/ 1 (max 1 len)))))))

(defun attribute-text (attribute)
  "Return ATTRIBUTE's display text."
  (let ((value (second attribute)))
    (if (stringp value)
        value
        "")))

(defun origin-default-attributes (suggestion origin)
  "Return the columns ORIGIN displays for SUGGESTION.
A source with no column selection contributes every column. These are the
columns shown before column editing."
  (let ((attributes (prompter:attributes suggestion)))
    (if (typep origin 'prompter:source)
        (loop for key in (prompter:active-attributes-keys origin)
              for attribute = (assoc key attributes :test #'string=)
              when attribute
                collect attribute)
        attributes)))

(defun composite-rows (source)
  "Return (INDEX SUGGESTION ATTRIBUTES) for each suggestion in SOURCE.
ATTRIBUTES are the originating source's default columns."
  (loop for suggestion in (prompter:suggestions source)
        for index from 0
        collect (list index
                      suggestion
                      (origin-default-attributes
                       suggestion
                       (prompter:match-data suggestion)))))

(defun pad-attributes (attributes length)
  "Return ATTRIBUTES followed by empty columns until it is LENGTH long."
  (append attributes
          (make-list (max 0 (- length (length attributes)))
                     :initial-element '("" ""))))

(defun render-composite-attributes (source prompt-buffer)
  "Render SOURCE as one table with no column headings.
Every row has as many cells as the suggestion with the most default columns,
so the columns line up. Shorter rows leave the extra cells empty."
  (let* ((rows (composite-rows source))
         (column-count (reduce #'max rows
                               :key (lambda (row) (length (third row)))
                               :initial-value 0))
         (widest (find column-count rows
                       :key (lambda (row) (length (third row)))))
         (widths (attribute-width-ratios (if widest (third widest) '()))))
    (spinneret:with-html
      (when (plusp column-count)
        (:table :class "source-content composite"
                (:colgroup
                 (dolist (width widths)
                   (:col :style (format nil "width: ~,2f%" (* 100 width)))))
                (dolist (row rows)
                  (let ((suggestion-index (first row))
                        (suggestion (second row)))
                    (:tr :id (when (equal (list suggestion source)
                                          (multiple-value-list
                                           (prompter:%current-suggestion
                                            prompt-buffer)))
                               "selection")
                         (loop for attribute in (pad-attributes (third row)
                                                                column-count)
                               for text = (attribute-text attribute)
                               collect
                               (:td :title text
                                    :onclick
                                    (ps:ps
                                      (cond
                                        ((ps:chain window event ctrl-key)
                                         (nyxt/ps:lisp-eval
                                          (:title "mark-this-suggestion"
                                           :buffer prompt-buffer)
                                          (prompter::set-current-suggestion-by-class-and-index
                                           prompt-buffer
                                           (class-name (class-of source))
                                           suggestion-index)
                                          (prompter:toggle-mark prompt-buffer)
                                          (prompt-render-suggestions prompt-buffer)))
                                        (t
                                         (nyxt/ps:lisp-eval
                                          (:title "return-this-suggestion"
                                           :buffer prompt-buffer)
                                          (prompter::set-current-suggestion-by-class-and-index
                                           prompt-buffer
                                           (class-name (class-of source))
                                           suggestion-index)
                                          (prompter:run-action-on-return
                                           (nyxt::current-prompt-buffer))))))
                                    text))))))))))

(defun render-attributes (source prompt-buffer)
  (when (typep source 'prompter:composite-source)
    (return-from render-attributes
      (render-composite-attributes source prompt-buffer)))
  (spinneret:with-html
    (when (prompter:suggestions source)
      (:table :class "source-content"
              (:colgroup
               (when (prompter:enable-marks-p source)
                 (:col :class "mark" :style "width: 28px"))
               (dolist (width (attribute-widths source))
                 (:col :style (format nil "width: ~,2f%" (* 100 width)))))
              (:tr
               :style (if (sera:single (prompter:active-attributes-keys source))
                          "display:none;"
                          "display:revert;")
               (when (prompter:enable-marks-p source) (:th :class "mark" " "))
               (loop for attribute-key in (prompter:active-attributes-keys source)
                     collect (:th (spinneret:escape-string attribute-key))))
              (loop
                ;; TODO: calculate how many lines fit in the prompt buffer
                with max-suggestion-count = 8
                repeat max-suggestion-count
                with cursor-index = (prompter:current-suggestion-position prompt-buffer)
                for suggestion-index from (max 0 (- cursor-index (- (/ max-suggestion-count 2) 1)))
                for suggestion in (nthcdr suggestion-index (prompter:suggestions source))
                collect
                (let ((suggestion-index suggestion-index))
                  (:tr :id (when (equal (list suggestion source)
                                        (multiple-value-list
                                         (prompter:%current-suggestion
                                          prompt-buffer)))
                             "selection")
                       :class (when (prompter:marked-p source (prompter:value suggestion))
                                "marked")
                       (when (prompter:enable-marks-p source)
                         (:td :class "mark"
                          (:input
                           :type "checkbox"
                           :checked (prompter:marked-p
                                     source (prompter:value suggestion))
                           :onchange (ps:ps
                                       (nyxt/ps:lisp-eval
                                        (:title "unmark-this-suggestion"
                                         :buffer prompt-buffer)
                                        (prompter::set-current-suggestion-by-class-and-index
                                         prompt-buffer
                                         (class-name (class-of source))
                                         suggestion-index)
                                        (prompter:toggle-mark prompt-buffer)
                                        (prompt-render-suggestions prompt-buffer))))))
                       (loop for (nil attribute)
                               in (prompter:active-attributes
                                   suggestion :source source)
                             collect
                             (:td
                              :title attribute
                              :onclick
                              (ps:ps
                                (cond
                                  ((ps:chain window event ctrl-key)
                                   (nyxt/ps:lisp-eval
                                    (:title "mark-this-suggestion"
                                     :buffer prompt-buffer)
                                    (prompter::set-current-suggestion-by-class-and-index
                                     prompt-buffer
                                     (class-name (class-of source))
                                     suggestion-index)
                                    (prompter:toggle-mark prompt-buffer)
                                    (prompt-render-suggestions prompt-buffer)))
                                  (t
                                   (nyxt/ps:lisp-eval
                                    (:title "return-this-suggestion"
                                     :buffer prompt-buffer)
                                    (prompter::set-current-suggestion-by-class-and-index
                                     prompt-buffer
                                     (class-name (class-of source))
                                     suggestion-index)
                                    (prompter:run-action-on-return
                                     (nyxt::current-prompt-buffer))))))
                              attribute)))))))))

(export 'prompt-render-suggestions)
(defmethod prompt-render-suggestions ((prompt-buffer prompt-buffer))
  "Refresh the rendering of the suggestion list in PROMPT-BUFFER."
  (let* ((sources (prompter:sources prompt-buffer))
         (current-source-index (position (current-source prompt-buffer) sources))
         (last-source-index (1- (length sources))))
    (flet ((source->html (source)
             (spinneret:with-html-string
               (:div.source
                (:div.source-name
                 (:div
                  (:nbutton
                    :id "next-source"
                    :text (:raw (gethash "down.svg" *static-data*))
                    :title (format nil "Next source (~a)"
                                   (binding-keys (sym:resolve-symbol
                                                  :next-source :command)
                                                 :modes (enabled-modes
                                                         prompt-buffer)))
                    :buffer prompt-buffer
                    '(funcall (sym:resolve-symbol :next-source :command)))
                  (:nbutton
                    :id "previous-source"
                    :text (:raw (gethash "up.svg" *static-data*))
                    :title (format nil "Previous source (~a)"
                                   (binding-keys (sym:resolve-symbol
                                                  :previous-source :command)
                                                 :modes (enabled-modes
                                                         prompt-buffer)))
                    :buffer prompt-buffer
                    '(funcall (sym:resolve-symbol :previous-source :command)))
                  (prompter:name source)
                  (:span
                   :class "suggestion-and-mark-count"
                   ;; To hide the suggestion count for the source, subclass
                   ;; `prompter:source' and handle the condition.  Note that
                   ;; `suggestion-and-mark-count' relies on the global prompt
                   ;; value `hide-suggestion-count-p'.
                   (suggestion-and-mark-count
                    prompt-buffer
                    (prompter:suggestions source)
                    (prompter:marks source)
                    :enable-marks-p (prompter:enable-marks-p source)))
                  (when (not (prompter:ready-p source)) "(In progress...)"))
                 (unless (typep source 'prompter:composite-source)
                   (:div
                    (:nbutton
                      :id "toggle-attributes"
                      :text (:raw "<span class=\"attribute-delta\">Δ</span>")
                      :title (format nil "Toggle attributes display (~a)"
                                     (binding-keys
                                      (sym:resolve-symbol
                                       'toggle-attributes-display :command)
                                      :modes (enabled-modes prompt-buffer)))
                      :buffer prompt-buffer
                      `(funcall (sym:resolve-symbol
                                 :toggle-attributes-display :command)
                                :source ,source)))))
                (render-attributes source prompt-buffer)))))
      (ps-eval :async t :buffer prompt-buffer
        (setf (ps:@ (nyxt/ps:qs document "#suggestions") |innerHTML|)
              (ps:lisp
               (sera:string-join
                (loop for i from current-source-index to last-source-index
                      for source = (nth i sources)
                      unless (null (prompter:suggestions source))
                        collect (source->html source))
                +newline+)))))
    (render-prompt prompt-buffer)))

(defun prompt-render-skeleton (prompt-buffer)
  ;; Synchronous. `html-write' returns before CEF applies the document, and
  ;; `update-prompt-input' then reads `#input' while it is still missing.
  (ps-eval :buffer prompt-buffer
    (setf (ps:chain document (get-elements-by-tag-name "html") 0 |innerHTML|)
          (ps:lisp
           (spinneret:with-html-string
             (:head (:nstyle (style prompt-buffer)))
             (:body
              (:div
               :id "root"
               (:div
                :id "prompt-area"
                (:div :id "prompt" (prompter:prompt prompt-buffer))
                (:div :id "prompt-extra" :class "arrow-right" "[?/?]")
                (:div :id "prompt-input"
                      (:input :type (if (invisible-input-p prompt-buffer)
                                        "password"
                                        "text")
                              :id "input"
                              :value (prompter:input prompt-buffer)))
                (:div :id "prompt-modes" :class "arrow-left" "")
                (:div :id "close-button" :class "arrow-left"
                      (:nbutton
                        :text "×"
                        :title "Close prompt"
                        :buffer prompt-buffer
                        '(funcall (sym:resolve-symbol
                                   :quit-prompt-buffer :command)))))
               (:div :id "suggestions"
                     :style (if (invisible-input-p prompt-buffer)
                                "visibility:hidden;"
                                "visibility:visible;")))))))))

(defun prompt-render-focus (prompt-buffer)
  (ps-eval :async t :buffer prompt-buffer
    (let ((input (ps:chain (nyxt/ps:qs document "#input"))))
      (ps:chain input (focus))
      (ps:chain input (select)))))

(defun update-prompt-input (prompt-buffer &optional input)
  "This blocks and updates the view.
INPUT is an implementation detail, don't rely on it.
If you want to set the input, see `set-prompt-buffer-input'."
  ;; TODO: This function is not thread-safe, add a lock?
  (let ((input (or input
                   (ps-eval :buffer prompt-buffer
                     (ps:chain (nyxt/ps:qs document "#input") value)))))
    (setf (prompter:input prompt-buffer) input)
    ;; TODO: Stop loop when prompt-buffer is no longer current.
    (labels ((maybe-update-view ()
               (let ((next-source (when (find prompt-buffer
                                              (active-prompt-buffers
                                               (window prompt-buffer)))
                                    (prompter:next-ready-p prompt-buffer))))
                 (cond
                   ;; Nothing to do:
                   ((eq t next-source)
                    ;; The renderer might have taken been too long to render the prompt
                    ;; buffer and its HTML input, causing the latter to not be in sync with
                    ;; what was send as input to the prompter sources.  Thus when we are done
                    ;; watching, check if we are in sync; if not, try again.
                    (let ((input (ps-eval :buffer prompt-buffer
                                   (ps:chain (nyxt/ps:qs document "#input") value))))
                      (unless (string= input (prompter:input prompt-buffer))
                        (update-prompt-input prompt-buffer input)))
                    t)
                   ((null next-source) nil)
                   (t ;; At least one source got updated.
                    (prompt-render-suggestions prompt-buffer)
                    (maybe-update-view))))))
      (maybe-update-view))))

(export-always 'set-prompt-buffer-input)
(defun set-prompt-buffer-input
    (input &optional (prompt-buffer (current-prompt-buffer)))
  "Set HTML INPUT in PROMPT-BUFFER.
See `update-prompt-input' to update the changes visually."
  ;; Synchronous so the following read-back sees this value. An async write
  ;; still queued when `update-prompt-input' compares the DOM restarts the
  ;; source update from the previous text.
  (ps-eval :buffer prompt-buffer
    (setf (ps:@ (nyxt/ps:qs document "#input") value)
          (ps:lisp input)))
  (update-prompt-input prompt-buffer input))

(defun wait-on-prompt-buffer (prompt-buffer) ; TODO: Export?  Better name?
  "Block and return PROMPT-BUFFER results."
  (when (prompt-buffer-p prompt-buffer)
    (show-prompt-buffer prompt-buffer)
    ;; A return writes the result before the interrupt. Take the result when
    ;; both are waiting so a completed prompt is not reported as a cancel.
    (loop
      (multiple-value-bind (results received-p)
          (lparallel.queue:try-pop-queue (prompter:result-channel prompt-buffer)
                                         :timeout 0)
        (when received-p
          (hide-prompt-buffer prompt-buffer)
          (return results)))
      (multiple-value-bind (interrupted received-p)
          (lparallel.queue:try-pop-queue (prompter:interrupt-channel prompt-buffer)
                                         :timeout 0.05)
        (declare (ignore interrupted))
        (when received-p
          (multiple-value-bind (results result-received-p)
              (lparallel.queue:try-pop-queue (prompter:result-channel prompt-buffer)
                                             :timeout 0)
            (hide-prompt-buffer prompt-buffer)
            (if result-received-p
                (return results)
                (error 'prompt-buffer-canceled))))))))

(eval-always
  (defvar %prompt-args
    (delete-duplicates (append
                        (mopu:direct-slot-names 'prompt-buffer)
                        (mopu:direct-slot-names 'prompter:prompter)
                        ;; `customize-instance' `:after' arguments:
                        '(extra-modes)))))
(export-always 'prompt)
(eval-always
  (defun prompt #.(append '(&rest args) `(&key ,@%prompt-args))
    "Open the prompt buffer, ready for user input.
PROMPTER and PROMPT-BUFFER are plists of keyword arguments passed to the
prompt-buffer constructor.

Example use:

\(prompt :prompt \"Test prompt\"
         :sources (make-instance 'prompter:source :name \"Test\"
                                                  :constructor '(\"foo\" \"bar\")))

See the documentation of `prompt-buffer' to know more about the options."
    (declare #.(cons 'ignorable %prompt-args))
    (when-let ((prompt-text (getf args :prompt)))
      (when (str:ends-with-p ":" prompt-text)
        (log:warn "Prompt text ~s should not end with a ':'." prompt-text)
        (setf (getf args :prompt) (string-right-trim
                                   (uiop:strcat ":" serapeum:whitespace)
                                   prompt-text))))
    (let ((prompt-object-channel (make-channel 1)))
      (ffi-within-renderer-thread
       (lambda ()
         (let ((prompt-buffer
                 (apply #'make-instance
                        'prompt-buffer
                        (append args (list :window (current-window))))))
           (lparallel.queue:push-queue prompt-buffer prompt-object-channel))))
      (let ((new-prompt (lparallel.queue:pop-queue prompt-object-channel)))
        (wait-on-prompt-buffer new-prompt)))))

(export-always 'prompt1)
(eval-always
  (defun prompt1 #.(append '(&rest args) `(&key ,@%prompt-args))
    "Return the first result of a prompt."
    (declare #.(cons 'ignorable %prompt-args))
    (first (apply #'prompt args))))

(defmethod prompter:object-attributes
    ((prompt-buffer prompt-buffer) (source prompter:source))
  (declare (ignore source))
  `(("Prompt" ,(prompter:prompt prompt-buffer))
    ("Input" ,(prompter:input prompt-buffer))))
