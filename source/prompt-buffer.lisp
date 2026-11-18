;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(eval-always
  (define-class prompt-buffer
      (network-buffer input-buffer modable-buffer auxiliary-buffer prompter:prompter)
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
     (input-update-lock
      (bt:make-lock "prompt-buffer input")
      :type bt:lock
      :export nil
      :initarg nil
      :documentation "Serializes changes to the prompt input.
Only one caller waits on `prompter:next-ready-p'.")
     (input-watcher-p
      nil
      :type boolean
      :export nil
      :initarg nil
      :documentation "Whether a thread is waiting on `prompter:next-ready-p'.")
     (input-finished-p
      nil
      :type boolean
      :export nil
      :initarg nil
      :documentation "Whether `prompter:destroy' has run.
Input updates after this are ignored.")
     (suggestions-view
      nil
      :type (or null user-interface:dynamic-element)
      :export nil
      :initarg nil
      :documentation "The sources drawn under the input.
A `user-interface:dynamic-element'. A source already drawn is kept
when what it shows has not changed.")
     (prompt-count-view
      nil
      :type (or null user-interface:element)
      :export nil
      :initarg nil
      :documentation "The bracketed count beside the prompt.
A `user-interface:element'.")
     (prompt-modes-view
      nil
      :type (or null user-interface:element)
      :export nil
      :initarg nil
      :documentation "The mode names beside the input.
A `user-interface:element'.")
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
        '(".count-total"
          :display "inline-flex"
          :align-items "center"
          :justify-content "center"
          :box-sizing "border-box"
          :width "3ch"
          :min-width "3ch"
          :max-width "3ch"
          :vertical-align "middle")
        '(".loading"
          :display "inline-flex"
          :align-items "center"
          :justify-content "center"
          :box-sizing "border-box"
          :width "100%"
          :min-width "0"
          :max-width "100%"
          :gap "3px")
        '(".loading span"
          :width "4px"
          :height "4px"
          :border-radius "50%"
          :background-color "currentColor"
          :opacity "0.2"
          :animation "prompt-loading 1.2s ease-in-out infinite")
        '(".loading span:nth-child(2)"
          :animation-delay "0.2s")
        '(".loading span:nth-child(3)"
          :animation-delay "0.4s")
        '(:keyframes "prompt-loading"
          ("0%, 80%, 100%" :opacity "0.2")
          ("40%" :opacity "1"))
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
        `("html.prompt-unfocused #input"
          :background-color ,(format nil "color-mix(in srgb, ~a 55%, ~a)"
                                     theme:secondary-color+
                                     theme:background-color))
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

(defmethod include-in-buffer-list-p ((buffer prompt-buffer))
  "A prompt buffer is not one of the window's tabs."
  nil)

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
    (unless (find prompt-buffer (active-prompt-buffers window))
      (push prompt-buffer (active-prompt-buffers window)))
    (lparallel.queue:push-queue prompt-buffer (prompt-buffer-ready-channel window)))
  ;; Height attachment can replace the document, so draw the skeleton after
  ;; the view is in the window. On CEF a write before that has no HTML element.
  (setf (height prompt-buffer) (slot-value prompt-buffer 'height))
  (prompt-render-skeleton prompt-buffer)
  (prompt-render-focus prompt-buffer)
  (ffi-focus-buffer prompt-buffer)
  (update-prompt-input prompt-buffer)
  (prompt-render-suggestions prompt-buffer)
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

(-> suggestion-and-mark-count
    (prompt-buffer list list &key (:enable-marks-p boolean) (:loading-p boolean))
    string)
(defun suggestion-and-mark-count
    (prompt-buffer suggestions marks &key enable-marks-p loading-p)
  "Return the bracketed mark and suggestion count.
When LOADING-P is non-nil, the suggestion total is the loading mark."
  (alex:maxf (max-suggestions prompt-buffer)
             (length suggestions))
  (if (hide-suggestion-count-p prompt-buffer)
      ""
      (format nil "[~a<span class=\"count-total\">~a</span>]"
              (if (or marks enable-marks-p)
                  (format nil "<span class=\"count-marks\">~2,' d/</span>"
                          (length marks))
                  "")
              (if loading-p
                  "<span class=\"loading\" aria-hidden=\"true\"><span></span><span></span><span></span></span>"
                  (format nil "~3,' d" (length suggestions))))))

(-> prompt-count-html (prompt-buffer) string)
(defun prompt-count-html (prompt-buffer)
  "Return PROMPT-BUFFER's bracketed count.
The suggestion total is the loading mark while a source is still computing."
  (suggestion-and-mark-count
   prompt-buffer
   (prompter:all-suggestions prompt-buffer)
   (prompter:all-marks prompt-buffer)
   :enable-marks-p (some #'prompter:enable-marks-p
                         (prompter:sources prompt-buffer))
   :loading-p (notevery #'prompter:ready-p
                        (prompter:sources prompt-buffer))))

(-> prompt-current-p (prompt-buffer) boolean)
(defun prompt-current-p (prompt-buffer)
  "Return non-nil when PROMPT-BUFFER is the top of its window's prompt stack.
The window has one prompt view. Only this prompt draws into it."
  (and (window prompt-buffer)
       (eq prompt-buffer
           (first (active-prompt-buffers (window prompt-buffer))))))

(-> prompt-modes-text (prompt-buffer) string)
(defun prompt-modes-text (prompt-buffer)
  "Return the mode names PROMPT-BUFFER shows beside its input."
  (let ((window (or (window prompt-buffer) (current-window))))
    (if (and window (status-buffer window))
        (str:join " "
                  (mapcar (curry #'mode-status (status-buffer window))
                          (sort-modes-for-status (enabled-modes prompt-buffer))))
        "")))

(-> prompt-render-loading (prompt-buffer) boolean)
(defun prompt-render-loading (prompt-buffer)
  "Show the loading mark in the suggestion count while a source is computing."
  (let ((loading-p (notevery #'prompter:ready-p (prompter:sources prompt-buffer))))
    (when (prompt-current-p prompt-buffer)
      (ensure-prompt-views prompt-buffer)
      (setf (user-interface:raw (prompt-count-view prompt-buffer))
            (prompt-count-html prompt-buffer)))
    loading-p))

(defmethod render-prompt ((prompt-buffer prompt-buffer))
  (when (prompt-current-p prompt-buffer)
    (ensure-prompt-views prompt-buffer)
    (user-interface:batch ((prompt-count-view prompt-buffer))
      (setf (user-interface:raw (prompt-count-view prompt-buffer))
            (prompt-count-html prompt-buffer)
            (user-interface:text (prompt-modes-view prompt-buffer))
            (prompt-modes-text prompt-buffer)))))

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

(-> prompt-copy (t) string)
(defun prompt-copy (text)
  "Return TEXT as a string the caller can keep."
  (if (stringp text)
      (copy-seq text)
      ""))

(-> prompt-suggestion-window () integer)
(defun prompt-suggestion-window ()
  "Return how many rows a regular source shows."
  8)

(-> prompt-source-name (prompter:source) string)
(defun prompt-source-name (source)
  "Return SOURCE's name as text."
  (let ((name (prompter:name source)))
    (if (stringp name)
        name
        "")))

(-> prompt-suggestion-selected-p (prompt-buffer prompter:source t) boolean)
(defun prompt-suggestion-selected-p (prompt-buffer source suggestion)
  "Return non-nil when SUGGESTION is PROMPT-BUFFER's current one in SOURCE."
  (equal (list suggestion source)
         (multiple-value-list (prompter:%current-suggestion prompt-buffer))))

(-> prompt-row-attributes (prompter:source t) list)
(defun prompt-row-attributes (source suggestion)
  "Return the columns SOURCE shows for SUGGESTION.
A composite row uses the originating source's default columns. A missing
column is an empty one, so every row of a regular source has one cell per
active key. Reading these does not start asynchronous attribute work."
  (if (typep source 'prompter:composite-source)
      (origin-default-attributes suggestion (prompter:match-data suggestion))
      (loop for key in (prompter:active-attributes-keys source)
            collect (or (assoc key (prompter:attributes suggestion) :test #'string=)
                        (list key "")))))

(-> prompt-visible-rows (prompt-buffer prompter:source) list)
(defun prompt-visible-rows (prompt-buffer source)
  "Return (INDEX SUGGESTION ATTRIBUTES) for each row SOURCE shows.
A composite source shows every suggestion. A regular source shows eight
rows, starting three before the cursor once the cursor is that far down.
The cursor is the prompt's current position, including when SOURCE is not
the current source."
  (if (typep source 'prompter:composite-source)
      (loop for suggestion in (prompter:suggestions source)
            for index from 0
            collect (list index
                          suggestion
                          (prompt-row-attributes source suggestion)))
      (let* ((window (prompt-suggestion-window))
             (start (max 0
                         (- (or (prompter:current-suggestion-position prompt-buffer)
                                0)
                            (1- (floor window 2))))))
        (loop for suggestion in (nthcdr start (prompter:suggestions source))
              for index from start
              repeat window
              collect (list index
                            suggestion
                            (prompt-row-attributes source suggestion))))))

(-> prompt-visible-sources (prompt-buffer) list)
(defun prompt-visible-sources (prompt-buffer)
  "Return the sources PROMPT-BUFFER shows, from the current one downward.
A source with no suggestions is omitted once it is ready."
  (let ((current (current-source prompt-buffer))
        (sources (prompter:sources prompt-buffer)))
    (if (and current (member current sources))
        (loop for source in (member current sources)
              when (or (prompter:suggestions source)
                       (not (prompter:ready-p source)))
                collect source)
        '())))

(-> prompt-column-widths (prompter:source list) list)
(defun prompt-column-widths (source rows)
  "Return a width ratio for each column SOURCE shows."
  (attribute-width-ratios
   (cond
     ((typep source 'prompter:composite-source)
      (let* ((count (reduce #'max rows
                            :key (lambda (row) (length (third row)))
                            :initial-value 0))
             (widest (find count rows
                           :key (lambda (row) (length (third row))))))
        (if widest
            (third widest)
            '())))
     ((prompter:suggestions source)
      (prompt-row-attributes source (first (prompter:suggestions source))))
     (t '()))))

(-> prompt-row-stamp (prompt-buffer prompter:source list) list)
(defun prompt-row-stamp (prompt-buffer source row)
  "Return the displayed state of ROW, a fresh list."
  (destructuring-bind (index suggestion attributes) row
    (list index
          (mapcar #'prompt-copy (mapcar #'attribute-text attributes))
          (and (prompt-suggestion-selected-p prompt-buffer source suggestion) t)
          (and (not (typep source 'prompter:composite-source))
               (prompter:marked-p source (prompter:value suggestion))
               t))))

(-> prompt-source-stamp (prompt-buffer prompter:source) list)
(defun prompt-source-stamp (prompt-buffer source)
  "Return the value that changes when SOURCE should be drawn again.
The list is fresh, so later changes to a suggestion do not rewrite it."
  (let ((rows (prompt-visible-rows prompt-buffer source)))
    (list (prompt-copy (prompt-source-name source))
          (and (prompter:ready-p source) t)
          (and (prompter:enable-marks-p source) t)
          (mapcar #'prompt-copy (prompter:active-attributes-keys source))
          (length (prompter:suggestions source))
          (length (prompter:marks source))
          (prompt-column-widths source rows)
          (mapcar (lambda (row)
                    (prompt-row-stamp prompt-buffer source row))
                  rows))))

(-> prompt-row-scripts (prompt-buffer prompter:source integer)
    (values string string string &optional))
(defun prompt-row-scripts (prompt-buffer source index)
  "Return the click, control-click, and mark scripts for SOURCE at INDEX."
  (let ((class-name (class-name (class-of source))))
    (values
     (ps:ps
       (nyxt/ps:lisp-eval
        (:title "return-this-suggestion"
         :buffer prompt-buffer)
        (prompter::set-current-suggestion-by-class-and-index
         prompt-buffer class-name index)
        (prompter:run-action-on-return prompt-buffer)))
     (ps:ps
       (nyxt/ps:lisp-eval
        (:title "mark-this-suggestion"
         :buffer prompt-buffer)
        (prompter::set-current-suggestion-by-class-and-index
         prompt-buffer class-name index)
        (prompter:toggle-mark prompt-buffer)
        (prompt-render-suggestions prompt-buffer)))
     (ps:ps
       (nyxt/ps:lisp-eval
        (:title "unmark-this-suggestion"
         :buffer prompt-buffer)
        (prompter::set-current-suggestion-by-class-and-index
         prompt-buffer class-name index)
        (prompter:toggle-mark prompt-buffer)
        (prompt-render-suggestions prompt-buffer))))))

(-> prompt-table-row (prompt-buffer prompter:source integer t list) t)
(defun prompt-table-row (prompt-buffer source index suggestion attributes)
  "Return the table row for SUGGESTION at INDEX in SOURCE."
  (multiple-value-bind (action control-action mark-action)
      (prompt-row-scripts prompt-buffer source index)
    (user-interface:make-table-row
     (mapcar #'attribute-text attributes)
     :selected-p (prompt-suggestion-selected-p prompt-buffer source suggestion)
     :marked-p (and (not (typep source 'prompter:composite-source))
                    (prompter:marked-p source (prompter:value suggestion))
                    t)
     :action action
     :control-action control-action
     :mark-action mark-action)))

(-> prompt-schedule-attributes (prompter:source list) t)
(defun prompt-schedule-attributes (source rows)
  "Start SOURCE's asynchronous attribute work for the suggestions in ROWS.
A composite source has no such work. A suggestion whose attributes are
already strings is left alone."
  (unless (typep source 'prompter:composite-source)
    (dolist (row rows)
      (prompter:active-attributes (second row) :source source)))
  nil)

(-> prompt-table-columns (prompter:source list) list)
(defun prompt-table-columns (source rows)
  "Return the columns of the table for SOURCE."
  (let ((widths (prompt-column-widths source rows)))
    (mapcar (lambda (text width)
              (make-instance 'user-interface:column
                             :text (if (stringp text) text "")
                             :width width))
            (if (typep source 'prompter:composite-source)
                (make-list (length widths) :initial-element "")
                (prompter:active-attributes-keys source))
            widths)))

(-> prompt-source-table (prompt-buffer prompter:source list) t)
(defun prompt-source-table (prompt-buffer source rows)
  "Return the table of ROWS for SOURCE."
  (prompt-schedule-attributes source rows)
  (make-instance
   'user-interface:table
   :html-class (if (typep source 'prompter:composite-source)
                   "source-content composite"
                   "source-content")
   :marks-p (and (prompter:enable-marks-p source)
                 (not (typep source 'prompter:composite-source))
                 t)
   :header-p (not (typep source 'prompter:composite-source))
   :header-visible-p (not (sera:single (prompter:active-attributes-keys source)))
   :columns (prompt-table-columns source rows)
   :rows (mapcar (lambda (row)
                   (destructuring-bind (index suggestion attributes) row
                     (prompt-table-row prompt-buffer source index
                                       suggestion attributes)))
                 rows)))

(-> prompt-command-label (prompt-buffer t string) string)
(defun prompt-command-label (prompt-buffer command description)
  "Return DESCRIPTION followed by COMMAND's binding in PROMPT-BUFFER."
  (format nil "~a (~a)"
          description
          (binding-keys (sym:resolve-symbol command :command)
                        :modes (enabled-modes prompt-buffer))))

(-> prompt-command-button (prompt-buffer &key (:id string) (:label string)
                                         (:raw t) (:command t))
    t)
(defun prompt-command-button (prompt-buffer &key id label raw command)
  "Return a button that runs COMMAND in PROMPT-BUFFER.
LABEL is the tooltip. RAW is the button body, such as an icon."
  (make-instance 'user-interface:button
                 :id id
                 :alt-text label
                 :raw raw
                 :action (ps:ps
                           (nyxt/ps:lisp-eval
                            (:title id
                             :buffer prompt-buffer)
                            (funcall (sym:resolve-symbol command :command))))))

(-> prompt-attributes-button (prompt-buffer prompter:source) t)
(defun prompt-attributes-button (prompt-buffer source)
  "Return the button that edits which columns SOURCE shows."
  (make-instance
   'user-interface:button
   :id "toggle-attributes"
   :alt-text (format nil "Toggle attributes display (~a)"
                     (binding-keys
                      (sym:resolve-symbol 'toggle-attributes-display :command)
                      :modes (enabled-modes prompt-buffer)))
   :raw "<span class=\"attribute-delta\">Δ</span>"
   :action (ps:ps
             (nyxt/ps:lisp-eval
              (:title "toggle-attributes"
               :buffer prompt-buffer)
              (funcall (sym:resolve-symbol :toggle-attributes-display :command)
                       :source source)))))

(-> prompt-source-count (prompt-buffer prompter:source) t)
(defun prompt-source-count (prompt-buffer source)
  "Return the mark and suggestion count shown for SOURCE."
  (make-instance 'user-interface:element
                 :tag :span
                 :html-class "suggestion-and-mark-count"
                 :raw (suggestion-and-mark-count
                       prompt-buffer
                       (prompter:suggestions source)
                       (prompter:marks source)
                       :enable-marks-p (and (prompter:enable-marks-p source) t)
                       :loading-p (not (prompter:ready-p source)))))

(-> prompt-source-header (prompt-buffer prompter:source) t)
(defun prompt-source-header (prompt-buffer source)
  "Return SOURCE's heading: its navigation, its name, and its count."
  (let ((main (make-instance
               'user-interface:element
               :tag :div
               :children (list (prompt-command-button
                                prompt-buffer
                                :id "next-source"
                                :label (prompt-command-label
                                        prompt-buffer :next-source "Next source")
                                :raw (gethash "down.svg" *static-data*)
                                :command :next-source)
                               (prompt-command-button
                                prompt-buffer
                                :id "previous-source"
                                :label (prompt-command-label
                                        prompt-buffer :previous-source
                                        "Previous source")
                                :raw (gethash "up.svg" *static-data*)
                                :command :previous-source)
                               (prompt-source-name source)
                               (prompt-source-count prompt-buffer source)))))
    (make-instance
     'user-interface:element
     :tag :div
     :html-class "source-name"
     :children (if (typep source 'prompter:composite-source)
                   (list main)
                   (list main
                         (make-instance
                          'user-interface:element
                          :tag :div
                          :children (list (prompt-attributes-button
                                           prompt-buffer source))))))))

(-> prompt-source-view (prompt-buffer prompter:source) t)
(defun prompt-source-view (prompt-buffer source)
  "Return the element for SOURCE: its heading and, when it has rows, its table."
  (let ((rows (prompt-visible-rows prompt-buffer source)))
    (make-instance
     'user-interface:element
     :tag :div
     :html-class "source"
     :children (if rows
                   (list (prompt-source-header prompt-buffer source)
                         (prompt-source-table prompt-buffer source rows))
                   (list (prompt-source-header prompt-buffer source))))))

(defun ensure-prompt-views (prompt-buffer)
  "Create PROMPT-BUFFER's count, modes, and suggestion list the first time."
  (unless (suggestions-view prompt-buffer)
    (setf (prompt-count-view prompt-buffer)
          (make-instance 'user-interface:element
                         :id "prompt-count"
                         :tag :span
                         :raw (prompt-count-html prompt-buffer))
          (prompt-modes-view prompt-buffer)
          (make-instance 'user-interface:element
                         :id "prompt-modes"
                         :tag :div
                         :html-class "arrow-left"
                         :text (prompt-modes-text prompt-buffer))
          (suggestions-view prompt-buffer)
          (make-instance 'user-interface:dynamic-element
                         :id "suggestions"
                         :attributes (list :style
                                           (if (invisible-input-p prompt-buffer)
                                               "visibility:hidden;"
                                               "visibility:visible;"))
                         :template (lambda (source)
                                     (prompt-source-view prompt-buffer source))
                         :item-key (lambda (source)
                                     (prompt-source-stamp prompt-buffer source))))
    (user-interface:connect (prompt-count-view prompt-buffer) prompt-buffer)
    (user-interface:connect (prompt-modes-view prompt-buffer) prompt-buffer)
    (user-interface:connect (suggestions-view prompt-buffer) prompt-buffer)))

(export 'prompt-render-suggestions)
(defmethod prompt-render-suggestions :around ((prompt-buffer prompt-buffer))
  (when (prompt-current-p prompt-buffer)
    (call-next-method)))

(defmethod prompt-render-suggestions ((prompt-buffer prompt-buffer))
  "Refresh the sources shown in PROMPT-BUFFER.
A source view already built is kept when what it shows has not changed."
  (ensure-prompt-views prompt-buffer)
  (let ((view (suggestions-view prompt-buffer)))
    (user-interface:batch (view)
      (setf (user-interface:items view)
            (prompt-visible-sources prompt-buffer))))
  (render-prompt prompt-buffer))

(-> prompt-close-button (prompt-buffer) t)
(defun prompt-close-button (prompt-buffer)
  "Return the button that closes PROMPT-BUFFER."
  (make-instance 'user-interface:button
                 :text "×"
                 :alt-text "Close prompt"
                 :action (ps:ps
                           (nyxt/ps:lisp-eval
                            (:title "quit-prompt-buffer"
                             :buffer prompt-buffer)
                            (funcall (sym:resolve-symbol
                                      :quit-prompt-buffer :command))))))

(-> prompt-document (prompt-buffer) string)
(defun prompt-document (prompt-buffer)
  "Return PROMPT-BUFFER's document."
  (spinneret:with-html-string
    (:head (:nstyle (style prompt-buffer)))
    (:body
     (:div
      :id "root"
      (:div
       :id "prompt-area"
       (:div :id "prompt" (prompter:prompt prompt-buffer))
       (:div :id "prompt-extra" :class "arrow-right"
             (user-interface:to-html (prompt-count-view prompt-buffer)))
       (:div :id "prompt-input"
             (:input :type (if (invisible-input-p prompt-buffer)
                               "password"
                               "text")
                     :id "input"
                     :value (prompter:input prompt-buffer)))
       (user-interface:to-html (prompt-modes-view prompt-buffer))
       (:div :id "close-button" :class "arrow-left"
             (user-interface:to-html (prompt-close-button prompt-buffer))))
      (user-interface:to-html (suggestions-view prompt-buffer))))))

(defun prompt-render-skeleton (prompt-buffer)
  ;; Synchronous. `html-write' returns before CEF applies the document, and
  ;; `update-prompt-input' then reads `#input' while it is still missing.
  (when (prompt-current-p prompt-buffer)
    (ensure-prompt-views prompt-buffer)
    (ps-eval :buffer prompt-buffer
      (setf (ps:chain document (get-elements-by-tag-name "html") 0 |innerHTML|)
            (ps:lisp (prompt-document prompt-buffer))))))

(defun prompt-render-focus (prompt-buffer)
  (when (prompt-current-p prompt-buffer)
    (ps-eval :async t :buffer prompt-buffer
    (let ((input (ps:chain (nyxt/ps:qs document "#input"))))
      (ps:chain input (focus))
      (ps:chain input (select)))
    (unless (ps:@ window nyxt-prompt-focus-watch)
      (setf (ps:@ window nyxt-prompt-focus-watch) t)
      (ps:chain window
                (add-event-listener
                 "blur"
                 (lambda ()
                   (ps:chain document document-element class-list
                             (add "prompt-unfocused")))))
      (ps:chain window
                (add-event-listener
                 "focus"
                 (lambda ()
                   (ps:chain document document-element class-list
                             (remove "prompt-unfocused")))))))))

(-> prompt-buffer-focused-p (prompt-buffer) boolean)
(defun prompt-buffer-focused-p (prompt-buffer)
  "Return non-nil when PROMPT-BUFFER's document has keyboard focus."
  (when (prompt-current-p prompt-buffer)
    (ps-eval :buffer prompt-buffer
      (ps:chain document (has-focus)))))

(export-always 'indicate-prompt-buffer-focus)
(-> indicate-prompt-buffer-focus (prompt-buffer boolean) t)
(defun indicate-prompt-buffer-focus (prompt-buffer focused-p)
  "Show that PROMPT-BUFFER has keyboard focus when FOCUSED-P is non-nil.
The caret and the input's fill are the marks."
  (when (prompt-current-p prompt-buffer)
    (if focused-p
        (ps-eval :buffer prompt-buffer
        (setf (ps:@ document document-element style opacity) "")
        (ps:chain document document-element class-list (remove "prompt-unfocused"))
        (ps:let ((input (nyxt/ps:qs document "#input")))
          (when input
            (ps:chain input (focus)))))
      (ps-eval :buffer prompt-buffer
        (setf (ps:@ document document-element style opacity) "")
        (ps:chain document document-element class-list (add "prompt-unfocused"))
        (ps:let ((input (nyxt/ps:qs document "#input")))
          (when input
            (ps:chain input (blur))))))))

(-> prompt-input-stop-p (prompt-buffer) boolean)
(defun prompt-input-stop-p (prompt-buffer)
  "Return non-nil when PROMPT-BUFFER should no longer follow its sources."
  (or (input-finished-p prompt-buffer)
      (prompter:returned-p prompt-buffer)
      (not (and (window prompt-buffer)
                (find prompt-buffer
                      (active-prompt-buffers (window prompt-buffer)))))))

(-> prompt-field-text (prompt-buffer) string)
(defun prompt-field-text (prompt-buffer)
  "Return the text currently shown in PROMPT-BUFFER's input field.
A prompt under another one has no field of its own, so this is its stored input."
  (let ((text (if (prompt-current-p prompt-buffer)
                  (ps-eval :buffer prompt-buffer
                    (ps:chain (nyxt/ps:qs document "#input") value))
                  (prompter:input prompt-buffer))))
    (if (stringp text)
        text
        "")))

(-> prompt-text= (t t) boolean)
(defun prompt-text= (existing updated)
  "Return non-nil when EXISTING and UPDATED are the same prompt text."
  (string= (if (stringp existing) existing "")
           (if (stringp updated) updated "")))

(defmethod prompter:destroy :before ((prompt-buffer prompt-buffer))
  (setf (input-finished-p prompt-buffer) t))

(-> watch-prompt-input (prompt-buffer) boolean)
(defun watch-prompt-input (prompt-buffer)
  "Redraw PROMPT-BUFFER until its sources are ready or the prompt is done.
A new input restarts the wait. `prompter:destroy' ends it."
  (loop
    (when (prompt-input-stop-p prompt-buffer)
      (return nil))
    (let ((waiting-on (prompter:input prompt-buffer)))
      (prompt-render-loading prompt-buffer)
      (let ((next-source (prompter:next-ready-p prompt-buffer)))
        (cond
          ((eq t next-source)
           (let ((text (prompt-field-text prompt-buffer)))
             (bt:with-lock-held ((input-update-lock prompt-buffer))
               (cond
                 ((prompt-input-stop-p prompt-buffer)
                  (return nil))
                 ((not (prompt-text= (prompter:input prompt-buffer) waiting-on))
                  nil)
                 ((not (prompt-text= text (prompter:input prompt-buffer)))
                  (setf (prompter:input prompt-buffer) text))
                 (t
                  (return t))))))
          ((null next-source)
           (when (or (prompt-input-stop-p prompt-buffer)
                     (prompt-text= (prompter:input prompt-buffer) waiting-on))
             (return nil)))
          (t
           (unless (prompt-input-stop-p prompt-buffer)
             (prompt-render-suggestions prompt-buffer))))))))

(defun update-prompt-input (prompt-buffer &optional input)
  "Set PROMPT-BUFFER's input from INPUT and redraw suggestions as sources update.
INPUT defaults to the text currently in the prompt field.
One caller waits until the sources are ready or the prompt is done. Another
caller only changes the input, which restarts that wait, so a key cannot
leave the prompt blocked on `prompter:next-ready-p'."
  (let ((input (or input (prompt-field-text prompt-buffer)))
        (watcher nil))
    (bt:with-lock-held ((input-update-lock prompt-buffer))
      (unless (prompt-input-stop-p prompt-buffer)
        (setf (prompter:input prompt-buffer) input)
        (unless (input-watcher-p prompt-buffer)
          (setf (input-watcher-p prompt-buffer) t
                watcher t))))
    (when watcher
      (unwind-protect
           (watch-prompt-input prompt-buffer)
        (bt:with-lock-held ((input-update-lock prompt-buffer))
          (setf (input-watcher-p prompt-buffer) nil))))))

(export-always 'set-prompt-buffer-input)
(defun set-prompt-buffer-input
    (input &optional (prompt-buffer (current-prompt-buffer)))
  "Set HTML INPUT in PROMPT-BUFFER.
See `update-prompt-input' to update the changes visually."
  ;; Synchronous so the following read-back sees this value. An async write
  ;; still queued when `update-prompt-input' compares the DOM restarts the
  ;; source update from the previous text.
  (when (prompt-current-p prompt-buffer)
    (ps-eval :buffer prompt-buffer
      (setf (ps:@ (nyxt/ps:qs document "#input") value)
            (ps:lisp input))))
  (update-prompt-input prompt-buffer input))

(defun wait-on-prompt-buffer (prompt-buffer) ; TODO: Export?  Better name?
  "Block and return PROMPT-BUFFER results.
Leave the prompt stack when waiting ends, including when the prompt fails."
  (when (prompt-buffer-p prompt-buffer)
    (unwind-protect
         (progn
           (show-prompt-buffer prompt-buffer)
           ;; A return writes the result before the interrupt. Take the result
           ;; when both are waiting so a completed prompt is not reported as a
           ;; cancel.
           (loop
             (multiple-value-bind (results received-p)
                 (lparallel.queue:try-pop-queue (prompter:result-channel prompt-buffer)
                                                :timeout 0)
               (when received-p
                 (return results)))
             (multiple-value-bind (interrupted received-p)
                 (lparallel.queue:try-pop-queue (prompter:interrupt-channel prompt-buffer)
                                                :timeout 0.05)
               (declare (ignore interrupted))
               (when received-p
                 (multiple-value-bind (results result-received-p)
                     (lparallel.queue:try-pop-queue (prompter:result-channel prompt-buffer)
                                                    :timeout 0)
                   (if result-received-p
                       (return results)
                       (error 'prompt-buffer-canceled)))))))
      (when (and (window prompt-buffer)
                 (find prompt-buffer
                       (active-prompt-buffers (window prompt-buffer))))
        (hide-prompt-buffer prompt-buffer)))))

(eval-always
  (defvar %prompt-args
    (remove-duplicates (append
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
