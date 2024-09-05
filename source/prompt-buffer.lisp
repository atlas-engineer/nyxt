;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(sera:eval-always
  (define-class prompt-buffer (network-buffer input-buffer modable-buffer prompter:prompter)
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
- `:fit-to-prompt', which shrinks the height to fit the input area;
- an integer, which corresponds to the height in pixels.")
     (prompter:history
      (prompt-buffer-generic-history *browser*)
      ;; Both set to nil since it overrides the default value.
      :accessor nil
      :export nil
      :documentation "Override `prompter:history' to share input history globally.")
     (invisible-input-p
      nil
      :documentation "Whether to replace input by a placeholder character.  This
is useful to conceal passwords.")
     (hide-suggestion-count-p
      nil
      :documentation "Whether to hide the number of suggestions.
Affects both the prompt and its sources.")
     (max-suggestions
      0
      :export nil
      :documentation "Maximum number of total suggestions that were listed at
some point.")
     (mouse-support-p
      t
      :type boolean
      :documentation "Whether to allow mouse events to act on prompt buffer suggestions.
The following mouse keybindings are available:
- button1: `run-action-on-return'
- C-button1: `toggle-mark-forwards'
- s-button1: `toggle-mark-forwards'
- M-button1: `set-action-on-return'.")
     (style
      (theme:themed-css (theme *browser*)
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "400" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Regular.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "400" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Italic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "100" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Thin.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "100" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ThinItalic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "200" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraLight.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "200" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraLightItalic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "300" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Light.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "300" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-LightItalic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "500" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Medium.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "500" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-MediumItalic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "600" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-SemiBold.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "600" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-SemiBoldItalic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "700" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Bold.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "700" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-BoldItalic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "800" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraBold.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "800" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraBoldItalic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "normal" :font-weight "900" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Black.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "public sans" :font-style "italic" :font-weight "900" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-BlackItalic.woff") "format('woff')")
        #-darwin
        `(:font-face :font-family "dejavu sans mono" :src ,(format nil "url('nyxt-resource:~a')" "DejaVuSansMono.ttf") "format('ttf')")
        `(*
          :font-size "14px"
          :line-height "18px")
        `(body
          :font-family ,theme:font-family
          :border-right "2px solid"
          :border-color ,theme:primary
          :overflow "hidden"
          :margin "0"
          :padding "0")
        `("#prompt-area"
          :background-color ,theme:primary
          :color ,theme:on-primary
          :border-top "2px solid"
          :border-bottom "2px solid"
          :border-color ,theme:primary
          :display "grid"
          :grid-template-columns "auto auto 1fr auto auto"
          :width "100%")
        `("#prompt"
          :background-color ,theme:primary
          :color ,theme:on-primary
          :padding-left "10px"
          :padding-right "8px"
          :line-height "28px")
        `("#prompt-input"
          :margin-right "-10px"
          :line-height "28px")
        `("#prompt-extra"
          :font-family ,theme:monospace-font-family
          :z-index "1"
          :min-width "12px"
          :padding-right 14px !important
          :background-color ,theme:primary
          :color ,theme:on-primary
          :line-height "28px"
          :padding-right "7px")
        `("#prompt-modes"
          :background-color ,theme:secondary
          :padding-left "10px !important"
          :padding-right "14px !important"
          :line-height "28px"
          :padding-left "3px"
          :padding-right "3px")
        `("#close-button"
          :text-align "right"
          :background-color ,theme:primary
          :min-width "24px"
          :line-height "28px"
          :font-weight "bold"
          :font-size "20px")
        `(".arrow-right"
          :clip-path "polygon(0 0, calc(100% - 10px) 0, 100% calc(50% - 1px), 100% 50%, 100% calc(50% + 1px), calc(100% - 10px) 100%, 0 100%)"
          :margin-right "-10px")
        `(".arrow-left"
          :clip-path "polygon(10px 0, 100% 0, 100% 100%, 10px 100%, 0px calc(50% + 1px), 0% 50%, 0px calc(50% - 1px))"
          :margin-left "-10px")
        `(button
          :background "transparent"
          :color "inherit"
          :text-decoration "none"
          :border "none"
          :padding 0
          :font "inherit"
          :outline "inherit")
        `(.button.action
          :background-color ,theme:action
          :color ,theme:on-action)
        `((:and .button :hover)
          :cursor "pointer"
          :color ,theme:action)
        `(".button:hover svg path"
          :stroke ,theme:action-)
        `((:and .button (:or :visited :active))
          :color ,theme:background)
        `(input
          :font-family ,theme:monospace-font-family)
        `("#input"
          :height "28px"
          :margin-top "0"
          :margin-bottom "0"
          :padding-left "16px !important"
          :background-color ,theme:background
          :color ,theme:on-background
          :opacity 0.9
          :border 2px solid ,theme:secondary
          :outline "none"
          :padding "3px"
          :width "100%"
          :autofocus "true")
        `("#input:focus"
          :border-color ,(cl-colors2:print-hex theme:action- :alpha 0.40))
        `(".source"
          :margin-left "10px"
          :margin-top "15px")
        `(".source-name"
          :padding-left "4px"
          :background-color ,theme:secondary
          :color ,theme:on-secondary
          :display "flex"
          :justify-content "space-between"
          :align-items "stretch"
          :border-radius "2px")
        '(".source-name > div"
          :line-height "22px")
        `(".source-name > div > button"
          :padding "5px 5px 5px 0px"
          :min-height "100%")
        `("#next-source > svg, #previous-source > svg"
          :margin-bottom "2px"
          :height "5px")
        '("#previous-source"
          :padding 0)
        '("#next-source"
          :padding 0)
        `("#suggestions"
          :background-color ,theme:background
          :color ,theme:on-background
          :overflow-y "hidden"
          :overflow-x "hidden"
          :height "100%"
          :margin-right "3px")
        `(".suggestion-and-mark-count"
          :font-family ,theme:monospace-font-family)
        `(".source-content"
          :box-sizing "border-box"
          :padding-left "16px"
          :margin-left "2px"
          :width "100%"
          :table-layout "fixed"
          (td
           :color ,theme:on-background
           :border-radius "2px"
           :white-space "nowrap"
           :height "20px"
           :padding-left "4px"
           :overflow "auto")
          ("tr:not(:first-child)"
           :font-family ,theme:monospace-font-family)
          ("tr:hover"
           :background-color ,theme:action-
           :color ,theme:on-action
           :cursor "pointer")
          (th
           :background-color ,theme:primary
           :color ,theme:on-primary
           :font-weight "normal"
           :padding-left "4px"
           :border-radius "2px"
           :text-align "left")
          ("td::-webkit-scrollbar"
           :display "none"))
        `("#selection"
          :background-color ,theme:action
          :color ,theme:on-action)
        `(.marked
          :background-color ,theme:secondary
          :color ,theme:on-secondary
          :font-weight "bold")
        `(.selected
          :background-color ,theme:primary
          :color ,theme:on-primary))
      :documentation "The CSS applied to prompt buffer."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:export-predicate-name-p t)
    (:documentation "The prompt buffer is the interface for user interactions.
Each prompt spawns a new object: this makes it possible to nest prompts, such as
invoking `prompt-buffer:history'.

See `prompt' for how to invoke prompts.")
    (:metaclass user-class)))

(defmethod customize-instance :after ((prompt-buffer prompt-buffer)
                                      &key extra-modes &allow-other-keys)
  (hooks:run-hook (prompt-buffer-make-hook *browser*) prompt-buffer)
  (enable-modes* (append (reverse (default-modes prompt-buffer))
                         (uiop:ensure-list extra-modes))
                 prompt-buffer))

(defmethod (setf height) (value (prompt-buffer prompt-buffer))
  (setf (ffi-height prompt-buffer)
        (case value
          (:default (round (/ (ffi-height (window prompt-buffer)) 3)))
          (:fit-to-prompt
           (ps-eval :buffer prompt-buffer
             (+ (ps:chain (nyxt/ps:qs document "#prompt-area") offset-height)
                ;; Buffer whitespace between the prompt buffer's input area and
                ;; the status buffer.  Not clear how to the derive the value
                ;; from another element's height.
                4)))
          (t value)))
  (setf (slot-value prompt-buffer 'height) value))

(export-always 'current-source)
(defun current-source (&optional (prompt-buffer (current-prompt-buffer)))
  "Current PROMPT-BUFFER `prompter:source'.
If PROMPT-BUFFER is not provided, use `current-prompt-buffer'."
  (prompter:current-source prompt-buffer))

(export-always 'current-suggestion-value)
(defun current-suggestion-value (&optional (prompt-buffer (current-prompt-buffer)))
  "Return selected PROMPT-BUFFER `prompter:suggestion' `prompter:value'.
Return `prompter:source' as second value.
To access the suggestion instead, see `prompter:%current-suggestion'."
  (multiple-value-bind (suggestion source)
      (prompter:%current-suggestion prompt-buffer)
    (values (when suggestion (prompter:value suggestion)) source)))

(defmethod show-prompt-buffer ((prompt-buffer prompt-buffer))
  (with-slots (window) prompt-buffer
    (push prompt-buffer (active-prompt-buffers window))
    (calispel:! (prompt-buffer-channel window) prompt-buffer)
    (prompt-render prompt-buffer)
    (setf (height prompt-buffer) (height prompt-buffer)))
  (run-thread "Show prompt watcher"
    (update-prompt-input prompt-buffer)
    (hooks:run-hook (prompt-buffer-ready-hook *browser*) prompt-buffer)))

(defmethod hide-prompt-buffer ((prompt-buffer prompt-buffer))
  "Hide PROMPT-BUFFER and display the next active one, if any."
  (with-slots (window) prompt-buffer
    (alex:deletef (active-prompt-buffers window) prompt-buffer)
    ;; The channel values are irrelevant, so is the element order:
    (calispel:? (prompt-buffer-channel window) 0)
    (ffi-buffer-delete prompt-buffer)
    (if (active-prompt-buffers window)
        (show-prompt-buffer (first (active-prompt-buffers window)))
        (setf (height prompt-buffer) 0))))

(defun suggestion-and-mark-count (prompt-buffer suggestions marks &key enable-marks-p pad-p)
  (alex:maxf (max-suggestions prompt-buffer)
             (length suggestions))
  (flet ((digits-count (n) (1+ (floor (log (abs n) 10)))))
    (unless (or (not suggestions)
                (hide-suggestion-count-p prompt-buffer))
      (let ((padding (if pad-p
                         (prin1-to-string (digits-count (max-suggestions prompt-buffer)))
                         "0")))
        (format nil
                (str:concat "[~a~" padding ",,,' @a]")
                (if (or marks enable-marks-p)
                    (format nil (str:concat "~" padding ",,,' @a/") (length marks))
                    "")
                (length suggestions))))))

(defun prompt-render-prompt (prompt-buffer)
  (ps-eval :async t :buffer prompt-buffer
    (setf (ps:@ (nyxt/ps:qs document "#prompt-extra") |innerHTML|)
          (ps:lisp
           (suggestion-and-mark-count prompt-buffer
                                      (prompter:all-suggestions prompt-buffer)
                                      (prompter:all-marks prompt-buffer)
                                      :pad-p t
                                      :enable-marks-p (some #'prompter:enable-marks-p
                                                            (prompter:sources prompt-buffer)))))
    (setf (ps:@ (nyxt/ps:qs document "#prompt-modes") |innerHTML|)
          (ps:lisp (str:join " "
                             (mapcar (curry #'mode-status
                                            (status-buffer (current-window)))
                                     (sort-modes-for-status (modes prompt-buffer))))))))

(defmethod attribute-widths ((source prompter:source))
  "Return the widths of SOURCE's attribute columns (as ratios)."
  ;; In a proportion a:b, a is the "mean" and b is the "extreme".
  (let* ((means (mapcar (lambda (attr) (getf (third attr) ':width))
                        (prompter:active-attributes (first (prompter:suggestions source))
                                                    :source source)))
         (extreme (ignore-errors (reduce #'+ means))))
    (if extreme
        (mapcar (lambda (ratio) (/ ratio extreme)) means)
        (let ((len (length means)))
          (log:debug "Fallback on uniform width distribution for lack of allocation on ~a."
                     source)
          (make-list len :initial-element (/ 1 len))))))

(defun render-attributes (source prompt-buffer)
  (spinneret:with-html-string
    (when (prompter:suggestions source)
      (:table :class "source-content"
              (:colgroup
               (when (prompter:enable-marks-p source)
                 (:col :style "width: 25px"))
               (dolist (width (attribute-widths source))
                 (:col :style (format nil "width: ~,2f%" (* 100 width)))))
              (:tr
               :style (if (sera:single (prompter:active-attributes-keys source))
                          "display:none;"
                          "display:revert;")
               (when (prompter:enable-marks-p source) (:th " "))
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
                   (let ((suggestion-index suggestion-index)
                         (cursor-index cursor-index))
                     (:tr :id (when (equal (list suggestion source)
                                           (multiple-value-list (prompter:%current-suggestion prompt-buffer)))
                                "selection")
                          :class (when (prompter:marked-p source (prompter:value suggestion))
                                   "marked")
                          (when (prompter:enable-marks-p source)
                            (:td
                             (:input
                              :type "checkbox"
                              :checked (prompter:marked-p source (prompter:value suggestion))
                              :onchange (ps:ps
                                          (nyxt/ps:lisp-eval
                                           (:title "unmark-this-suggestion"
                                            :buffer prompt-buffer)
                                           (prompter::set-current-suggestion
                                            prompt-buffer
                                            (- suggestion-index cursor-index))
                                           (prompter:toggle-mark prompt-buffer)
                                           (prompter::set-current-suggestion
                                            prompt-buffer
                                            (- cursor-index suggestion-index))
                                           (prompt-render-suggestions prompt-buffer))))))
                          (loop for (nil attribute)
                                  in (prompter:active-attributes suggestion :source source)
                                collect (:td
                                         :title attribute
                                         :onclick (when (and (mouse-support-p prompt-buffer)
                                                             (not (find :darwin *features*)))
                                                    (ps:ps
                                                      (cond
                                                        ((or (ps:chain window event ctrl-key)
                                                             (ps:chain window event shift-key))
                                                         (nyxt/ps:lisp-eval
                                                          (:title "mark-this-suggestion"
                                                           :buffer prompt-buffer)
                                                          (prompter::set-current-suggestion
                                                           prompt-buffer
                                                           (- suggestion-index cursor-index))
                                                          (prompter:toggle-mark prompt-buffer)
                                                          (prompter::set-current-suggestion
                                                           prompt-buffer
                                                           (- cursor-index suggestion-index))
                                                          (prompt-render-suggestions prompt-buffer)))
                                                        ((ps:chain window event alt-key)
                                                         (nyxt/ps:lisp-eval
                                                          (:title "return-this-suggestion-with-another-action"
                                                           :buffer prompt-buffer)
                                                          (prompter::set-current-suggestion
                                                           prompt-buffer
                                                           (- suggestion-index cursor-index))
                                                          (uiop:symbol-call
                                                           :nyxt/prompt-buffer-mode :set-action-on-return
                                                           (nyxt::current-prompt-buffer))))
                                                        (t
                                                         (nyxt/ps:lisp-eval
                                                          (:title "return-this-suggestion"
                                                           :buffer prompt-buffer)
                                                          (prompter::set-current-suggestion
                                                           prompt-buffer
                                                           (- suggestion-index cursor-index))
                                                          (prompter:run-action-on-return
                                                           (nyxt::current-prompt-buffer)))))))
                                         (:raw attribute))))))))))

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
                  #-darwin
                  (:nbutton
                    :id "next-source"
                    :text (:raw (gethash "down.svg" *static-data*))
                    :title (format nil "Next source (~a)"
                                   (binding-keys (sym:resolve-symbol :next-source
                                                                     :command)
                                                 :modes (modes prompt-buffer)))
                    :buffer prompt-buffer
                    '(funcall (sym:resolve-symbol :next-source :command)))
                  #-darwin
                  (:nbutton
                    :id "previous-source"
                    :text (:raw (gethash "up.svg" *static-data*))
                    :title (format nil "Previous source (~a)"
                                   (binding-keys (sym:resolve-symbol :previous-source
                                                                     :command)
                                                 :modes (modes prompt-buffer)))
                    :buffer prompt-buffer
                    '(funcall (sym:resolve-symbol :previous-source :command)))
                  (prompter:name source)
                  (:span
                   :class "suggestion-and-mark-count"
                   ;; To hide the suggestion count for the source, subclass
                   ;; `prompter:source' and handle the condition.  Note that
                   ;; `suggestion-and-mark-count' relies on the global prompt
                   ;; value `hide-suggestion-count-p'.
                   (suggestion-and-mark-count prompt-buffer
                                              (prompter:suggestions source)
                                              (prompter:marks source)
                                              :enable-marks-p (prompter:enable-marks-p source)))
                  (when (not (prompter:ready-p source)) "(In progress...)"))
                 (:div
                  #-darwin
                  (:nbutton
                    :id "toggle-attributes"
                    :text (:raw (gethash "plus-minus.svg" *static-data*))
                    :title (format nil "Toggle attributes display (~a)"
                                   (binding-keys (sym:resolve-symbol 'toggle-attributes-display
                                                                     :command)
                                                 :modes (modes prompt-buffer)))
                    :buffer prompt-buffer
                    `(funcall (sym:resolve-symbol :toggle-attributes-display :command)
                              :source ,source))))
                (:raw (render-attributes source prompt-buffer))))))
      (ps-eval :async t :buffer prompt-buffer
        (setf (ps:@ (nyxt/ps:qs document "#suggestions") |innerHTML|)
              (ps:lisp
               (sera:string-join (loop for i from current-source-index to last-source-index
                                       for source = (nth i sources)
                                       unless (null (prompter:suggestions source))
                                         collect (source->html source))
                                 +newline+)))))
    (prompt-render-prompt prompt-buffer)))

(defun prompt-render-skeleton (prompt-buffer)
  (html-write (spinneret:with-html-string
                (:head (:nstyle (style prompt-buffer)))
                (:body
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
                          '(funcall (sym:resolve-symbol :quit-prompt-buffer :command)))))
                 (:div :id "suggestions"
                       :style (if (invisible-input-p prompt-buffer)
                                  "visibility:hidden;"
                                  "visibility:visible;"))))
              prompt-buffer))

(defun prompt-render-focus (prompt-buffer)
  (ps-eval :async t :buffer prompt-buffer
    (ps:chain (nyxt/ps:qs document "#input") (focus))))

(defmethod prompt-render ((prompt-buffer prompt-buffer)) ; TODO: Merge into `show-prompt-buffer'?
  (prompt-render-skeleton prompt-buffer)
  (prompt-render-focus prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

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
                                              (active-prompt-buffers (window prompt-buffer)))
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
(defun set-prompt-buffer-input (input &optional (prompt-buffer (current-prompt-buffer)))
  "Set HTML INPUT in PROMPT-BUFFER.
See `update-prompt-input' to update the changes visually."
  (ps-eval :async t :buffer prompt-buffer
    (setf (ps:@ (nyxt/ps:qs document "#input") value)
          (ps:lisp input)))
  (update-prompt-input prompt-buffer input))

(defun wait-on-prompt-buffer (prompt-buffer) ; TODO: Export?  Better name?
  "Block and return PROMPT-BUFFER results."
  (when (prompt-buffer-p prompt-buffer)
    (show-prompt-buffer prompt-buffer)
    (calispel:fair-alt
      ((calispel:? (prompter:result-channel prompt-buffer) results)
       (hide-prompt-buffer prompt-buffer)
       results)
      ((calispel:? (prompter:interrupt-channel prompt-buffer))
       (hide-prompt-buffer prompt-buffer)
       (error 'prompt-buffer-canceled)))))

(sera:eval-always
  (defvar %prompt-args (delete-duplicates
                        (append
                         (public-initargs 'prompt-buffer)
                         (public-initargs 'prompter:prompter)
                         ;; `customize-instance' `:after' arguments:
                         '(extra-modes)))))
(export-always 'prompt)
(sera:eval-always
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
        (setf (getf args :prompt) (string-right-trim (uiop:strcat ":" serapeum:whitespace) prompt-text))))
    (let ((prompt-object-channel (make-channel 1)))
      (ffi-within-renderer-thread
       (lambda ()
         (let ((prompt-buffer (apply #'make-instance
                                     'prompt-buffer
                                     (append args
                                             (list :window (current-window)
                                                   :result-channel (make-channel)
                                                   :interrupt-channel (make-channel))))))
           (calispel:! prompt-object-channel prompt-buffer))))
      (let ((new-prompt (calispel:? prompt-object-channel)))
        (wait-on-prompt-buffer new-prompt)))))

(export-always 'prompt1)
(sera:eval-always
  (defun prompt1 #.(append '(&rest args) `(&key ,@%prompt-args))
    "Return the first result of a prompt."
    (declare #.(cons 'ignorable %prompt-args))
    (first (apply #'prompt args))))

(defmethod prompter:object-attributes ((prompt-buffer prompt-buffer) (source prompter:source))
  (declare (ignore source))
  `(("Prompt" ,(prompter:prompt prompt-buffer))
    ("Input" ,(prompter:input prompt-buffer))))
