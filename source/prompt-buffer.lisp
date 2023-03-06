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
      :documentation "The height occupied by the prompt buffer.
The options are:
- `:default', which sets it to the value of `prompt-buffer-open-height';
- `:fit-to-prompt', which shrinks the height to fit the input area;
- an integer, which corresponds to the height in pixels.")
     (resumable-p t
                  :type boolean)
     (prompter:history (prompt-buffer-generic-history *browser*)
                       ;; No need to export or define the accessor since this is
                       ;; an override of the prompter slot.
                       :accessor nil
                       :export nil
                       :documentation "By default the prompter library creates a
new history for each new prompt buffer.  Here we set the history to be shared globally.")
     ;; TODO: Need a changed-callback?  Probably not, see `search-buffer'.  But
     ;; can we run the postprocessor without running the filter?
     (invisible-input-p
      nil
      :documentation "Whether to replace input by a placeholder character.  This
is useful to conceal passwords.")
     (hide-suggestion-count-p
      nil
      :documentation "Whether to show the number of chosen suggestions inside
brackets.")
     (max-suggestions
      0
      :export nil
      :documentation "Maximum number of total suggestions that were listed at
some point.")
     ;; TODO: Need max-lines?
     ;; (max-lines 10
     ;;               :documentation "Max number of suggestion lines to show.
     ;; You will want edit this to match the changes done to `style'.")
     (hide-single-source-header-p
      nil
      :documentation "Hide source header when there is only one.")
     (mouse-support-p
      t
      :type boolean
      :documentation "Whether to allow mouse events to set and return the
current suggestion in the prompt buffer.")
     (dynamic-attribute-width-p
      nil
      :type boolean
      :documentation "Whether to adjust the column widths to their content.
Columns with more content get more space, at the expense of smaller ones.
The default behavior is to use static widths (either uniform across attributes or provided
in `prompter:object-attributes').
See `nyxt::attribute-widths'.")
     (style
      (theme:themed-css (theme *browser*)
        `(*
          :font-family "monospace,monospace"
          :font-size "14px"
          :line-height "18px")
        `(body
          :border-right "3px solid"
          :border-color ,theme:primary
          :overflow "hidden"
          :margin "0"
          :padding "0")
        `("#prompt-area"
          :background-color ,theme:primary
          :color ,theme:on-primary
          :display "grid"
          :grid-template-columns "auto auto 1fr auto auto"
          :width "100%")
        `("#prompt"
          :padding-left "10px"
          :line-height "26px")
        `("#prompt-input"
          :margin-right "-10px"
          :line-height "26px")
        `("#prompt-extra"
          :line-height "26px"
          :padding-right "7px")
        `("#prompt-modes"
          :background-color ,theme:primary
          :min-width "5px"
          :line-height "26px"
          :padding-left "3px"
          :padding-right "3px")
        `("#close-button"
          :padding-right "3px"
          :line-height "24px"
          :font-weight "bold"
          :font-size "20px")
        `(".arrow-left"
          :clip-path "polygon(10px 0, 100% 0, 100% 100%, 10px 100%, 0% 50%)")
        `(button
          :background "transparent"
          :color "inherit"
          :text-decoration "none"
          :border "none"
          :padding 0
          :font "inherit"
          :outline "inherit")
        `(.button.accent
          :background-color ,theme:accent
          :color ,theme:on-accent)
        `((:and .button :hover)
          :opacity 0.6)
        `((:and .button (:or :visited :active))
          :color ,theme:background)
        `("#input"
          :background-color ,theme:background
          :color ,theme:on-background
          :opacity 0.9
          :border "none"
          :outline "none"
          :padding "3px"
          :width "100%"
          :autofocus "true")
        `(".source"
          :margin-left "10px"
          :margin-top "15px")
        `(".source-name"
          :background-color ,theme:secondary
          :color ,theme:on-secondary
          :padding-left "5px"
          :line-height "24px")
        `("#suggestions"
          :background-color ,theme:background
          :color ,theme:on-background
          :overflow-y "hidden"
          :overflow-x "hidden"
          :height "100%"
          :margin-right "3px")
        `(".source-content"
          :background-color ,theme:background
          :color ,theme:on-background
          :margin-left "16px"
          :width "100%"
          :table-layout "fixed"
          (td
           :white-space "nowrap"
           :height "20px"
           :overflow "auto")
          (th
           :background-color ,theme:primary
           :color ,theme:on-primary
           :font-weight "normal"
           :padding-left "3px"
           :text-align "left")
          ("td::-webkit-scrollbar"
           :display "none"))
        `("#selection"
          :background-color ,theme:accent
          :color ,theme:on-accent)
        `(.marked
          :background-color ,theme:secondary
          :color ,theme:on-secondary
          :font-weight "bold")
        `(.selected
          :background-color ,theme:primary
          :color ,theme:on-primary))
      :documentation "The CSS applied to prompt buffer.")
     (override-map
      (make-keymap "override-map")
      :type keymaps:keymap
      :documentation "Keymap that takes precedence over all modes' keymaps.
See `buffer's `override-map' for more details."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:export-predicate-name-p t)
    (:accessor-name-transformer (class*:make-name-transformer name))
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
                 prompt-buffer)
  (when (or (invisible-input-p prompt-buffer)
            (and (sera:single (prompter:sources prompt-buffer))
                 ;; Using eq here because we don't want to trigger it for
                 ;; raw-source subclasses, don't we?
                 (eq 'prompter:raw-source
                     (sera:class-name-of (first (prompter:sources prompt-buffer))))))
    (setf (height prompt-buffer) :fit-to-prompt)))

(export-always 'current-source)
(defun current-source (&optional (prompt-buffer (current-prompt-buffer)))
  (prompter:current-source prompt-buffer))

(export-always 'current-suggestion-value)
(defun current-suggestion-value (&optional (prompt-buffer (current-prompt-buffer)))
  "Return selected PROMPT-BUFFER suggestion value.
Return source as second value.
To access the suggestion instead, see `prompter:%current-suggestion'."
  (multiple-value-bind (suggestion source)
      (prompter:%current-suggestion prompt-buffer)
    (values (when suggestion (prompter:value suggestion)) source)))

(defun show-prompt-buffer (prompt-buffer &key (height (height prompt-buffer)))
  "Show the last active PROMPT-BUFFER, if any.
This is a low-level display function.
See also `hide-prompt-buffer'."
  ;; TODO: Add method that returns if there is only 1 source with no filter.
  (when prompt-buffer
    (push prompt-buffer (active-prompt-buffers (window prompt-buffer)))
    (calispel:! (prompt-buffer-channel (window prompt-buffer)) prompt-buffer)
    (prompt-render prompt-buffer)
    (setf (ffi-height prompt-buffer)
          (case height
            (:default (prompt-buffer-open-height (window prompt-buffer)))
            (:fit-to-prompt
             (ps-eval :buffer prompt-buffer
               (ps:chain (nyxt/ps:qs document "#prompt") offset-height)))
            (t height)))
    (run-thread "Show prompt watcher"
      (let ((prompt-buffer prompt-buffer))
        (update-prompt-input prompt-buffer)
        (hooks:run-hook (prompt-buffer-ready-hook *browser*) prompt-buffer)))))

(defun hide-prompt-buffer (prompt-buffer)
  "Hide PROMPT-BUFFER, display next active one.
This is a low-level display function.
See also `show-prompt-buffer'."
  (let ((window (window prompt-buffer)))
    ;; Note that PROMPT-BUFFER is not necessarily first in the list, e.g. a new
    ;; prompt-buffer was invoked before the old one reaches here.
    (alex:deletef (active-prompt-buffers window) prompt-buffer)
    ;; The channel values are irrelevant, so is the element order:
    (calispel:? (prompt-buffer-channel (window prompt-buffer)) 0)
    (if (resumable-p prompt-buffer)
        (flet ((prompter= (prompter1 prompter2)
                 (and (string= (prompter:prompt prompter1)
                               (prompter:prompt prompter2))
                      (string= (prompter:input prompter1)
                               (prompter:input prompter2)))))
          ;; Delete previous, similar prompts, if any.
          (mapc (lambda (old-prompt)
                  (when (and (prompter= old-prompt prompt-buffer)
                             (not (eq old-prompt prompt-buffer)))
                    (ffi-buffer-delete old-prompt)))
                (old-prompt-buffers *browser*))
          (alex:deletef (old-prompt-buffers *browser*)
                        prompt-buffer
                        :test #'prompter=)
          (push prompt-buffer (old-prompt-buffers *browser*)))
        (ffi-buffer-delete prompt-buffer))
    (if (active-prompt-buffers window)
        (let ((next-prompt-buffer (first (active-prompt-buffers window))))
          (show-prompt-buffer next-prompt-buffer))
        (setf (ffi-height prompt-buffer) 0))))

(defun suggestion-and-mark-count (prompt-buffer suggestions marks
                                  &key enable-marks-p pad-p)
  (alex:maxf (max-suggestions prompt-buffer)
             (length suggestions))
  (labels ((decimals (n)
             (cond
               ((< n 0)
                (decimals (- n)))
               ((< n 10)
                1)
               (t (1+ (decimals (truncate n 10)))))))
    (cond
      ((not suggestions)
       "")
      ((hide-suggestion-count-p prompt-buffer)
       "")
      (t
       (let ((padding (if pad-p
                          (prin1-to-string (decimals (max-suggestions prompt-buffer)))
                          "0")))
         (format nil (str:concat "[~a~" padding ",,,' @a]")
                 (cond
                   ((or marks enable-marks-p)
                    (format nil
                            (str:concat "~" padding ",,,' @a/")
                            (length marks)))
                   (t ""))
                 (length suggestions)))))))

(defun prompt-render-prompt (prompt-buffer)
  (let* ((suggestions (prompter:all-suggestions prompt-buffer))
         (marks (prompter:all-marks prompt-buffer))
         ;; TODO: Should prompt-buffer be a status-buffer?
         ;; Then no need to depend on the current status buffer.
         (status-buffer (status-buffer (current-window))))
    (ps-eval :async t :buffer prompt-buffer
      (setf (ps:@ (nyxt/ps:qs document "#prompt-extra") |innerHTML|)
            (ps:lisp
             (suggestion-and-mark-count
              prompt-buffer suggestions marks
              :pad-p t
              :enable-marks-p (some #'prompter:enable-marks-p
                                       (prompter:sources prompt-buffer)))))
      (setf (ps:@ (nyxt/ps:qs document "#prompt-modes") |innerHTML|)
            (ps:lisp (str:join " "
                               (mapcar (curry #'mode-status status-buffer)
                                       (sort-modes-for-status (modes prompt-buffer)))))))))

(defun average-attribute-width (attribute-values)
  (let* ((values (remove-if #'str:blankp attribute-values))
         (total (reduce #'+ values :key #'length)))
    (if (zerop (length values))
        0
        (/ total (length values)))))

(defgeneric attribute-widths (source &key width-function dynamic-p)
  (:method (source &key (width-function #'average-attribute-width) dynamic-p)
    (labels ((clip-extremes (widths)
               (let* ((length (length widths))
                      (minimal-size (/ 1 length 2)))
                 (cond
                   ((= 1 length)
                    (list 1))
                   ((some (rcurry #'< minimal-size) widths)
                    (let* ((lacking (remove-if-not #'plusp
                                                   (mapcar (curry #'- minimal-size) widths)))
                           (abundant (set-difference widths lacking))
                           (lack (reduce #'+ lacking)))
                      (loop for width in widths
                            when (< width minimal-size)
                              collect minimal-size into new-widths
                            else
                              collect (- width
                                         (* lack
                                            (/ width
                                               (reduce #'+ abundant))))
                                into new-widths
                            finally (return new-widths))))
                   (t widths))))
             (width-normalization (widths)
               (let ((total (reduce #'+ widths)))
                 (mapcar (lambda (w) (if (zerop total) 0 (/ w total))) widths))))
      ;; FIXME: This loop ignores attribute names in the attribute width
      ;; computation. Because of this, attribute names could theoretically get
      ;; cropped if the values are short enough. This is bad, but not exactly
      ;; critical.
      (if dynamic-p
          (loop with suggestions = (prompter:suggestions source)
                with attributes = (mapcar (rcurry #'prompter:active-attributes :source source) suggestions)
                for key in (prompter:active-attributes-keys source)
                for width
                  = (funcall width-function (mapcar (compose #'first (rcurry #'str:s-assoc-value key))
                                                    attributes))
                collect width into widths
                finally (return (clip-extremes (width-normalization widths))))
          (sera:and-let* ((suggestions (prompter:suggestions source))
                          (fourth-attributes
                           (mapcar #'fourth (prompter:active-attributes (first suggestions) :source source)))
                          (coefficients (substitute 1 nil fourth-attributes)))
            (width-normalization coefficients)))))
  (:documentation "Compute the widths of SOURCE attribute columns (as percent).

If DYNAMIC-P, compute widths based on content length. Otherwise, rely on the
fourth value of `prompter:object-attributes' for the given source.

Returns a list of ratios that sum up to one.
Uses the WIDTH-FUNCTION (by default computing average length of non-blank
attribute values) to derive the width of the list of attribute
values. WIDTH-FUNCTION is a function accepting a list of strings and returning
an integer."))

(defun render-attributes (source prompt-buffer)
  (spinneret:with-html-string
    (when (prompter:suggestions source)
      (:table :class "source-content"
              (:colgroup
               (dolist (width (attribute-widths
                               source :dynamic-p (dynamic-attribute-width-p prompt-buffer)))
                 (:col :style (format nil "width: ~,2f%" (* 100 width)))))
              (:tr :style (if (or (eq (prompter:hide-attribute-header-p source) :always)
                                  (and (eq (prompter:hide-attribute-header-p source) :single)
                                       (sera:single (prompter:active-attributes-keys source))))
                              "display:none;"
                              "display:revert;")
                   (loop for attribute-key in (prompter:active-attributes-keys source)
                         collect (:th (spinneret::escape-string attribute-key))))
              (loop
                ;; TODO: Only print as many lines as fit the height.
                ;; But how can we know in advance?
                with max-suggestion-count = 10
                repeat max-suggestion-count
                with cursor-index = (prompter:current-suggestion-position prompt-buffer)
                for suggestion-index from (max 0 (- cursor-index (/ max-suggestion-count 2)))
                for suggestion in (nthcdr suggestion-index (prompter:suggestions source))
                collect
                (let ((suggestion-index suggestion-index)
                      (cursor-index cursor-index))
                  (:tr :id (when (equal (list suggestion source)
                                        (multiple-value-list (prompter:%current-suggestion prompt-buffer)))
                             "selection")
                       :class (when (prompter:marked-p source (prompter:value suggestion))
                                "marked")
                       :onmousedown (when (mouse-support-p prompt-buffer)
                                      (ps:ps
                                       (lambda (event)
                                         (nyxt/ps:lisp-eval
                                          (:title "choose-this-suggestion"
                                                  :buffer prompt-buffer)
                                          (prompter::set-current-suggestion
                                           (current-prompt-buffer)
                                           (- suggestion-index cursor-index)))
                                         (cond
                                           ;; Ctrl-click to mark a single suggestion.
                                           ;; TODO: Shift-click to mark a region.
                                           ((or (ps:@ event meta-key)
                                                (ps:@ event ctrl-key))
                                            (nyxt/ps:lisp-eval
                                             (:title "mark-this-suggestion"
                                                     :buffer prompt-buffer)
                                             (funcall (read-from-string "nyxt/prompt-buffer-mode:toggle-mark")
                                                      prompt-buffer)))
                                           (t (nyxt/ps:lisp-eval
                                               (:title "return-this-suggestion"
                                                       :buffer prompt-buffer)
                                               (prompter:run-action-on-return
                                                (nyxt::current-prompt-buffer))))))))
                       (loop for (nil attribute attribute-display)
                               in (prompter:active-attributes suggestion :source source)
                             collect (:td :title attribute
                                          (if attribute-display
                                              (:raw attribute-display)
                                              attribute))))))))))

(export 'prompt-render-suggestions)
(defmethod prompt-render-suggestions ((prompt-buffer prompt-buffer))
  "Refresh the rendering of the suggestion list.
This does not redraw the whole prompt buffer, unlike `prompt-render'."
  (let* ((sources (prompter:sources prompt-buffer))
         (current-source-index (position (current-source prompt-buffer) sources))
         (last-source-index (1- (length sources))))
    (flet ((source->html (source)
             (spinneret:with-html-string
               (:div :class "source"
                     (:div :class "source-name"
                           :style (if (and (hide-single-source-header-p prompt-buffer)
                                           (sera:single sources))
                                      "display:none;"
                                      "display:revert")
                           (prompter:name source)
                           (if (prompter:hide-suggestion-count-p source)
                               ""
                               (suggestion-and-mark-count prompt-buffer
                                                          (prompter:suggestions source)
                                                          (prompter:marks source)
                                                          :enable-marks-p (prompter:enable-marks-p source)))
                           (if (prompter:ready-p source)
                               ""
                               "(In progress...)"))
                     (:raw (render-attributes source prompt-buffer))))))
      (ps-eval :buffer prompt-buffer
        (setf (ps:@ (nyxt/ps:qs document "#suggestions") |innerHTML|)
              (ps:lisp
               (sera:string-join (loop for i from current-source-index to last-source-index
                                       for source = (nth i sources)
                                       unless (null (prompter:suggestions source))
                                         collect (source->html source))
                                 +newline+)))))
    (prompt-render-prompt prompt-buffer)))

(defun erase-document (prompt-buffer)
  (ps-eval :async t :buffer prompt-buffer
    (ps:chain document (open))
    (ps:chain document (close))))

(defun prompt-render-skeleton (prompt-buffer)
  (erase-document prompt-buffer)
  (html-set (spinneret:with-html-string
              (:head
               (:nstyle (style prompt-buffer)))
              (:body
               (:div :id "prompt-area"
                     (:div :id "prompt" (:mayberaw (prompter:prompt prompt-buffer)))
                     (:div :id "prompt-extra" "[?/?]")
                     (:div :id "prompt-input"
                           (:input :type (if (invisible-input-p prompt-buffer)
                                             "password"
                                             "text")
                                   :id "input"
                                   :value (prompter:input prompt-buffer)))
                     (:div :id "prompt-modes" :class "arrow-left" "")
                     (:div :id "close-button"
                           (:nbutton
                             :text "×"
                             :title "Close prompt"
                             :buffer prompt-buffer
                             (funcall (sym:resolve-symbol :quit-prompt-buffer :command)))))
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
               (let ((next-source (when (find prompt-buffer (active-prompt-buffers (window prompt-buffer)))
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
  (ps-eval :buffer prompt-buffer
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
    (unless *interactive-p*
      (restart-case
          (error 'prompt-buffer-non-interactive :name prompter:prompt)
        (prompt-anyway () nil)
        (cancel () (error 'prompt-buffer-canceled))))
    (alex:when-let ((prompt-text (getf args :prompt)))
      (when (str:ends-with-p ":" prompt-text)
        (log:warn "Prompt text ~s should not end with a ':'." prompt-text)
        (setf (getf args :prompt) (string-right-trim (uiop:strcat ":" serapeum:whitespace) prompt-text))))
    (let ((prompt-object-channel (make-channel 1)))
      (ffi-within-renderer-thread
       *browser*
       (lambda ()
         (let ((prompt-buffer (apply #'make-instance 'prompt-buffer
                                     (append args
                                             (list
                                              :window (current-window)
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

(define-class resume-prompt-source (prompter:source)
  ((prompter:name "Resume prompters")
   (prompter:constructor (old-prompt-buffers *browser*))
   ;; TODO: Remove duplicates.
   ;; TODO: History?
   ))

(defmethod prompter:object-attributes ((prompt-buffer prompt-buffer) (source prompter:source))
  (declare (ignore source))
  `(("Prompt" ,(prompter:prompt prompt-buffer))
    ("Input" ,(prompter:input prompt-buffer))))

(define-command resume-prompt ()
  "Query an older prompt and resume it."
  (let ((old-prompt (prompt1 :prompt "Resume prompt session"
                             :resumable-p nil
                             :sources 'resume-prompt-source)))
    (when old-prompt
      (prompter:resume old-prompt)
      (wait-on-prompt-buffer old-prompt))))
