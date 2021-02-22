;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmacro define-function (name args docstring &body body)
  "Eval ARGS and DOCSTRING then define function over the resulting lambda list
and string.
All ARGS are declared as `ignorable'."
  (let ((evaluated-args (eval args))
        (evaluated-docstring (eval docstring)))
    `(defun ,name ,evaluated-args
       ,evaluated-docstring
       (declare (ignorable ,@(set-difference (mapcar (lambda (arg) (if (listp arg) (first arg) arg))
                                                     evaluated-args)
                                             lambda-list-keywords)))
       ,@body)))

(sera:eval-always
  (define-class prompt-buffer (user-internal-buffer)
    ((prompter (error "Prompter required") ; TODO: Inherit instead?
               :type prompter:prompter)
     (default-modes '(prompt-buffer-mode))
     (resumable-p t
                  :type boolean)
     ;; TODO: Need a changed-callback?
     ;; TODO: Need a invisible-input-p slot?
     (invisible-input-p nil
                        :documentation "If non-nil, input is replaced by placeholder character.
;; This is useful to conceal passwords.")
     (hide-suggestion-count-p nil       ; TODO: Move to `prompter' library?
                              :documentation "Show the number of chosen suggestions
;; inside brackets. It can be useful to disable, for instance for a yes/no question.")
     ;; TODO: If we move selection cursor to `prompter' library, then it can be
     ;; restored when resuming.
     ;; (suggestion-head 0
     ;;                  :export nil)
     ;; (suggestion-cursor 0
     ;;                    :export nil)
     (content ""
              :accessor nil
              :export nil
              :documentation "The HTML content of the minibuffer.")
     ;; TODO: Need max-lines?
     ;; (max-lines 10
     ;;               :documentation "Max number of suggestion lines to show.
     ;; You will want edit this to match the changes done to `style'.")
     (style #.(cl-css:css
               '((* :font-family "monospace,monospace"
                    :font-size "14px"
                    :line-height "18px")
                 (body
                  :overflow "hidden"
                  :margin "0"
                  :padding "0")
                 ("#prompt-area"
                  :background-color "dimgray"
                  :display "grid"
                  :grid-template-columns "auto auto 1fr"
                  :width "100%"
                  :color "white")
                 ("#prompt"
                  :padding-left "10px"
                  :line-height "26px")
                 ("#prompt-extra"
                  :line-height "26px"
                  :padding-right "7px")
                 ("#input"
                  :border "none"
                  :outline "none"
                  :padding "3px"
                  :background-color "#E8E8E8"
                  :width "100%"
                  :autofocus "true")
                 (".source"
                  :margin-left "10px"
                  :margin-top "15px")
                 (".source-glyph"
                  :margin-right "3px")
                 (".source-name"
                  :color "white"
                  :padding-left "5px"
                  :line-height "24px"
                  :background-color "gray")
                 ("#suggestions"
                  :flex-grow "1"
                  :overflow-y "auto"
                  :overflow-x "hidden"
                  :width "100%")
                 (".source-content"
                  :margin-left "16px"
                  :background-color "#F7F7F7"
                  :width "100%"
                  :table-layout "fixed")
                 (".source-content td"
                  :white-space "nowrap"
                  :overflow "auto")
                 (".source-content th"
                  :font-weight "normal"
                  :padding-left "3px"
                  :text-align "left"
                  :background-color "#E8E8E8"
                  :white-space "nowrap"
                  :overflow "auto")
                 (".source-content td::-webkit-scrollbar"
                  :display "none")
                 ("#selection"
                  :background-color "575757"
                  :color "white")
                 (ul :list-style "none"
                     :padding "0"
                     :margin "0")
                 (li :padding "2px")
                 (.marked :background-color "darkgray"
                          :font-weight "bold"
                          :color "white")
                 (.selected :background-color "gray"
                            :color "white")))
            :documentation "The CSS applied to a minibuffer when it is set-up.")
     (override-map (let ((map (make-keymap "overide-map")))
                     (define-key map
                       "escape"
                       ;; We compute symbol at runtime because
                       ;; nyxt/minibuffer-mode does not exist at
                       ;; compile-time since it's loaded afterwards.
                       (find-symbol (string 'cancel-input)
                                    (find-package 'nyxt/prompt-buffer-mode))))
                   :type keymap:keymap
                   :documentation "Keymap that takes precedence over all modes' keymaps."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:documentation "The prompt buffer is the interface for user interactions.
Each prompt spawns a new object: this makes it possible to nest prompts , such
as invoking `prompt-history'.

A prompt query is typically done as follows:

\(let ((tags (prompt-minibuffer
              :input-prompt \"Space-separated tag (s) \"
              :default-modes '(set-tag-mode minibuffer-mode)
              :suggestion-function (tag-suggestion-filter))))
  ;; Write form here in which `tags' is bound to the resulting element(s).
  )")))

(define-user-class prompt-buffer)

(defmethod initialize-instance :after ((prompt-buffer prompt-buffer) &key) ; TODO: Merge in `make-prompt-buffer'?
  (hooks:run-hook (minibuffer-make-hook *browser*) prompt-buffer) ; TODO: Rename `minibuffer'.
  ;; We don't want to show the input in the suggestion list when invisible.
  (when (invisible-input-p prompt-buffer)
    (dolist (source (prompter:sources (prompter prompt-buffer)))
      ;; This way the minibuffer won't display the input as a suggestion.
      (setf (prompter:must-match-p source) t)))
  (initialize-modes prompt-buffer))

(export-always 'make-prompt-buffer)
(define-function make-prompt-buffer (append
                                     '(&rest args)
                                     `(&key (window (current-window))
                                            ,@(public-initargs 'prompt-buffer)))
  "TODO: Complete me!"
  (let* ((initargs (alex:remove-from-plist args :window)) ; TODO: Make window a slot or prompt-buffer? That would simplify `make-prompt-buffer' args.
         (prompt-buffer (apply #'make-instance 'prompt-buffer initargs)))
    ;; (update-display prompt-buffer) ; TODO: Remove when sure.
    (show-prompt-buffer prompt-buffer :window window)
    ;; TODO: Add method that returns if there is only 1 source with no filter.
    ;; (apply #'show
    ;;        (unless (prompter:filter prompt-buffer)
    ;;          ;; We don't need so much height since there is no suggestion to display.
    ;;          (list :height (minibuffer-open-single-line-height (current-window)))))
    ))

(export-always 'current-source)
(defun current-source (&optional (prompt-buffer (current-prompt-buffer)))
  (prompter:selected-source (prompter prompt-buffer)))

(export-always 'current-suggestion)
(defun current-suggestion (&optional (prompt-buffer (current-prompt-buffer)))
  "Return selected prompt-buffer suggestion.
To access the actual value, call `prompter:value' over the returned suggestion.
Return source as second value."
  (prompter:selected-suggestion (prompter prompt-buffer)))

(export-always 'all-marked-suggestions)
(defun all-marked-suggestions (&optional (prompt-buffer (current-prompt-buffer)))
  "Return the list of the marked suggestion values in the prompt-buffer."
  (prompter:all-marked-suggestions (prompter prompt-buffer)))

(defun show-prompt-buffer (prompt-buffer &key (window (current-window)) height)
  "Show the last active prompt-buffer, if any."
  (when prompt-buffer
    (push prompt-buffer (active-minibuffers window))
    (erase-document prompt-buffer)      ; TODO: When to erase?
    (update-display prompt-buffer)
    (pexec ()
      (watch-prompt prompt-buffer))
    (ffi-window-set-prompt-buffer-height
     (current-window)
     (or height
         (minibuffer-open-height (current-window)))))) ; TODO: Rename `minibuffer'.

(defmethod state-changed ((prompt-buffer prompt-buffer)) ; TODO: Remove when done.
  nil)

(export-always 'hide-prompt-buffer)
(defun hide-prompt-buffer (prompt-buffer &optional return-function) ; TODO: Rename `hide'
  "Hide PROMPT-BUFFER, display next active one, and return PROMPT-BUFFER suggestion."
  ;; Note that PROMPT-BUFFER is not necessarily first in the list, e.g. a new
  ;; prompt-buffer was invoked before the old one reaches here.
  (alex:deletef (active-minibuffers (current-window)) prompt-buffer)
  (when (resumable-p prompt-buffer)
    (flet ((prompter= (prompter1 prompter2)
             (and (string= (prompter:prompt prompter1)
                           (prompter:prompt prompter2))
                  (string= (prompter:input prompter1)
                           (prompter:input prompter2)))))
      ;; Delete a previous, similar prompt, if any.
      (alex:deletef (old-prompt-buffers *browser*)
                    (prompter prompt-buffer)
                    :key #'prompter
                    :test #'prompter=)
      (push prompt-buffer (old-prompt-buffers *browser*))))
  (if (active-minibuffers (current-window))
      (let ((next-prompt-buffer (first (active-minibuffers (current-window)))))
        ;; TODO: Remove when done with `minibuffer'.
        (if (prompt-buffer-p next-prompt-buffer)
            (show-prompt-buffer next-prompt-buffer)
            (show))
        ;; TODO: Remove?
        ;; We need to refresh so that the nested prompt-buffers don't have to do it.
        ;; (state-changed (first (active-minibuffers (current-window))))
        ;; (update-display (first (active-minibuffers (current-window))))
        )
      (progn
        (ffi-window-set-prompt-buffer-height (current-window) 0)))
  (when return-function
    (funcall return-function))
  ;; Destroy prompter last, or else `return-function' may not work.
  (prompter:destroy (nyxt:prompter prompt-buffer)))

(export-always 'evaluate-script)
(defmethod evaluate-script ((prompt-buffer prompt-buffer) script) ; TODO: Remove?
  "Evaluate SCRIPT into PROMPT-BUFFER's webview.
The new webview HTML content is set as the MINIBUFFER's `content'."
  (when prompt-buffer
    (let ((new-content (str:concat script (ps:ps (ps:chain document body |outerHTML|))))) ; TODO: Why do we postfix with this (ps:ps ... |outerHTML|)?
      (ffi-minibuffer-evaluate-javascript-async
       (current-window)
       new-content))))

(defmethod erase-document ((prompt-buffer prompt-buffer)) ; TODO: Remove, empty automatically when `content' is set?
  (evaluate-script prompt-buffer (ps:ps
                                   (ps:chain document (open))
                                   (ps:chain document (close)))))

(defmethod generate-prompt-html ((prompt-buffer prompt-buffer))
  (markup:markup
   (:head (:style (style prompt-buffer)))
   (:body
    (:div :id "prompt-area"
          (:div :id "prompt" (prompter:prompt (prompter prompt-buffer)))
          ;; TODO: See minibuffer `generate-prompt-html' to print the counts.
          (:div :id "prompt-extra" "[?/?]")
          (:div (:input :type "text" :id "input" :value (prompter:input (prompter prompt-buffer)))))
    ;; TODO: Support multi columns and sources.
    (:div :id "suggestions"))))


(export 'update-suggestion-html)
(defmethod update-suggestion-html ((prompt-buffer prompt-buffer))
  (let* ((sources (prompter:sources (prompter prompt-buffer)))
         (current-source-index (position (current-source prompt-buffer) sources))
         (last-source-index (1- (length sources))))
    ;; TODO: Factor out property printing.
    ;; TODO: Only print `active-properties'.
    (flet ((source->html (source)
             (markup:markup
              (:div :class "source"
                    (:div :class "source-name" (:span :class "source-glyph" "⛯") (prompter:name source))
                    (:table :class "source-content"
                            (:tr
                             (loop with property-sample = (if (first (prompter:suggestions source)) ; TODO: Instead, ensure that suggestions always has an element?
                                                              (prompter:properties (first (prompter:suggestions source)))
                                                              (list :default ""))
                                   for (property-name _) on property-sample by #'cddr
                                   collect (markup:markup (:th (str:capitalize (symbol-name property-name))))))
                            (loop ;; TODO: Only print as many lines as fit the height.  But how can we know in advance?
                                  ;; Maybe first make the table, then add the element one by one _if_ there are into view.
                                  with max-suggestion-count = 10
                                  repeat max-suggestion-count
                                  with cursor-index = (prompter:selected-suggestion-position (prompter prompt-buffer))
                                  for suggestion-index from (max 0 (- cursor-index (/ max-suggestion-count 2)))
                                  for suggestion in (nthcdr suggestion-index (prompter:suggestions source))
                                  collect (markup:markup
                                           (:tr :id (when (equal (list suggestion source)
                                                                 (multiple-value-list (prompter:selected-suggestion (prompter prompt-buffer))))
                                                      "selection")
                                                :class (when (find (prompter:value suggestion) (prompter:marked-suggestions source))
                                                         "marked")
                                                (loop for (_ property) on (prompter:properties suggestion) by #'cddr
                                                      collect (markup:markup (:td property)))))))))))
      (evaluate-script
       prompt-buffer
       (ps:ps
         (setf (ps:chain document (get-element-by-id "suggestions") |innerHTML|)
               (ps:lisp
                (str:join (string #\newline)
                          (loop for i from current-source-index to last-source-index
                                for source = (nth i sources)
                                collect (source->html source))))))))

    (let* ((source (current-source prompt-buffer))
           (suggestions (prompter:suggestions source))
           (marked-suggestions (prompter:marked-suggestions source)))
      (evaluate-script
       prompt-buffer
       (ps:ps
         (setf (ps:chain document (get-element-by-id "prompt-extra") |innerHTML|)
               (ps:lisp
                (cond
                  ((not suggestions)
                   "")
                  ((hide-suggestion-count-p prompt-buffer)
                   "")
                  (marked-suggestions
                   (format nil "[~a/~a]"
                           (length marked-suggestions)
                           (length suggestions)))
                  ((and (not marked-suggestions)
                        (prompter:multi-selection-p source))
                   (format nil "[0/~a]"
                           (length suggestions)))
                  ((not marked-suggestions)
                   (format nil "[~a]"
                           (length suggestions)))))))))))

(defmethod update-display ((prompt-buffer prompt-buffer)) ; TODO: Merge into `show'?
  ;; TODO: Finish me!
  (ffi-minibuffer-evaluate-javascript-async ; TODO: Replace with `evaluate-script'?  Rename the latter?
   (current-window)
   (ps:ps (ps:chain document
                    (write (ps:lisp (str:concat (generate-prompt-html prompt-buffer)))))))
  ;; TODO: The following is supposed to focus on the HTML input but does not work.
  (ffi-minibuffer-evaluate-javascript-async
   (current-window)
   (ps:ps (ps:chain document
                    (get-element-by-id "input")
                    (focus))))
  (update-suggestion-html prompt-buffer))

(defun watch-prompt (prompt-buffer)
  "This blocks and updates the view."
  ;; TODO: Stop loop when prompt-buffer is no longer current.
  (sera:nlet maybe-update-view ((next-source (prompter:next-ready-p (prompter prompt-buffer))))
    (cond
      ;; Nothing to do:
      ((eq t next-source) t)
      ((null next-source) nil)
      (t ;; At least one source got updated.
       (update-suggestion-html prompt-buffer)
       (maybe-update-view (prompter:next-ready-p (prompter prompt-buffer)))))))

(defun set-prompt-input (prompt-buffer input)
  "Set prompter's INPUT in PROMPT-BUFFER."
  (setf (prompter:input (prompter prompt-buffer))
        input))

(defun set-prompt-buffer-input (input)
  "Set HTML INPUT in PROMPT-BUFFER."
  (when (first (active-minibuffers (current-window)))
    (ffi-minibuffer-evaluate-javascript
     (current-window)
     (ps:ps
       (setf (ps:chain document (get-element-by-id "input") value)
             (ps:lisp input))))))

(export-always 'prompt)
(defun prompt (&key prompter prompt-buffer)
  "Open the prompt buffer, ready for user input.
PROMPTER and PROMPT-BUFFER are plists of keyword arguments passed to the
prompt-buffer constructor.

Example use:

\(prompt
  :prompter (list
             :sources (list (make-instance 'prompter:source :filter #'my-suggestion-filter)))
  :prompt-buffer (list ...))

See the documentation of `prompt-buffer' to know more about the options."
  (let ((prompt (apply #'prompter:make prompter)))
    (ffi-within-renderer-thread
     *browser*
     (lambda ()
       (apply 'make-prompt-buffer (append (list :prompter prompt)
                                          prompt-buffer))))
    ;; Wait until it's destroyed and get the selections from `return-selection'.
    (calispel:fair-alt
      ((calispel:? (prompter:result-channel prompt) results)
       results)
      ((calispel:? (prompter:interrupt-channel prompt))
       (error 'nyxt-prompt-buffer-canceled)))))

(export-always 'prompter-if-confirm)    ; TODO: Rename to `if-confirm' once `minibuffer' is gone.
(defmacro prompter-if-confirm (prompt yes-form &optional no-form)
  "Ask the user for confirmation before executing either YES-FORM or NO-FORM.
YES-FORM is executed on  \"yes\" answer, NO-FORM -- on \"no\".
PROMPT is a list fed to `format nil'.

Example usage defaulting to \"no\":

\(let ((*yes-no-choices* '(:no \"no\" :yes \"yes\")))
  (if-confirm (\"Are you sure to kill ~a buffers?\" count)
     (delete-buffers)))"
  ;; TODO: Can we keep the `*yes-no-choices*' customization option?
  `(let ((answer (prompt
                  :input-prompt (format nil ,@prompt)
                  :suggestion-function (yes-no-suggestion-filter)
                  :hide-suggestion-count-p t)))
     (if (confirmed-p answer)
         ,yes-form
         ,no-form)))

(defmethod prompter:object-properties ((prompt-buffer prompt-buffer))
  (list :prompt (prompter:prompt (prompter prompt-buffer))
        :input (prompter:input (prompter prompt-buffer))))

(define-class resume-prompt-source (prompter:source)
  ((prompter:name "Resume prompters")
   (prompter:initial-suggestions (old-prompt-buffers *browser*))
   ;; TODO: Remove duplicates.
   ;; TODO: History?
   ))

(define-command resume-prompt ()
  "Query an older prompt and resume it."
  (let ((old-prompt
          (prompt
           :prompt-buffer (list :resumable-p nil)
           :prompter (list
                      :prompt "Resume prompt session"
                      :sources (list (make-instance 'resume-prompt-source))))))
    (when old-prompt
      (prompter:resume (prompter old-prompt))
      (show-prompt-buffer old-prompt))))
