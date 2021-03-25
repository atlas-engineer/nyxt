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
  (define-class prompt-buffer (user-internal-buffer prompter:prompter)
    ((default-modes '(prompt-buffer-mode))
     (window nil
             :type (or null window)
             :export nil
             :documentation "The window in which the prompt buffer is showing.")
     (resumable-p t
                  :type boolean)
     ;; TODO: Need a changed-callback?  Probably not, see `search-buffer'.  But
     ;; can we run the postprocessor without running the filter?
     ;; TODO: Need a invisible-input-p slot?
     (invisible-input-p nil
                        :documentation "If non-nil, input is replaced by placeholder character.
;; This is useful to conceal passwords.")
     (hide-suggestion-count-p nil       ; TODO: Move to `prompter' library?
                              :documentation "Show the number of chosen suggestions
;; inside brackets. It can be useful to disable, for instance for a yes/no question.")
     (content ""
              :accessor nil
              :export nil
              :documentation "The HTML content of the prompt-buffer.")
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
                  :overflow-y "hidden"
                  :overflow-x "hidden"
                  :height "100%"
                  :width "100%")
                 (".source-content"
                  :margin-left "16px"
                  :background-color "#F7F7F7"
                  :width "100%"
                  :table-layout "fixed")
                 (".source-content td"
                  :white-space "nowrap"
                  :height "20px"
                  :overflow "auto")
                 (".source-content th"
                  :font-weight "normal"
                  :padding-left "3px"
                  :text-align "left"
                  :background-color "#E8E8E8")
                 (".source-content td::-webkit-scrollbar"
                  :display "none")
                 ("#selection"
                  :background-color "575757"
                  :color "white")
                 (.marked :background-color "darkgray"
                          :font-weight "bold"
                          :color "white")
                 (.selected :background-color "gray"
                            :color "white")))
            :documentation "The CSS applied to a prompt-buffer when it is set-up.")
     (override-map (let ((map (make-keymap "override-map")))
                     (define-key map
                       "escape"
                       ;; We compute symbol at runtime because
                       ;; nyxt/prompt-buffer-mode does not exist at
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
as invoking `prompt-buffer-history'.

See `prompt' for how to invoke prompts.")))

(define-user-class prompt-buffer)

(defmethod initialize-instance :after ((prompt-buffer prompt-buffer) &key) ; TODO: Merge in `make-prompt-buffer'?
  (hooks:run-hook (prompt-buffer-make-hook *browser*) prompt-buffer)
  ;; We don't want to show the input in the suggestion list when invisible.
  (when (invisible-input-p prompt-buffer)
    (dolist (source (prompter:sources prompt-buffer))
      ;; This way the prompt-buffer won't display the input as a suggestion.
      (setf (prompter:must-match-p source) t)))
  (initialize-modes prompt-buffer))

(export-always 'current-source)
(defun current-source (&optional (prompt-buffer (current-prompt-buffer)))
  (prompter:selected-source prompt-buffer))

(export-always 'current-suggestion-value)
(defun current-suggestion-value (&optional (prompt-buffer (current-prompt-buffer)))
  "Return selected prompt-buffer suggestion value.
Return source as second value.
To access the suggestion instead, see `prompter:selected-suggestion'."
  (multiple-value-bind (suggestion source)
      (prompter:selected-suggestion prompt-buffer)
    (values (when suggestion (prompter:value suggestion)) source)))

(defun show-prompt-buffer (prompt-buffer &key height)
  "Show the last active prompt-buffer, if any."
  ;; TODO: Add method that returns if there is only 1 source with no filter.
  (when prompt-buffer
    (push prompt-buffer (active-prompt-buffers (window prompt-buffer)))
    (erase-document prompt-buffer)      ; TODO: When to erase?
    (update-display prompt-buffer)
    (run-thread
      (watch-prompt prompt-buffer))
    (ffi-window-set-prompt-buffer-height
     (window prompt-buffer)
     (or height
         (prompt-buffer-open-height (window prompt-buffer))))))

(defmethod state-changed ((prompt-buffer prompt-buffer)) ; TODO: Remove when done.
  nil)

(export-always 'hide-prompt-buffer)
(defun hide-prompt-buffer (prompt-buffer &optional return-function) ; TODO: Rename `hide'
  "Hide PROMPT-BUFFER, display next active one, and return PROMPT-BUFFER suggestion."
  ;; Note that PROMPT-BUFFER is not necessarily first in the list, e.g. a new
  ;; prompt-buffer was invoked before the old one reaches here.
  (alex:deletef (active-prompt-buffers (current-window)) prompt-buffer)
  (when (resumable-p prompt-buffer)
    (flet ((prompter= (prompter1 prompter2)
             (and (string= (prompter:prompt prompter1)
                           (prompter:prompt prompter2))
                  (string= (prompter:input prompter1)
                           (prompter:input prompter2)))))
      ;; Delete a previous, similar prompt, if any.
      (alex:deletef (old-prompt-buffers *browser*)
                    prompt-buffer
                    :test #'prompter=)
      (push prompt-buffer (old-prompt-buffers *browser*))))
  (if (active-prompt-buffers (current-window))
      (let ((next-prompt-buffer (first (active-prompt-buffers (current-window)))))
        (show-prompt-buffer next-prompt-buffer))
      (progn
        (ffi-window-set-prompt-buffer-height (current-window) 0)))
  (when return-function
    (funcall return-function))
  ;; Destroy prompter last, or else `return-function' may not work.
  (prompter:destroy prompt-buffer))

(export-always 'evaluate-script)
(defmethod evaluate-script ((prompt-buffer prompt-buffer) script) ; TODO: Remove?
  "Evaluate SCRIPT into PROMPT-BUFFER's webview.
The new webview HTML content is set as the PROMPT-BUFFER's `content'."
  (when prompt-buffer
    (let ((new-content (str:concat script (ps:ps (ps:chain document body |outerHTML|))))) ; TODO: Why do we postfix with this (ps:ps ... |outerHTML|)?
      (ffi-prompt-buffer-evaluate-javascript-async
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
          (:div :id "prompt" (prompter:prompt prompt-buffer))
          (:div :id "prompt-extra" "[?/?]")
          (:div (:input :type "text" :id "input" :value (prompter:input prompt-buffer))))
    ;; TODO: Support multi columns and sources.
    (:div :id "suggestions"))))


(export 'update-suggestion-html)
(defmethod update-suggestion-html ((prompt-buffer prompt-buffer))
  (let* ((sources (prompter:sources prompt-buffer))
         (current-source-index (position (current-source prompt-buffer) sources))
         (last-source-index (1- (length sources))))
    ;; TODO: Factor out property printing.
    (flet ((source->html (source)
             (markup:markup
              (:div :class "source"
                    (:div :class "source-name" (:span :class "source-glyph" "⛯") (prompter:name source))
                    (:table :class "source-content"
                            (:tr
                             (loop for property in (prompter:active-properties source)
                                   ;; TODO: If we turn properties to strings, no need to capitalize.
                                   collect (markup:markup (:th (str:capitalize property)))))
                            (loop ;; TODO: Only print as many lines as fit the height.  But how can we know in advance?
                                  ;; Maybe first make the table, then add the element one by one _if_ there are into view.
                                  with max-suggestion-count = 10
                                  repeat max-suggestion-count
                                  with cursor-index = (prompter:selected-suggestion-position prompt-buffer)
                                  for suggestion-index from (max 0 (- cursor-index (/ max-suggestion-count 2)))
                                  for suggestion in (nthcdr suggestion-index (prompter:suggestions source))
                                  collect (markup:markup
                                           (:tr :id (when (equal (list suggestion source)
                                                                 (multiple-value-list (prompter:selected-suggestion prompt-buffer)))
                                                      "selection")
                                                :class (when (prompter:marked-p source (prompter:value suggestion))
                                                         "marked")
                                                (loop for (nil property) on (prompter:active-properties suggestion :source source) by #'cddr
                                                      collect (markup:markup (:td property)))))))))))
      (evaluate-script
       prompt-buffer
       (ps:ps
         (setf (ps:chain document (get-element-by-id "suggestions") |innerHTML|)
               (ps:lisp
                (str:join +newline+
                          (loop for i from current-source-index to last-source-index
                                for source = (nth i sources)
                                collect (source->html source))))))))

    (let* ((source (current-source prompt-buffer))
           (suggestions (prompter:suggestions source))
           (marks (prompter:marks source)))
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
                  (marks
                   (format nil "[~a/~a]"
                           (length marks)
                           (length suggestions)))
                  ((and (not marks)
                        (prompter:multi-selection-p source))
                   (format nil "[0/~a]"
                           (length suggestions)))
                  ((not marks)
                   (format nil "[~a]"
                           (length suggestions)))))))))))

(defmethod update-display ((prompt-buffer prompt-buffer)) ; TODO: Merge into `show'?
  ;; TODO: Finish me!
  (ffi-prompt-buffer-evaluate-javascript-async ; TODO: Replace with `evaluate-script'?  Rename the latter?
   (current-window)
   (ps:ps (ps:chain document
                    (write (ps:lisp (str:concat (generate-prompt-html prompt-buffer)))))))
  ;; TODO: The following is supposed to focus on the HTML input but does not work.
  (ffi-prompt-buffer-evaluate-javascript-async
   (current-window)
   (ps:ps (ps:chain document
                    (get-element-by-id "input")
                    (focus))))
  (update-suggestion-html prompt-buffer))

(defun watch-prompt (prompt-buffer)
  "This blocks and updates the view."
  ;; TODO: Stop loop when prompt-buffer is no longer current.
  (sera:nlet maybe-update-view ((next-source (prompter:next-ready-p prompt-buffer)))
    (cond
      ;; Nothing to do:
      ((eq t next-source) t)
      ((null next-source) nil)
      (t ;; At least one source got updated.
       (update-suggestion-html prompt-buffer)
       (maybe-update-view (prompter:next-ready-p prompt-buffer))))))

(defun set-prompt-input (prompt-buffer input)
  "Set prompter's INPUT in PROMPT-BUFFER."
  (setf (prompter:input prompt-buffer)
        input))

(defun set-prompt-buffer-input (input)
  "Set HTML INPUT in PROMPT-BUFFER."
  (when (first (active-prompt-buffers (current-window)))
    (ffi-prompt-buffer-evaluate-javascript
     (current-window)
     (ps:ps
       (setf (ps:chain document (get-element-by-id "input") value)
             (ps:lisp input))))))

(export-always 'prompt)
(sera:eval-always
  (define-function prompt (append
                           '(&rest args)
                           `(&key ,@(append
                                     (public-initargs 'prompt-buffer)
                                     (public-initargs 'prompter:prompter))))
      "Open the prompt buffer, ready for user input.
PROMPTER and PROMPT-BUFFER are plists of keyword arguments passed to the
prompt-buffer constructor.

Example use:

\(prompt
  :prompt \"Test prompt\"
  :sources (list (make-instance 'prompter:source :filter #'my-suggestion-filter)))

See the documentation of `prompt-buffer' to know more about the options."
    (let ((result-channel (make-channel 1))
          (interrupt-channel (make-channel 1)))
      (ffi-within-renderer-thread
       *browser*
       (lambda ()
         (let ((prompt-buffer (apply #'make-instance 'user-prompt-buffer
                                     (append args
                                             (list
                                              :window (current-window)
                                              :result-channel result-channel
                                              :interrupt-channel interrupt-channel)))))
           (show-prompt-buffer prompt-buffer))))
      ;; Wait until it's destroyed and get the selections from `return-selection'.
      (calispel:fair-alt
        ((calispel:? result-channel results)
         results)
        ((calispel:? interrupt-channel)
         (error 'nyxt-prompt-buffer-canceled))))))

(defmethod prompter:object-properties ((prompt-buffer prompt-buffer))
  (list :prompt (prompter:prompt prompt-buffer)
        :input (prompter:input prompt-buffer)))

(define-class resume-prompt-source (prompter:source)
  ((prompter:name "Resume prompters")
   (prompter:constructor (old-prompt-buffers *browser*))
   ;; TODO: Remove duplicates.
   ;; TODO: History?
   ))

(define-command resume-prompt ()
  "Query an older prompt and resume it."
  (let ((old-prompt
          (first (prompt
                  :resumable-p nil
                  :prompt "Resume prompt session"
                  :sources (list (make-instance 'resume-prompt-source))))))
    (when old-prompt
      (prompter:resume old-prompt)
      (show-prompt-buffer old-prompt))))
