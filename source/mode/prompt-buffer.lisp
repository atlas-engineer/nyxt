;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/prompt-buffer-mode)
(use-nyxt-package-nicknames)

(define-mode prompt-buffer-mode ()
  "The prompt buffer is the where all the interactions between Nyxt and the user happen.
It shows a list of suggestions which is filtered and narrowed down live while
the user types.

Many prompter-buffer-specific commands are available; you can list them with
`run-prompt-buffer-command', bound to \"f1 b\" by default.

The prompt buffer can have multiple 'sources' of suggestions.  Each source has
its own properties, such as the ability to mark multiple suggestions.
A same source can be used by different prompt buffers.

Each source offers a set of 'actions' for its selection(s).
Actions can be listed and run with `return-selection-over-action' (bound to
\"M-return\" by default)."
  ((keymap-scheme
    (define-scheme "prompt-buffer"
      scheme:cua
      (list
       "escape" 'cancel-input
       "down" 'select-next
       "up" 'select-previous
       "button5" 'select-next
       "button4" 'select-previous
       "home" 'select-first
       "end" 'select-last
       "pagehome" 'select-first
       "pageend" 'select-last
       "C-up" 'select-first
       "C-down" 'select-last
       "C-pagedown" 'select-next-source
       "C-pageup" 'select-previous-source
       "tab" 'insert-selection
       "return" 'return-selection
       "M-return" 'return-selection-over-action
       "C-return" 'run-follow-mode-function
       "f1 b" 'run-prompt-buffer-command
       "f1 m" 'describe-prompt-buffer
       "C-c C-f" 'toggle-follow         ; TODO: This is the Emacs Helm binding.  Better?
       "C-]" 'toggle-attributes-display ; TODO: This is the Emacs Helm binding.  Better?
       "C-space" 'toggle-mark
       "shift-space" 'toggle-mark-backwards
       "M-space" 'toggle-mark
       "M-a" 'mark-all
       "M-u" 'unmark-all
       "M-m" 'toggle-mark-all
       "C-w" 'copy-selection
       "C-v" 'paste
       "M-h" 'history)

      scheme:emacs
      (list
       "C-y" 'paste
       "C-n" 'select-next
       "C-p" 'select-previous
       "M-n" 'select-next-page
       "M-p" 'select-previous-page
       "M->" 'select-last
       "M-<" 'select-first
       "M-]" 'select-next-source        ; Emacs Helm binding.
       "M-[" 'select-previous-source    ; Emacs Helm binding.
       ;; Those two are only bound in Emacs mode. CUA, VI?
       "C-M-n" 'scroll-other-buffer-down
       "C-M-p" 'scroll-other-buffer-up
       "C-j" 'run-follow-mode-function
       "C-g" 'cancel-input
       "C-h b" 'run-prompt-buffer-command
       "C-e" 'move-end-of-input
       "C-a" 'move-start-of-input
       "C-f" 'cursor-forwards
       "C-b" 'cursor-backwards
       "M-f" 'cursor-forwards-word
       "M-b" 'cursor-backwards-word
       "C-x h" 'select-all)

      scheme:vi-normal
      (list
       "j" 'select-next
       "k" 'select-previous
       ;; C-j and C-k are useful in insert mode since "j", "k" are taken.
       ;; We bind C-j and C-k in normal mode for consistency between the two modes.
       "C-j" 'select-next
       "C-k" 'select-previous
       "C-f" 'select-next-page
       "C-b" 'select-previous-page
       "G" 'select-last
       "g g" 'select-first
       "J" 'select-next-source
       "K" 'select-previous-source
       ;; Same as with C-j.
       "C-J" 'select-next-source
       "C-K" 'select-previous-source
       "z f" 'toggle-follow
       "z a" 'toggle-attributes-display
       "y" 'copy-selection
       "p" 'paste
       "$" 'move-end-of-input
       "^" 'move-start-of-input
       "l" 'cursor-forwards
       "h" 'cursor-backwards
       "w" 'cursor-forwards-word
       "b" 'cursor-backwards-word)

      scheme:vi-insert
      (list
       "C-j" 'select-next
       "C-k" 'select-previous
       "C-J" 'select-next-source
       "C-K" 'select-previous-source
       "C-f" 'select-next-page
       "C-b" 'select-previous-page)))))

(defmacro define-command-prompt (name (prompt-buffer &rest arglist) &body body)
  "Like `define-command' but the first argument is special:
- it is considered a keyword argument if `&keyword' is in arglist, `&optional' otherwise,
- it is bound to `(current-prompt-buffer)` if unspecified,
- the body is skipped and a warning is emitted when it's nil."
  (multiple-value-bind (forms declares documentation)
      (alex:parse-body body :documentation t)
    (multiple-value-bind (required optional rest keywords aok? aux key?)
        (alex:parse-ordinary-lambda-list arglist)
      (flet ((unparse-arguments (prompt-buffer-sym)
               (if keywords
                   (push `((,(intern (string prompt-buffer-sym) "KEYWORD") ,prompt-buffer-sym) (current-prompt-buffer) nil)
                         keywords)
                   (push `(,prompt-buffer-sym (current-prompt-buffer) nil)
                         optional))
               (sera:unparse-ordinary-lambda-list required optional rest keywords aok? aux key?)))
        `(define-command ,name ,(unparse-arguments prompt-buffer)
           ,@(sera:unsplice documentation)
           ,@declares
           (if prompt-buffer
               (progn ,@forms)
               (log:warn "Can't call ~a on nil prompt buffer" ',name)))))))

(define-command-prompt select-next (prompt-buffer)
  "Select next entry in prompt buffer."
  (prompter:select-next prompt-buffer)
  ;; TODO: Update display?  The library should probably decide when to update
  ;; it.  Drawback is that it maybe result in too many draws.  If the caller
  ;; decides when redraw, it has more control.
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-previous (prompt-buffer)
  "Select next entry in prompt buffer."
  (prompter:select-previous prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-first (prompt-buffer)
  "Select first entry in prompt buffer."
  (prompter:select-first prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-last (prompt-buffer)
  "Select first entry in prompt buffer."
  (prompter:select-last prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-next-source (prompt-buffer)
  "Select next soruce in prompt buffer."
  (prompter:select-next-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-previous-source (prompt-buffer)
  "Select previous source in prompt buffer."
  (prompter:select-previous-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-next-page (prompt-buffer &key (steps 1))
  "Select entry by STEPS next page in prompt buffer.
If STEPS is negative, go to previous pages instead."
  (unless (= 0 steps)
    (let ((step-page-index              ; TODO: Add multi-source support.
            (ffi-buffer-evaluate-javascript
             prompt-buffer
             (ps:ps
               (defun step-row (row)
                 (ps:chain
                  (aref (ps:chain row parent-node rows)
                        (max 0
                             (min (- (ps:chain row parent-node rows length) 1)
                                  (+ (if (< 0 (ps:lisp steps)) 1 -1)
                                     (ps:chain row row-index)))))))
               (defun find-first-element-out-of-view (row)
                 (if (nyxt/ps:element-in-view-port-p row)
                     (let ((new-row (step-row row)))
                       (if (eq new-row row)
                           row
                           (find-first-element-out-of-view new-row)))
                     row))
               (-
                (ps:chain
                 (find-first-element-out-of-view (ps:chain document (get-element-by-id "selection")))
                 row-index)
                (ps:chain (ps:chain document (get-element-by-id "selection")) row-index))))))
      (sera:and-let* ((index-diff step-page-index))
        (prompter:select-next prompt-buffer
                              index-diff)))
    ;; TODO: Update display?  The library should probably decide when to update
    ;; it.  Drawback is that it maybe result in too many draws.  If the caller
    ;; decides when redraw, it has more control.
    (prompt-render-suggestions prompt-buffer)))

(define-command-prompt select-previous-page (prompt-buffer &key (steps 1))
  "Select entry by STEPS previous page in prompt buffer.
If STEPS is negative, go to next pages instead."
  (select-next-page :prompt-buffer prompt-buffer :steps (- steps)))

(define-command-prompt return-selection (prompt-buffer)
  "Have the PROMT-BUFFER return the selection, then quit."
  (prompter:return-selection prompt-buffer))

(defun make-attribute-suggestion (attribute &optional source input)
  "Return a `suggestion' wrapping around ATTRIBUTE. "
  (declare (ignore source input))
  (make-instance 'prompter:suggestion
                 :value attribute
                 :attributes `(("Attribute key" ,attribute))))

(define-class attribute-source (prompter:source)
  ((prompter:name "List of prompter attributes")
   (prompter:multi-selection-p t)
   (prompter:suggestion-maker 'make-attribute-suggestion)
   (prompter:actions '(return-marks-only))))

(defun return-marks-only (suggestion-values)
  "Return marked suggestions only.
They are returned untouched.
This is useful for prompters where we want either marks or nothing, but not the
current unmarked selection."
  (multiple-value-bind (suggestion source)
      (prompter:selected-suggestion (current-prompt-buffer))
    (if (and (typep source 'attribute-source)
             (not (prompter:marks source)))
        (remove (prompter:value suggestion) suggestion-values
                :test #'equal)
        suggestion-values)))

(define-command-prompt toggle-attributes-display (prompt-buffer)
  "Prompt for which prompter attributes to display."
  (let ((attributes (prompt
                     :prompt "Mark attributes to display"
                     :sources (list (make-instance 'attribute-source
                                                   :marks (intersection
                                                           (prompter:active-attributes-keys (current-source prompt-buffer))
                                                           (prompter:attributes-keys-non-default
                                                            (current-source prompt-buffer))
                                                           :test #'string=)
                                                   :constructor (prompter:attributes-keys-non-default
                                                                 (current-source prompt-buffer)))))))
    (setf (prompter:active-attributes-keys (current-source prompt-buffer))
          attributes)
    (prompt-render-suggestions prompt-buffer)))

(define-class prompt-buffer-command-source (command-source)
  ((prompter:name "Prompt buffer commands")
   (parent-prompt-buffer (error "Parent prompt buffer required"))
   (global-p nil)
   (buffer (current-prompt-buffer))
   (prompter:suggestion-maker 'make-prompt-buffer-command-suggestion))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defun make-prompt-buffer-command-suggestion (command source)
  "Return a `suggestion' wrapping around COMMAND."
  (make-instance
   'prompter:suggestion
   :value command
   :attributes (nyxt::command-attributes command (parent-prompt-buffer source))))

(define-command-prompt run-prompt-buffer-command (prompt-buffer)
  "Prompt for a command to call in PROMPT-BUFFER."
  (let ((command (prompt1
                   :prompt "Command to run in current prompt buffer"
                   :sources (list (make-instance 'prompt-buffer-command-source
                                                 :parent-prompt-buffer prompt-buffer)))))
    (funcall* command)))

(defun prompt-buffer-actions (&optional (window (current-window)))
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    (prompter:actions first-prompt-buffer)))

;; TODO: Should actions be commands?  For now, they can be either commands or symbols.
(defun make-action-suggestion (action &optional source input)
  "Return a `suggestion' wrapping around ACTION."
  (declare (ignore source input))
  (make-instance
   'prompter:suggestion
   :value action
   ;; TODO: Include bindings in attributes.
   :attributes `(("Name" ,(symbol-name (typecase action
                                         (command (name action))
                                         (t action))))
                 ("Documentation" ,(or (first (sera:lines
                                               (typecase action
                                                 (command (nyxt::docstring action))
                                                 (t (documentation action 'function)))))
                                       "")))))

(define-class action-source (prompter:source)
  ((prompter:name "List of actions")
   (prompter:constructor (prompt-buffer-actions))
   (prompter:suggestion-maker 'make-action-suggestion)))

(define-command-prompt return-selection-over-action (prompt-buffer)
  "Prompt for an action to run over PROMPT-BUFFER selection."
  (let ((action (prompt1
                 :prompt "Action to run on selection"
                 :sources (list (make-instance 'action-source)))))
    (when action
      (prompter:return-selection prompt-buffer action))))

(define-command-prompt run-follow-mode-function (prompt-buffer)
  "Run follow-mode function over selected suggestion without closing PROMPT-BUFFER."
  (prompter:call-follow-mode-function prompt-buffer))

(define-command-prompt cancel-input (prompt-buffer) ; TODO: Rename.
  "Close the prompt-buffer without further action."
  (prompter:destroy prompt-buffer))

(define-command-prompt toggle-follow (prompt-buffer)
  "Close the prompt-buffer without further action."
  (prompter:toggle-follow prompt-buffer))

(define-command-prompt toggle-mark (prompt-buffer &key (direction :forward))
  "Mark selection.
Only available if current prompt-buffer source `multi-selection-p' is non-nil.
DIRECTION can be `:forward' or `:backward' and specifies which suggestion to
select next."
  (prompter:toggle-mark prompt-buffer)
  (match direction
    (:forward (select-next prompt-buffer))
    (:backward (select-previous prompt-buffer))))

(define-command-prompt toggle-mark-backwards (prompt-buffer)
  "Mark selection.
Only available if pomrpt-buffer `multi-selection-p' is non-nil.  DIRECTION can be
`:forward' or `:backward' and specifies which suggestion to select next."
  (toggle-mark :prompt-buffer prompt-buffer
               :direction :backward))

(define-command-prompt mark-all (prompt-buffer)
  "Mark all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (prompter:mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt unmark-all (prompt-buffer)
  "Unmark all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (prompter:unmark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt toggle-mark-all (prompt-buffer)
  "Toggle the mark over all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (prompter:toggle-mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt copy-selection (prompt-buffer)
  "Copy default property of selection to clipboard."
  (let* ((marks (prompter:all-marks prompt-buffer))
         (props (if marks
                    (mapcar #'prompter:attributes-default marks)
                    (list (prompter:attributes-default (prompter:selected-suggestion
                                                       prompt-buffer)))))
         ;; Reverse so that text is ordered from oldest mark to newest.
         (text (str:join +newline+ (reverse props))))
    (unless (str:emptyp text)
      (trivial-clipboard:text text)
      (echo "Copied ~s to clipboard." text))))

(define-command-prompt paste (prompt-buffer)
  "Paste clipboard text to input."
  (ffi-buffer-paste prompt-buffer)
  (nyxt::update-prompt-input prompt-buffer))

(defun history-entries (&optional (window (current-window)))
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    (containers:container->list
     (prompter:history first-prompt-buffer))))

(define-class prompt-buffer-history-source (prompter:source)
  ((prompter:name "Prompt buffer input history")
   (prompter:constructor (history-entries))))

(define-command-prompt history (prompt-buffer)
  "Choose a prompt-buffer input history entry to insert as input."
  (let ((history (prompter:history prompt-buffer)))
    (if (and history (not (containers:empty-p history)))
        (let ((input (prompt1
                      :prompt "Input history"
                      :sources (list (make-instance 'prompt-buffer-history-source)))))
          (unless (str:empty? input)
            (nyxt::set-prompt-buffer-input input)))
        (echo "Prompt buffer has no history."))))

(define-command-prompt insert-selection (prompt-buffer)
  "Insert current selection default property in the prompt buffer input."
  (alex:when-let ((selection (prompter:attributes-default
                              (prompter:selected-suggestion prompt-buffer))))
    (nyxt::set-prompt-buffer-input selection)))

(define-command-prompt move-start-of-input (prompt-buffer)
  "Move to the beginning of PROMPT-BUFFER input."
  (ffi-buffer-evaluate-javascript
   prompt-buffer
   (ps:ps
     (let ((input (ps:chain document (get-element-by-id "input"))))
       (setf (ps:@ input selection-start) 0
             (ps:@ input selection-end) 0)))))

(define-command-prompt move-end-of-input (prompt-buffer)
  "Move to the end of PROMPT-BUFFER input."
  (ffi-buffer-evaluate-javascript
   prompt-buffer
   (ps:ps
     (let ((input (ps:chain document (get-element-by-id "input"))))
       (setf (ps:@ input selection-start) (ps:@ input value length)
             (ps:@ input selection-end) (ps:@ input value length))))))

(define-command-prompt cursor-forwards (prompt-buffer)
  "Move cursor forward by one character."
  (with-current-buffer prompt-buffer
    (nyxt/input-edit-mode:cursor-forwards)))

(define-command-prompt cursor-backwards (prompt-buffer)
  "Move cursor backwards by one character."
  (with-current-buffer prompt-buffer
    (nyxt/input-edit-mode:cursor-backwards)))

(define-command-prompt cursor-forwards-word (prompt-buffer)
  "Move cursor forward by one word."
  (with-current-buffer prompt-buffer
    (nyxt/input-edit-mode:cursor-forwards-word)))

(define-command-prompt cursor-backwards-word (prompt-buffer)
  "Move cursor backwards by one word."
  (with-current-buffer prompt-buffer
    (nyxt/input-edit-mode:cursor-backwards-word)))

(define-command-prompt select-all (prompt-buffer)
  "Select all the text in the prompt input."
  (ffi-buffer-select-all prompt-buffer))

(define-command-prompt scroll-other-buffer-up (prompt-buffer
                                               &key (scroll-distance
                                                     (scroll-distance (current-buffer))))
  "Scroll the buffer behind the prompt up."
  (with-current-buffer (current-buffer)
    ;; FIXME: Copy-paste from scroll.lisp. Move it somewhere prompt-buffer.lisp can reach it?
    (pflet ((scroll-up ()
            (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance))))))
    (scroll-up))))

(define-command-prompt scroll-other-buffer-down (prompt-buffer
                                                 &key (scroll-distance
                                                       (scroll-distance (current-buffer))))
  "Scroll the buffer behind the prompt down."
  (with-current-buffer (current-buffer)
    ;; FIXME: Copy-paste from scroll.lisp. Move it somewhere prompt-buffer.lisp can reach it?
    (pflet ((scroll-down ()
            (ps:chain window (scroll-by 0 (ps:lisp scroll-distance)))))
    (scroll-down))))
