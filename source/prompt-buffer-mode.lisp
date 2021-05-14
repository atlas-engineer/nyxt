;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/prompt-buffer-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

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
       "tab" 'prompt-buffer-insert-selection
       "return" 'return-selection
       "M-return" 'return-selection-over-action
       "C-return" 'run-follow-mode-function
       "f1 b" 'run-prompt-buffer-command
       "f1 m" 'describe-prompt-buffer
       "C-c C-f" 'toggle-follow         ; TODO: This is the Emacs Helm binding.  Better?
       "C-]" 'toggle-attributes-display ; TODO: This is the Emacs Helm binding.  Better?
       "C-space" 'prompt-buffer-toggle-mark
       "shift-space" 'prompt-buffer-toggle-mark-backwards
       "M-space" 'prompt-buffer-toggle-mark
       "M-a" 'prompt-buffer-mark-all
       "M-u" 'prompt-buffer-unmark-all
       "M-m" 'prompt-buffer-toggle-mark-all
       "C-w" 'copy-selection
       "C-v" 'prompt-buffer-paste
       "M-h" 'prompt-buffer-history)

      scheme:emacs
      (list
       "C-n" 'select-next
       "C-p" 'select-previous
       "M-n" 'select-next-page
       "M-p" 'select-previous-page
       "M->" 'select-last
       "M-<" 'select-first
       "M-]" 'select-next-source        ; Emacs Helm binding.
       "M-[" 'select-previous-source    ; Emacs Helm binding.
       "C-j" 'run-follow-mode-function
       "C-g" 'cancel-input
       "C-h b" 'run-prompt-buffer-command)

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
       "p" 'prompt-buffer-paste)

      scheme:vi-insert
      (list
       "C-j" 'select-next
       "C-k" 'select-previous
       "C-J" 'select-next-source
       "C-K" 'select-previous-source
       "C-f" 'select-next-page
       "C-b" 'select-previous-page)))))

(define-command select-next (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next entry in prompt buffer."
  (prompter:select-next prompt-buffer)
  ;; TODO: Update display?  The library should probably decide when to update
  ;; it.  Drawback is that it maybe result in too many draws.  If the caller
  ;; decides when redraw, it has more control.
  (prompt-render-suggestions prompt-buffer))

(define-command select-previous (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next entry in prompt buffer."
  (prompter:select-previous prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command select-first (&optional (prompt-buffer (current-prompt-buffer)))
  "Select first entry in prompt buffer."
  (prompter:select-first prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command select-last (&optional (prompt-buffer (current-prompt-buffer)))
  "Select first entry in prompt buffer."
  (prompter:select-last prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command select-next-source (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next soruce in prompt buffer."
  (prompter:select-next-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command select-previous-source (&optional (prompt-buffer (current-prompt-buffer)))
  "Select previous source in prompt buffer."
  (prompter:select-previous-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command select-next-page (&key (prompt-buffer (current-prompt-buffer))
                                  (steps 1))
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
      (sera:and-let* ((index-diff (parse-integer step-page-index :junk-allowed t)))
        (prompter:select-next prompt-buffer
                              index-diff)))
    ;; TODO: Update display?  The library should probably decide when to update
    ;; it.  Drawback is that it maybe result in too many draws.  If the caller
    ;; decides when redraw, it has more control.
    (prompt-render-suggestions prompt-buffer)))

(define-command select-previous-page (&key (prompt-buffer (current-prompt-buffer))
                                      (steps 1))
  "Select entry by STEPS previous page in prompt buffer.
If STEPS is negative, go to next pages instead."
  (select-next-page :prompt-buffer prompt-buffer :steps (- steps)))

(define-command return-selection (&optional (prompt-buffer (current-prompt-buffer)))
  "Have the PROMT-BUFFER return the selection, then quit."
  (prompter:return-selection prompt-buffer))

(defun make-attribute-suggestion (attribute &optional source input)
  "Return a `suggestion' wrapping around ATTRIBUTE. "
  (declare (ignore source input))
  (make-instance 'prompter:suggestion
                 :value attribute
                 :attributes `(("Attribute key" ,attribute))))

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

(define-class attribute-source (prompter:source)
  ((prompter:name "List of prompter attributes")
   (prompter:multi-selection-p t)
   (prompter:suggestion-maker 'make-attribute-suggestion)
   (prompter:actions '(return-marks-only))))

(define-command toggle-attributes-display (&optional (prompt-buffer (current-prompt-buffer)))
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

(define-class prompt-buffer-command-source (prompter:source)
  ((prompter:name "List of prompt buffer commands")
   (parent-prompt-buffer (error "Parent prompt buffer required"))
   (prompter:suggestion-maker 'make-prompt-buffer-command-suggestion)
   (prompter:constructor (nyxt::get-commands (current-prompt-buffer))))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defun make-prompt-buffer-command-suggestion (command source)
  "Return a `suggestion' wrapping around COMMAND."
  (make-instance
   'prompter:suggestion
   :value command
   :attributes (nyxt::command-attributes command (parent-prompt-buffer source))))

(define-command run-prompt-buffer-command (&optional (prompt-buffer (current-prompt-buffer)))
  "Prompt for a command to call in PROMPT-BUFFER."
  (let ((command (first (prompt
                         :prompt "Command to run in current prompt buffer"
                         :sources (list (make-instance 'prompt-buffer-command-source
                                                       :parent-prompt-buffer prompt-buffer))))))
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

(define-command return-selection-over-action (&optional (prompt-buffer (current-prompt-buffer)))
  "Prompt for an action to run over PROMPT-BUFFER selection."
  (let ((action (first (prompt
                        :prompt "Action to run on selection"
                        :sources (list (make-instance 'action-source))))))
    (when action
      (prompter:return-selection prompt-buffer action))))

(define-command run-follow-mode-function (&optional (prompt-buffer (current-prompt-buffer)))
  "Run follow-mode function over selected suggestion without closing PROMPT-BUFFER."
  (prompter:call-follow-mode-function prompt-buffer))

(define-command cancel-input (&optional (prompt-buffer (current-prompt-buffer))) ; TODO: Rename.
  "Close the prompt-buffer without further action."
  (hide-prompt-buffer prompt-buffer))

(define-command toggle-follow (&optional (prompt-buffer (current-prompt-buffer)))
  "Close the prompt-buffer without further action."
  (prompter:toggle-follow prompt-buffer))

; TODO: Remove the "prompt-buffer-" prefix of all prompt-buffer-mode commands.
(define-command prompt-buffer-toggle-mark (&key
                                           (prompt-buffer (current-prompt-buffer))
                                           (direction :forward))
  "Mark selection.
Only available if current prompt-buffer source `multi-selection-p' is non-nil.
DIRECTION can be `:forward' or `:backward' and specifies which suggestion to
select next."
  (prompter:toggle-mark prompt-buffer)
  (match direction
    (:forward (select-next prompt-buffer))
    (:backward (select-previous prompt-buffer))))

(define-command prompt-buffer-toggle-mark-backwards (&key
                                                     (prompt-buffer (current-prompt-buffer)))
  "Mark selection.
Only available if pomrpt-buffer `multi-selection-p' is non-nil.  DIRECTION can be
`:forward' or `:backward' and specifies which suggestion to select next."
  (prompt-buffer-toggle-mark :prompt-buffer prompt-buffer
                             :direction :backward))

(define-command prompt-buffer-mark-all (&optional (prompt-buffer (current-prompt-buffer)))
  "Mark all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (prompter:mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command prompt-buffer-unmark-all (&optional (prompt-buffer (current-prompt-buffer)))
  "Unmark all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (prompter:unmark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command prompt-buffer-toggle-mark-all (&optional
                                               (prompt-buffer (current-prompt-buffer)))
  "Toggle the mark over all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (prompter:toggle-mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command copy-selection (&optional (prompt-buffer (current-prompt-buffer)))
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

(define-command prompt-buffer-paste ()
  "Paste clipboard text to input."
  (ffi-buffer-evaluate-javascript
   (current-prompt-buffer)
   (ps:ps
     (nyxt/ps:insert-at (ps:chain document (get-element-by-id "input"))
                        (ps:lisp (ring-insert-clipboard (nyxt::clipboard-ring *browser*)))))))

(defun prompt-buffer-history-entries (&optional (window (current-window)))
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    (containers:container->list
     (prompter:history first-prompt-buffer))))

(define-class prompt-buffer-history-source (prompter:source)
  ((prompter:name "Prompt buffer input history")
   (prompter:constructor (prompt-buffer-history-entries))))

(define-command prompt-buffer-history (&optional (prompt-buffer (current-prompt-buffer)))
  "Choose a prompt-buffer input history entry to insert as input."
  (let ((history (prompter:history prompt-buffer)))
    (if (and history (not (containers:empty-p history)))
        (let ((input (first (prompt
                             :prompt "Input history"
                             :sources (list (make-instance 'prompt-buffer-history-source))))))
          (unless (str:empty? input)
            (nyxt::set-prompt-buffer-input input)))
        (echo "Prompt buffer has no history."))))

(define-command prompt-buffer-insert-selection (&optional (prompt-buffer (current-prompt-buffer)))
  "Insert current selection default property in the prompt buffer input."
  (alex:when-let ((selection (prompter:attributes-default
                              (prompter:selected-suggestion prompt-buffer))))
    (nyxt::set-prompt-buffer-input selection)))
