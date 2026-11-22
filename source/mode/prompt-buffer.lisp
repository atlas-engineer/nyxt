;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/prompt-buffer
  (:documentation "Package for `prompt-buffer-mode' to interact with the
`prompt-buffer'."))
(in-package :nyxt/mode/prompt-buffer)

(define-mode prompt-buffer-mode ()
  "The prompt buffer is where all interactions between Nyxt and the user take
place.  It displays a list of suggestions which are filtered as the user types.

Many prompter-buffer-specific commands are available; you can list them with
`run-prompt-buffer-command', bound to \"f1 b\" by default.

The prompt buffer can have multiple `prompter:source's of suggestions.  Each
source has its own properties, such as the ability to mark multiple suggestions.
The same source can be used by different prompt buffers.

Each source offers a set of `actions-on-return' for marked items.  These can be
listed and chosen from with the command `set-action-on-return' (bound to
\"M-return\" by default)."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "prompt-buffer-mode" ()
      keyscheme:default
      (list
       "M-up" 'expand
       "M-down" 'contract
       "up" 'previous-suggestion
       "button4" 'previous-suggestion
       "down" 'next-suggestion
       "button5" 'next-suggestion
       "shift-home" 'first-suggestion
       "pagehome" 'first-suggestion
       "shift-end" 'last-suggestion
       "pageend" 'last-suggestion
       "pageup" 'previous-page
       "pagedown" 'next-page
       "shift-up" 'previous-source
       "shift-down" 'next-source
       "shift-left" 'first-suggestion-within-source
       "shift-right" 'last-suggestion-within-source
       "M-o" 'toggle-prompt-buffer-focus
       "escape" 'quit-prompt-buffer
       "M-a" 'mark-all
       "M-u" 'unmark-all
       "C-space" 'toggle-mark-forwards
       "M-space" 'toggle-mark-forwards
       "shift-space" 'toggle-mark-backwards
       "M-shift-space" 'toggle-mark-backwards
       "M-m" 'toggle-mark-all
       "M-h" 'history
       "f1 b" 'run-prompt-buffer-command
       "f1 m" 'describe-prompt-buffer
       "return" 'run-action-on-return
       "keypadenter" 'run-action-on-return
       "M-return" 'set-action-on-return
       "M-keypadenter" 'set-action-on-return
       "C-return" 'toggle-mark-forwards
       "C-keypadenter" 'toggle-mark-forwards
       "shift-return" 'toggle-mark-forwards
       "shift-keypadenter" 'toggle-mark-forwards
       "tab" 'insert-current-suggestion
       "C-c C-f" 'toggle-actions-on-current-suggestion
       "C-]" 'toggle-attributes-display)
      keyscheme:cua
      (list
       "C-up" 'first-suggestion
       "C-shift-up" 'first-suggestion-within-source
       "C-down" 'last-suggestion
       "C-shift-down" 'last-suggestion-within-source
       "C-v" 'paste
       "C-x" 'cut
       "C-a" 'select-input-text)
      #+darwin keyscheme:cua
      #+darwin
      (list
       "S-up" 'first-suggestion
       "S-shift-up" 'first-suggestion-within-source
       "S-down" 'last-suggestion
       "S-shift-down" 'last-suggestion-within-source
       "S-c" 'nyxt/mode/document:copy
       "S-v" 'paste
       "S-x" 'cut
       "S-a" 'select-input-text
       "S-z" 'nyxt/mode/document:undo
       "S-Z" 'nyxt/mode/document:redo)
      keyscheme:emacs
      (list
       "C-p" 'previous-suggestion
       "C-n" 'next-suggestion
       "M-<" 'first-suggestion
       "M-," 'first-suggestion-within-source
       "M->" 'last-suggestion
       "M-." 'last-suggestion-within-source
       "C-x o" 'toggle-prompt-buffer-focus
       "M-v" 'previous-page
       "C-v" 'next-page
       "M-p" 'previous-source
       "M-n" 'next-source
       "M-[" 'previous-source
       "M-]" 'next-source
       "C-M-n" 'scroll-other-buffer-down
       "C-M-p" 'scroll-other-buffer-up
       "C-M-v" 'scroll-page-down-other-buffer
       "C-M-V" 'scroll-page-up-other-buffer
       "C-g" 'quit-prompt-buffer
       "C-e" 'move-end-of-input
       "C-a" 'move-start-of-input
       "C-b" 'nyxt/mode/input-edit:cursor-backwards
       "C-f" 'nyxt/mode/input-edit:cursor-forwards
       "C-d" 'nyxt/mode/input-edit:delete-forwards
       "M-b" 'nyxt/mode/input-edit:cursor-backwards-word
       "M-f" 'nyxt/mode/input-edit:cursor-forwards-word
       "C-backspace" 'nyxt/mode/input-edit:delete-backwards-word
       "M-backspace" 'nyxt/mode/input-edit:delete-backwards-word
       "M-d" 'nyxt/mode/input-edit:delete-forwards-word
       "C-x h" 'select-input-text
       "M-w" 'copy-selection
       "C-y" 'paste
       "C-w" 'cut
       "C-h m" 'describe-prompt-buffer
       "C-h b" 'run-prompt-buffer-command)
      keyscheme:vi-normal
      (list
       "k" 'previous-suggestion
       "j" 'next-suggestion
       "C-k" 'previous-suggestion
       "C-j" 'next-suggestion
       "g g" 'first-suggestion
       "G" 'last-suggestion
       "C-b" 'previous-page
       "C-f" 'next-page
       "K" 'previous-source
       "J" 'next-source
       "C-K" 'previous-source
       "C-J" 'next-source
       "M-j" 'scroll-other-buffer-down
       "M-k" 'scroll-other-buffer-up
       "C-M-j" 'scroll-page-down-other-buffer
       "C-M-k" 'scroll-page-up-other-buffer
       "$" 'move-end-of-input
       "^" 'move-start-of-input
       "l" 'nyxt/mode/input-edit:cursor-forwards
       "h" 'nyxt/mode/input-edit:cursor-backwards
       "w" 'nyxt/mode/input-edit:cursor-forwards-word
       "b" 'nyxt/mode/input-edit:cursor-backwards-word
       "x" 'nyxt/mode/input-edit:delete-forwards
       "d b" 'nyxt/mode/input-edit:delete-backwards-word
       "d w" 'nyxt/mode/input-edit:delete-forwards-word
       "z f" 'toggle-actions-on-current-suggestion
       "z a" 'toggle-attributes-display
       "y" 'copy-selection
       "p" 'paste
       "d d" 'cut)
      keyscheme:vi-insert
      (list
       "return" 'run-action-on-return
       "keypadenter" 'run-action-on-return
       "M-return" 'set-action-on-return
       "M-keypadenter" 'set-action-on-return
       "C-k" 'previous-suggestion
       "C-j" 'next-suggestion
       "C-b" 'previous-page
       "C-f" 'next-page
       "C-K" 'previous-source
       "C-J" 'next-source))))
  (:toggler-command-p nil))

(export-always 'define-command-prompt)
(defmacro define-command-prompt (name (prompt-buffer &rest arglist) &body body)
  "Define a `prompt-buffer' command NAME taking PROMPT-BUFFER explicitly."
  `(define-command ,name ((,prompt-buffer prompt-buffer) ,@arglist)
     ,@body))

(define-command-prompt next-suggestion (prompt-buffer)
  "Select next entry in prompt buffer."
  (prompter:next-suggestion prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt previous-suggestion (prompt-buffer)
  "Select previous entry in PROMPT-BUFFER."
  (prompter:previous-suggestion prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt first-suggestion (prompt-buffer)
  "Select first entry in PROMPT-BUFFER."
  (prompter:first-suggestion prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt last-suggestion (prompt-buffer)
  "Select last entry in PROMPT-BUFFER."
  (prompter:last-suggestion prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt first-suggestion-within-source (prompt-buffer)
  "Select first entry in the current PROMPT-BUFFER's source."
  (let ((first-source-p (eq (prompter:current-source prompt-buffer)
                            (first (prompter:previous-source prompt-buffer)))))
    (if first-source-p
        (prompter:first-suggestion prompt-buffer)
        (prompter:next-suggestion prompt-buffer)))
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt last-suggestion-within-source (prompt-buffer)
  "Select last entry in the current PROMPT-BUFFER's source."
  (let ((last-source-p (eq (prompter:current-source prompt-buffer)
                           (first (prompter:next-source prompt-buffer)))))
    (if last-source-p
        (prompter:last-suggestion prompt-buffer)
        (prompter:previous-suggestion prompt-buffer)))
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt next-source (prompt-buffer)
  "Select next source in PROMPT-BUFFER."
  (prompter:next-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt previous-source (prompt-buffer)
  "Select previous source in PROMPT-BUFFER."
  (prompter:previous-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt next-page (prompt-buffer &key (n 1))
  "Select entry by N next pages in PROMPT-BUFFER.
If N is negative, go to previous pages instead."
  (unless (= 0 n)
    (prompter:next-suggestion prompt-buffer
                              (* n (nyxt::prompt-suggestion-window)))
    (prompt-render-suggestions prompt-buffer)))

(define-command-prompt contract (prompt-buffer &key (delta 10))
  (setf (ffi-height prompt-buffer)
        (max 56 (- (ffi-height prompt-buffer) delta))))

(define-command-prompt expand (prompt-buffer &key (delta 10))
  (incf (ffi-height prompt-buffer) delta))

(define-command-prompt previous-page (prompt-buffer &key (n 1))
  "Select entry by N previous pages in PROMPT-BUFFER.
If N is negative, go to next pages instead."
  (next-page prompt-buffer :n (- n)))

(define-command-prompt run-action-on-return (prompt-buffer)
  "Have the PROMPT-BUFFER return the marks, then quit."
  (prompter:run-action-on-return prompt-buffer))

(defun make-attribute-suggestion (attribute &optional source input)
  "Return a `suggestion' wrapped around ATTRIBUTE."
  (declare (ignore source input))
  (make-instance 'prompter:suggestion
                 :value attribute
                 :attributes `(("Attribute key" ,attribute))))

(define-class attribute-source (prompter:source)
  ((prompter:name "List of prompter attributes")
   (prompter:enable-marks-p t)
   (prompter:suggestion-maker 'make-attribute-suggestion)))

(defmethod initialize-instance :after ((source attribute-source)
                                       &key (actions-on-return nil actions-supplied-p)
                                       &allow-other-keys)
  (declare (ignore actions-on-return))
  (unless actions-supplied-p
    (setf (slot-value source 'prompter:actions-on-return)
          (list (lambda-command return-marks-only* (suggestion-values)
                  "Return marked suggestions only."
                  (when (prompter:marks source)
                    suggestion-values))))))

(define-command-prompt toggle-attributes-display
    (prompt-buffer &key (source (current-source prompt-buffer)))
  "Prompt for which prompter attributes to display.
The composite source keeps each originating source's default columns, so this
command does nothing there."
  (unless (typep source 'prompter:composite-source)
    (let ((attributes
            (prompt :prompt "Mark attributes to display"
                    :sources
                    (make-instance
                     'attribute-source
                     :marks (intersection
                             (prompter:active-attributes-keys source)
                             (prompter:attributes-keys-non-default source)
                             :test #'string=)
                     :constructor (prompter:attributes-keys-non-default source)))))
      (setf (prompter:active-attributes-keys source)
            attributes)
      (prompt-render-suggestions prompt-buffer))))

(define-class prompt-buffer-command-source (command-source)
  ((prompter:name "Prompt buffer commands")
   (parent-prompt-buffer (error "Parent prompt buffer required"))
   (global-p nil)
   (prompter:suggestion-maker 'make-prompt-buffer-command-suggestion)))

(defun make-prompt-buffer-command-suggestion (command source)
  "Return a `suggestion' wrapped around COMMAND."
  (make-instance
   'prompter:suggestion
   :value command
   :attributes (nyxt::command-attributes command (parent-prompt-buffer source))))

(define-command-prompt run-prompt-buffer-command (prompt-buffer)
  "Prompt for a command to call in PROMPT-BUFFER."
  (let ((command
          (prompt1 :prompt "Command to run in current prompt buffer"
                   :sources (make-instance
                             'prompt-buffer-command-source
                             :buffer prompt-buffer
                             :parent-prompt-buffer prompt-buffer))))
    (when command
      (dispatch-command command prompt-buffer))))

(defun prompt-buffer-actions-on-return (prompt-buffer)
  (or (and prompt-buffer (prompter:actions-on-return prompt-buffer))
      (progn
        (echo-warning "No actions to choose from.")
        (error 'prompt-buffer-canceled))))

(defun make-action-suggestion (action &optional source input)
  "Return a `suggestion' wrapped around ACTION."
  (declare (ignore source input))
  (make-instance
   'prompter:suggestion
   :value action
   :attributes `(("Name" ,(or (ignore-errors
                               (symbol-name (typecase action
                                              (command (name action))
                                              (t action))))
                              "Lambda"))
                 ("Documentation" ,(documentation-line action 'function "")))))

(define-class action-on-return-source (prompter:source)
  ((prompter:name "List of actions-on-return")
   (prompter:suggestion-maker 'make-action-suggestion)))

(define-command-prompt set-action-on-return (prompt-buffer)
  "Prompt for an action to run over PROMPT-BUFFER `prompter:marks'."
  (if (equal (mapcar #'type-of (prompter:sources prompt-buffer))
             '(action-on-return-source))
      (echo "Already displaying actions-on-return of previous prompt buffer.")
      (when-let ((action (prompt1 :prompt "Set return action to run over marks"
                                  :sources (make-instance
                                            'action-on-return-source
                                            :constructor (prompt-buffer-actions-on-return
                                                          prompt-buffer)))))
        (prompter:run-action-on-return prompt-buffer action))))

(define-command-prompt quit-prompt-buffer (prompt-buffer)
  "Close the PROMPT-BUFFER without further action."
  (prompter:destroy prompt-buffer))

(define-command-prompt toggle-actions-on-current-suggestion (prompt-buffer)
  "Toggle whether `prompter:actions-on-current-suggestion' are enabled for
PROMPT-BUFFER."
  (prompter:toggle-actions-on-current-suggestion prompt-buffer)
  (echo "Current suggestion actions: ~:[dis~;en~]abled."
        (prompter:actions-on-current-suggestion-enabled-p
         (current-source prompt-buffer))))

(define-command-prompt toggle-mark-forwards
    (prompt-buffer &key (direction :forward))
  "Mark current suggestion and `next-suggestion'.
Only available if current PROMPT-BUFFER source `enable-marks-p' is non-nil.
DIRECTION can be `:forward' or `:backward' and specifies which suggestion to
select next."
  (when (prompter:%current-suggestion prompt-buffer)
    (prompter:toggle-mark prompt-buffer))
  (match direction
    (:forward (next-suggestion prompt-buffer))
    (:backward (previous-suggestion prompt-buffer))))

(define-command-prompt toggle-mark-backwards (prompt-buffer)
  "Mark current suggestion and `previous-suggestion'.
Only available if `prompter:enable-marks-p' is non-nil."
  (toggle-mark-forwards prompt-buffer :direction :backward))

(define-command-prompt mark-all (prompt-buffer)
  "Mark all suggestions in the current source.
Only available if `prompter:enable-marks-p' is non-nil."
  (prompter:mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt unmark-all (prompt-buffer)
  "Unmark all visible suggestions in current source.
Only available if `prompter:enable-marks-p' is non-nil."
  (prompter:unmark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt toggle-mark-all (prompt-buffer)
  "Toggle the mark over all visible suggestions in current source.
Only available if `prompter:enable-marks-p' is non-nil."
  (prompter:toggle-mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(defun marked-selection-strings (prompt-buffer)
  "Return the default attribute string for each marked item in PROMPT-BUFFER."
  (loop for source in (prompter:sources prompt-buffer)
        nconc (loop for mark in (reverse (prompter:marks source))
                    for suggestion = (or (find mark (prompter:suggestions source)
                                               :key #'prompter:value)
                                         (find mark (prompter:initial-suggestions source)
                                               :key #'prompter:value))
                    collect (if suggestion
                                (prompter:attributes-default suggestion)
                                (second (first (prompter:object-attributes mark source)))))))

(define-command-prompt copy-selection (prompt-buffer)
  "Save default property of selection to clipboard."
  (let* ((marked (marked-selection-strings prompt-buffer))
         (current (prompter:%current-suggestion prompt-buffer))
         (props (cond
                  (marked marked)
                  (current (list (prompter:attributes-default current)))
                  (t '())))
         ;; Reverse so that text is ordered from oldest mark to newest.
         (text (str:join +newline+ (remove nil props))))
    (unless (str:emptyp text)
      (copy-to-clipboard text)
      (echo "~s copied to clipboard." text))))

(define-command-prompt paste (prompt-buffer)
  "Paste clipboard text to input."
  (ffi-buffer-paste prompt-buffer)
  (nyxt::update-prompt-input prompt-buffer))

(define-command-prompt cut (prompt-buffer)
  "Cut the input text to clipboard."
  (ffi-buffer-cut prompt-buffer)
  (nyxt::update-prompt-input prompt-buffer))

(define-class prompt-buffer-history-source (prompter:source)
  ((prompter:name "Prompt buffer input history")))

(define-command-prompt history (prompt-buffer)
  "Choose a PROMPT-BUFFER input history entry to insert as input."
  (let ((history (prompter:history prompt-buffer)))
    (if (and history (not (containers:empty-p history)))
        (let ((input (prompt1 :prompt "Input history"
                              :sources (make-instance
                                        'prompt-buffer-history-source
                                        :constructor (containers:container->list history)))))
          (unless (str:empty? input)
            (nyxt:set-prompt-buffer-input input prompt-buffer)))
        (echo "Prompt buffer has no history."))))

(define-command-prompt insert-current-suggestion (prompt-buffer)
  "Insert current suggestion default property in the PROMPT-BUFFER input."
  (when-let* ((suggestion (prompter:%current-suggestion prompt-buffer))
              (selection (prompter:attributes-default suggestion)))
    (nyxt:set-prompt-buffer-input selection prompt-buffer)))

(define-command-prompt move-start-of-input (prompt-buffer)
  "Move to the beginning of PROMPT-BUFFER input."
  (ps-eval :async t :buffer prompt-buffer
    (let ((input (nyxt/ps:qs document "#input")))
      (setf (ps:@ input selection-start) 0
            (ps:@ input selection-end) 0))))

(define-command-prompt move-end-of-input (prompt-buffer)
  "Move to the end of PROMPT-BUFFER input."
  (ps-eval :async t :buffer prompt-buffer
    (let ((input (nyxt/ps:qs document "#input")))
      (setf (ps:@ input selection-start) (ps:@ input value length)
            (ps:@ input selection-end) (ps:@ input value length)))))

(define-command-prompt select-input-text (prompt-buffer)
  "Select the prompt buffer's input text."
  (ffi-buffer-select-all prompt-buffer))

;; FIXME: Move scroll.lisp from document-mode so that prompt-buffer.lisp can reach
;; it.  Ideas?

(define-command-prompt scroll-other-buffer-up
    (prompt-buffer &key scroll-distance)
  "Scroll up the buffer behind the prompt."
  (when-let ((page (and (window prompt-buffer) (buffer (window prompt-buffer)))))
    (let ((distance (or scroll-distance (scroll-distance page))))
      (ps-eval :async t :buffer page
        (ps:chain window (scroll-by 0 (ps:lisp (- distance))))))))

(define-command-prompt scroll-other-buffer-down
    (prompt-buffer &key scroll-distance)
  "Scroll down the buffer behind the prompt."
  (when-let ((page (and (window prompt-buffer) (buffer (window prompt-buffer)))))
    (let ((distance (or scroll-distance (scroll-distance page))))
      (ps-eval :async t :buffer page
        (ps:chain window (scroll-by 0 (ps:lisp distance)))))))

(define-command-prompt scroll-page-up-other-buffer (prompt-buffer)
  "Scroll up the buffer behind the prompt by one page."
  (when-let ((page (and (window prompt-buffer) (buffer (window prompt-buffer)))))
    (ps-eval :async t :buffer page
      (ps:chain window
                (scroll-by 0 (- (* (ps:lisp (page-scroll-ratio page))
                                   (ps:@ window inner-height))))))))

(define-command-prompt scroll-page-down-other-buffer (prompt-buffer)
  "Scroll down the buffer behind the prompt by one page."
  (when-let ((page (and (window prompt-buffer) (buffer (window prompt-buffer)))))
    (ps-eval :async t :buffer page
      (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio page))
                                       (ps:@ window inner-height)))))))

(defmethod default-modes append ((buffer prompt-buffer)) '(prompt-buffer-mode))

(defmethod default-modes :around ((buffer prompt-buffer))
  (set-difference (call-next-method)
                  (list (sym:resolve-symbol :document-mode :mode)
                        (sym:resolve-symbol :base-mode :mode))))

(define-command-prompt describe-prompt-buffer (prompt-buffer)
  "Describe a prompt buffer instance."
  (describe-bindings prompt-buffer))
