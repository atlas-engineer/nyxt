;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/prompt-buffer-mode
    (:documentation "Mode for prompter buffer."))
(in-package :nyxt/prompt-buffer-mode)

(define-mode prompt-buffer-mode ()
  "The prompt buffer is where all interactions between Nyxt and the user take place.
It displays a list of suggestions which are filtered as the user types.

Many prompter-buffer-specific commands are available; you can list them with
`run-prompt-buffer-command', bound to \"f1 b\" by default.

The prompt buffer can have multiple `prompter:source's of suggestions.  Each
source has its own properties, such as the ability to mark multiple suggestions.
The same source can be used by different prompt buffers.

Each source offers a set of `return-actions' for its selection(s).  These can be
listed and run with `return-selection-over-action' (bound to \"M-return\" by
default)."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "prompt-buffer-mode" ()
      keyscheme:default
      (list
       "up" 'select-previous
       "down" 'select-next
       "button4" 'select-previous
       "button5" 'select-next
       "home" 'select-first
       "end" 'select-last
       "pagehome" 'select-first
       "pageend" 'select-last
       "escape" 'cancel-input
       "M-a" 'mark-all
       "M-u" 'unmark-all
       "C-space" 'toggle-mark
       "M-space" 'toggle-mark
       "shift-space" 'toggle-mark-backwards
       "M-shift-space" 'toggle-mark-backwards
       "M-m" 'toggle-mark-all
       "M-h" 'history
       "f1 b" 'run-prompt-buffer-command
       "f1 m" 'describe-prompt-buffer
       "return" 'return-selection
       "M-return" 'return-selection-over-action
       "C-return" 'run-selection-action
       "tab" 'insert-selection
       ; TODO: This is the Emacs Helm binding.  Better?
       "C-c C-f" 'toggle-selection-actions-enabled
       ; TODO: This is the Emacs Helm binding.  Better?
       "C-]" 'toggle-attributes-display)
      keyscheme:cua
      (list
       "C-up" 'select-first
       "C-down" 'select-last
       "C-pageup" 'select-previous-source
       "C-pagedown" 'select-next-source
       "C-v" 'paste
       "C-x" 'cut)
      keyscheme:emacs
      (list
       "C-p" 'select-previous
       "C-n" 'select-next
       "M-<" 'select-first
       "M->" 'select-last
       "M-v" 'select-previous-page
       "C-v" 'select-next-page
       "M-p" 'select-previous-source
       "M-n" 'select-next-source
       "M-[" 'select-previous-source    ; Emacs Helm binding.
       "M-]" 'select-next-source        ; Emacs Helm binding.
       "C-M-n" 'scroll-other-buffer-down
       "C-M-p" 'scroll-other-buffer-up
       "C-M-v" 'scroll-page-down-other-buffer
       "shift-C-M-v" 'scroll-page-up-other-buffer
       "C-g" 'cancel-input
       "C-e" 'move-end-of-input
       "C-a" 'move-start-of-input
       "C-b" 'nyxt/input-edit-mode:cursor-backwards
       "C-f" 'nyxt/input-edit-mode:cursor-forwards
       "C-d" 'nyxt/input-edit-mode:delete-forwards
       "M-b" 'nyxt/input-edit-mode:cursor-backwards-word
       "M-f" 'nyxt/input-edit-mode:cursor-forwards-word
       "C-backspace" 'nyxt/input-edit-mode:delete-backwards-word
       "M-backspace" 'nyxt/input-edit-mode:delete-backwards-word
       "M-d" 'nyxt/input-edit-mode:delete-forwards-word
       "C-x h" 'select-all
       "M-w" 'copy-selection
       "C-y" 'paste
       "C-w" 'cut
       "C-h b" 'run-prompt-buffer-command
       "C-j" 'run-selection-action)
      keyscheme:vi-normal
      (list
       "k" 'select-previous
       "j" 'select-next
       "C-k" 'select-previous
       ;; C-j and C-k are useful in insert mode since "j", "k" are taken.
       ;; We bind C-j and C-k in normal mode for consistency between the two modes.
       "C-j" 'select-next
       "g g" 'select-first
       "G" 'select-last
       "C-b" 'select-previous-page
       "C-f" 'select-next-page
       "K" 'select-previous-source
       "J" 'select-next-source
       "C-K" 'select-previous-source
       ;; Same as with C-j.
       "C-J" 'select-next-source
       "M-j" 'scroll-other-buffer-down
       "M-k" 'scroll-other-buffer-up
       "C-M-j" 'scroll-page-down-other-buffer
       "C-M-k" 'scroll-page-up-other-buffer
       "$" 'move-end-of-input
       "^" 'move-start-of-input
       "l" 'nyxt/input-edit-mode:cursor-forwards
       "h" 'nyxt/input-edit-mode:cursor-backwards
       "w" 'nyxt/input-edit-mode:cursor-forwards-word
       "b" 'nyxt/input-edit-mode:cursor-backwards-word
       "x" 'nyxt/input-edit-mode:delete-forwards
       ;; VI has no short keybinding for delete-backwards-word, hasn't it?
       "d w" 'nyxt/input-edit-mode:delete-forwards-word
       "z f" 'toggle-selection-actions-enabled
       "z a" 'toggle-attributes-display
       "y" 'copy-selection
       "p" 'paste
       "d d" 'cut)
      keyscheme:vi-insert
      (list
       "C-k" 'select-previous
       "C-j" 'select-next
       "C-b" 'select-previous-page
       "C-f" 'select-next-page
       "C-K" 'select-previous-source
       "C-J" 'select-next-source))))
  (:toggler-command-p nil))

(export-always 'define-command-prompt)
(defmacro define-command-prompt (name (prompt-buffer &rest arglist) &body body)
  "Like `define-command', but the first argument is special:
- it is considered a keyword argument if `&keyword' is in arglist, `&optional' otherwise,
- it is bound to `current-prompt-buffer' if unspecified,
- the body is skipped and a warning is emitted unless non-nil."
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
  "Select previous entry in PROMPT-BUFFER."
  (prompter:select-previous prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-first (prompt-buffer)
  "Select first entry in PROMPT-BUFFER."
  (prompter:select-first prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-last (prompt-buffer)
  "Select last entry in PROMPT-BUFFER."
  (prompter:select-last prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-next-source (prompt-buffer)
  "Select next source in PROMPT-BUFFER."
  (prompter:select-next-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-previous-source (prompt-buffer)
  "Select previous source in PROMPT-BUFFER."
  (prompter:select-previous-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt select-next-page (prompt-buffer &key (n 1))
  "Select entry by N next pages in PROMPT-BUFFER.
If N is negative, go to previous pages instead."
  (unless (= 0 n)
    (let ((step-page-index              ; TODO: Add multi-source support.
            (peval :buffer prompt-buffer
              (defun step-row (row)
                (ps:chain
                 (aref (ps:chain row parent-node rows)
                       (max 0
                            (min (- (ps:chain row parent-node rows length) 1)
                                 (+ (if (< 0 (ps:lisp n)) 1 -1)
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
               (ps:chain (ps:chain document (get-element-by-id "selection")) row-index)))))
      (sera:and-let* ((index-diff step-page-index))
        (prompter:select-next prompt-buffer
                              index-diff)))
    ;; TODO: Update display?  The library should probably decide when to update
    ;; it.  Drawback is that it maybe result in too many draws.  If the caller
    ;; decides when redraw, it has more control.
    (prompt-render-suggestions prompt-buffer)))

(define-command-prompt select-previous-page (prompt-buffer &key (n 1))
  "Select entry by N previous pages in PROMPT-BUFFER.
If N is negative, go to next pages instead."
  (select-next-page :prompt-buffer prompt-buffer :n (- n)))

(define-command-prompt return-selection (prompt-buffer)
  "Have the PROMPT-BUFFER return the selection, then quit."
  (prompter:return-selection prompt-buffer))

(defun make-attribute-suggestion (attribute &optional source input)
  "Return a `suggestion' wrapped around ATTRIBUTE."
  (declare (ignore source input))
  (make-instance 'prompter:suggestion
                 :value attribute
                 :attributes `(("Attribute key" ,attribute))))

(define-class attribute-source (prompter:source)
  ((prompter:name "List of prompter attributes")
   (prompter:multi-selection-p t)
   (prompter:suggestion-maker 'make-attribute-suggestion)
   (prompter:return-actions '(return-marks-only))))

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
  (let ((attributes (prompt :prompt "Mark attributes to display"
                            :sources (make-instance
                                      'attribute-source
                                      :marks (intersection
                                              (prompter:active-attributes-keys
                                               (current-source prompt-buffer))
                                              (prompter:attributes-keys-non-default
                                               (current-source prompt-buffer))
                                              :test #'string=)
                                      :constructor (prompter:attributes-keys-non-default
                                                    (current-source prompt-buffer))))))
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
  "Return a `suggestion' wrapped around COMMAND."
  (make-instance
   'prompter:suggestion
   :value command
   :attributes (nyxt::command-attributes command (parent-prompt-buffer source))))

(define-command-prompt run-prompt-buffer-command (prompt-buffer)
  "Prompt for a command to call in PROMPT-BUFFER."
  (let ((command (prompt1 :prompt "Command to run in current prompt buffer"
                          :sources (make-instance 'prompt-buffer-command-source
                                                  :parent-prompt-buffer prompt-buffer))))
    (funcall* command)))

(defun prompt-buffer-return-actions (&optional (window (current-window)))
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    (prompter:return-actions first-prompt-buffer)))

;; TODO: Should return-actions be commands?  For now, they can be either
;; commands or symbols.
(defun make-action-suggestion (action &optional source input)
  "Return a `suggestion' wrapped around ACTION."
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
                                                 (command (documentation action t))
                                                 (t (documentation action 'function)))))
                                       "")))))

(define-class action-source (prompter:source)
  ((prompter:name "List of return-actions")
   (prompter:constructor (prompt-buffer-return-actions))
   (prompter:suggestion-maker 'make-action-suggestion)))

(define-command-prompt return-selection-over-action (prompt-buffer)
  "Prompt for an action to run over PROMPT-BUFFER selection."
  (if (equal (mapcar #'type-of (prompter:sources (current-prompt-buffer)))
             '(action-source))
      (echo "Already displaying return-actions of previous prompt buffer.")
      (let ((action (prompt1 :prompt "Action to run on selection"
                             :sources 'action-source)))
        (when action
          (prompter:return-selection prompt-buffer action)))))

(define-command-prompt run-selection-action (prompt-buffer)
  "Run one of `prompter:selection-actions' without closing PROMPT-BUFFER."
  (prompter:call-selection-action prompt-buffer))

(define-command-prompt cancel-input (prompt-buffer) ; TODO: Rename.
  "Close the PROMPT-BUFFER without further action."
  (prompter:destroy prompt-buffer))

(define-command-prompt toggle-selection-actions-enabled (prompt-buffer)
  "Close the PROMPT-BUFFER without further action."
  (prompter:toggle-selection-actions-enabled prompt-buffer))

(define-command-prompt toggle-mark (prompt-buffer &key (direction :forward))
  "Mark selection.
Only available if current PROMPT-BUFFER source `multi-selection-p' is non-nil.
DIRECTION can be `:forward' or `:backward' and specifies which suggestion to
select next."
  (prompter:toggle-mark prompt-buffer)
  (match direction
    (:forward (select-next prompt-buffer))
    (:backward (select-previous prompt-buffer))))

(define-command-prompt toggle-mark-backwards (prompt-buffer)
  "Mark selection backwards.
Only available if `prompter:multi-selection-p' is non-nil."
  (toggle-mark :prompt-buffer prompt-buffer
               :direction :backward))

(define-command-prompt mark-all (prompt-buffer)
  "Mark all visible suggestions in current source.
Only available if `prompter:multi-selection-p' is non-nil."
  (prompter:mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt unmark-all (prompt-buffer)
  "Unmark all visible suggestions in current source.
Only available if `prompter:multi-selection-p' is non-nil."
  (prompter:unmark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt toggle-mark-all (prompt-buffer)
  "Toggle the mark over all visible suggestions in current source.
Only available if `prompter:multi-selection-p' is non-nil."
  (prompter:toggle-mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt copy-selection (prompt-buffer)
  "Save default property of selection to clipboard."
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

(define-command-prompt cut (prompt-buffer)
  "Cut the input text to clipboard."
  (ffi-buffer-cut prompt-buffer)
  (nyxt::update-prompt-input prompt-buffer))

(defun history-entries (&optional (window (current-window)))
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    (containers:container->list
     (prompter:history first-prompt-buffer))))

(define-class prompt-buffer-history-source (prompter:source)
  ((prompter:name "Prompt buffer input history")
   (prompter:constructor (history-entries))))

(define-command-prompt history (prompt-buffer)
  "Choose a PROMPT-BUFFER input history entry to insert as input."
  (let ((history (prompter:history prompt-buffer)))
    (if (and history (not (containers:empty-p history)))
        (let ((input (prompt1 :prompt "Input history"
                              :sources 'prompt-buffer-history-source)))
          (unless (str:empty? input)
            (nyxt:set-prompt-buffer-input input)))
        (echo "Prompt buffer has no history."))))

(define-command-prompt insert-selection (prompt-buffer)
  "Insert current selection default property in the PROMPT-BUFFER input."
  (alex:when-let ((selection (prompter:attributes-default
                              (prompter:selected-suggestion prompt-buffer))))
    (nyxt:set-prompt-buffer-input selection)))

(define-command-prompt move-start-of-input (prompt-buffer)
  "Move to the beginning of PROMPT-BUFFER input."
  (peval :buffer prompt-buffer
    (let ((input (ps:chain document (get-element-by-id "input"))))
      (setf (ps:@ input selection-start) 0
            (ps:@ input selection-end) 0))))

(define-command-prompt move-end-of-input (prompt-buffer)
  "Move to the end of PROMPT-BUFFER input."
  (peval :buffer prompt-buffer
    (let ((input (ps:chain document (get-element-by-id "input"))))
      (setf (ps:@ input selection-start) (ps:@ input value length)
            (ps:@ input selection-end) (ps:@ input value length)))))

(define-command-prompt select-all (prompt-buffer)
  "Select all the text in the prompt input."
  (ffi-buffer-select-all prompt-buffer))

;; FIXME: Move scroll.lisp from document-mode so that prompt-buffer.lisp can reach
;; it.  Ideas?

(define-command-prompt scroll-other-buffer-up (prompt-buffer
                                               &key (scroll-distance
                                                     (scroll-distance (current-buffer))))
  "Scroll up the buffer behind the prompt."
  (peval :buffer (current-buffer)
    (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance))))))

(define-command-prompt scroll-other-buffer-down (prompt-buffer
                                                 &key (scroll-distance
                                                       (scroll-distance (current-buffer))))
  "Scroll down the buffer behind the prompt."
  (peval :buffer (current-buffer)
    (ps:chain window (scroll-by 0 (ps:lisp scroll-distance)))))

(define-command-prompt scroll-page-up-other-buffer (prompt-buffer)
  "Scroll up the buffer behind the prompt by one page."
  (peval :buffer (current-buffer)
    (ps:chain window (scroll-by 0 (- (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                        (ps:@ window inner-height)))))))

(define-command-prompt scroll-page-down-other-buffer (prompt-buffer)
  "Scroll down the buffer behind the prompt by one page."
  (peval :buffer (current-buffer)
    (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                     (ps:@ window inner-height))))))

(defmethod default-modes append ((buffer prompt-buffer))
  '(prompt-buffer-mode))
(defmethod default-modes :around ((buffer prompt-buffer))
  ;; TODO: `prompt-buffer' should not be a web-buffer.
  (set-difference (call-next-method) (list (resolve-symbol :document-mode :mode)
                                           (resolve-symbol :base-mode :mode))))

;; FIXME: Arglist used to have prompt-buffer, but it's not URL-serializable.
;; Maybe have prompt-buffers have IDs so that we can identify those by IDs?
;; How do we actually identify prompt-buffers?
(define-internal-page-command describe-prompt-buffer ()
    (buffer (str:concat "*Help-" (prompter:prompt (current-prompt-buffer)) "-prompter*")
            ;; TODO: Can we somehow fix the load order in the .asd?
            (resolve-symbol :help-mode :mode))
  "Describe a prompt buffer instance."
  (let* ((prompt-buffer (current-prompt-buffer))
         (modes (modes prompt-buffer))
         (sources (prompter:sources prompt-buffer)))
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 (prompter:prompt prompt-buffer))
      (:p (:raw (resolve-backtick-quote-links (documentation 'prompt-buffer 'type) 'prompt-buffer)))
      (:h2 "Modes:")
      (:ul
       (loop for mode in modes
             collect (:li (:a :href
                              (nyxt-url
                               'describe-class
                               :class (sera:class-name-of mode))
                              (string (sera:class-name-of mode))))))
      (:h2 "Sources:")
      (:ul
       (loop for source in sources
             collect (:li (:a :href
                              (nyxt-url
                               'describe-class
                               :class (sera:class-name-of source))
                              (string (sera:class-name-of source)))))))))
