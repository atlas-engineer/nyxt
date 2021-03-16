;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/prompt-buffer-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(define-mode prompt-buffer-mode ()
  "Mode for the prompt buffer."
  ((keymap-scheme
    (define-scheme "prompt-buffer"
      scheme:cua
      (list
       "C-n" 'select-next
       "C-p" 'select-previous
       "M-n" 'select-next-page
       "M-p" 'select-previous-page
       "M->" 'select-last
       "M-<" 'select-first
       "M-]" 'select-next-source
       "M-[" 'select-previous-source
       "return" 'return-selection
       "C-return" 'return-input         ; TODO: Bind to shift-return instead?
       "M-return" 'return-selection-over-action       ; TODO: Also bind to C-return?
       "C-j" 'run-persistent-action
       "C-g" 'cancel-input
       "f1 b" 'run-prompt-buffer-command
       "C-h b" 'run-prompt-buffer-command ; TODO: Move to Emacs bindings.
       "C-c C-f" 'toggle-follow
       "C-]" 'toggle-properties-display ; "C-]" is Emacs Helm binding.
       "C-space" 'prompt-buffer-toggle-mark
       "shift-space" 'prompt-buffer-toggle-mark-backwards
       "M-space" 'prompt-buffer-toggle-mark
       "M-a" 'prompt-buffer-mark-all
       "M-u" 'prompt-buffer-unmark-all
       "M-m" 'prompt-buffer-toggle-mark-all
       "C-w" 'copy-selection
       "C-v" 'prompt-buffer-paste
       "M-h" 'prompt-buffer-history))
    ;; TODO: We could have VI bindings for the prompt-buffer too.
    ;; But we need to make sure it's optional + to have an indicator
    ;; for the mode.
    )))

(define-command select-next (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next entry in prompt buffer."
  (prompter:select-next prompt-buffer)
  ;; TODO: Update display?  The library should probably decide when to update
  ;; it.  Drawback is that it maybe result in too many draws.  If the caller
  ;; decides when redraw, it has more control.
  (update-suggestion-html prompt-buffer))

(define-command select-previous (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next entry in prompt buffer."
  (prompter:select-previous prompt-buffer)
  (update-suggestion-html prompt-buffer))

(define-command select-first (&optional (prompt-buffer (current-prompt-buffer)))
  "Select first entry in prompt buffer."
  (prompter:select-first prompt-buffer)
  (update-suggestion-html prompt-buffer))

(define-command select-last (&optional (prompt-buffer (current-prompt-buffer)))
  "Select first entry in prompt buffer."
  (prompter:select-last prompt-buffer)
  (update-suggestion-html prompt-buffer))

(define-command select-next-source (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next soruce in prompt buffer."
  (prompter:select-next-source prompt-buffer)
  (update-suggestion-html prompt-buffer))

(define-command select-previous-source (&optional (prompt-buffer (current-prompt-buffer)))
  "Select previous source in prompt buffer."
  (prompter:select-previous-source prompt-buffer)
  (update-suggestion-html prompt-buffer))

(define-command select-next-page (&key (prompt-buffer (current-prompt-buffer))
                                  (steps 1))
  "Select entry by STEPS next page in prompt buffer.
If STEPS is negative, go to previous pages instead."
  (unless (= 0 steps)
    (let ((step-page-index              ; TODO: Add multi-source support.
            (ffi-prompt-buffer-evaluate-javascript
             (current-window)
             (ps:ps
               (defun element-in-view-port-p (element)
                 (ps:let* ((rect (ps:chain element (get-bounding-client-rect))))
                   (if (and (>= (ps:chain rect top) 0)
                            (>= (ps:chain rect left) 0)
                            (<= (ps:chain rect right) (ps:chain window inner-width))
                            (<= (ps:chain rect bottom) (ps:chain window inner-height)))
                       t nil)))
               (defun step-row (row)
                 (ps:chain
                  (aref (ps:chain row parent-node rows)
                        (max 0
                             (min (- (ps:chain row parent-node rows length) 1)
                                  (+ (if (< 0 (ps:lisp steps)) 1 -1)
                                     (ps:chain row row-index)))))))
               (defun find-first-element-out-of-view (row)
                 (if (element-in-view-port-p row)
                     (let ((new-row (step-row row)))
                       (if (eq new-row row)
                           row
                           (find-first-element-out-of-view new-row)))
                     row))
               (-
                (ps:chain
                 (find-first-element-out-of-view (ps:chain document (get-element-by-id "cursor")))
                 row-index)
                (ps:chain (ps:chain document (get-element-by-id "cursor")) row-index))))))
      (sera:and-let* ((index-diff (parse-integer step-page-index :junk-allowed t)))
        (prompter:select-next prompt-buffer
                              index-diff)))
    ;; TODO: Update display?  The library should probably decide when to update
    ;; it.  Drawback is that it maybe result in too many draws.  If the caller
    ;; decides when redraw, it has more control.
    (update-suggestion-html prompt-buffer)))

(define-command select-previous-page (&key (prompt-buffer (current-prompt-buffer))
                                      (steps 1))
  "Select entry by STEPS previous page in prompt buffer.
If STEPS is negative, go to next pages instead."
  (select-next-page :prompt-buffer prompt-buffer :steps (- steps)))

(define-command return-selection (&optional (prompt-buffer (current-prompt-buffer)))
  "Have the PROMT-BUFFER return the selection, then quit."
  (hide-prompt-buffer prompt-buffer
                      (lambda ()
                        (prompter:return-selection prompt-buffer))))

(define-command return-input (&optional (prompt-buffer (current-prompt-buffer))) ; TODO: Remove if we remove `must-match-p'?
  "Have the PROMT-BUFFER return the selection, then quit."
  (hide-prompt-buffer prompt-buffer
                      (lambda () (prompter:return-input prompt-buffer))))

(defun make-property-suggestion (property source &optional input)
  "Return a `suggestion' wrapping around PROPERTY. "
  (make-instance 'prompter:suggestion
                 :value property
                 :properties `(:name ,(string-capitalize (symbol-name property)))
                 :source source
                 :input input))

(define-class property-source (prompter:source)
  ((prompter:name "List of prompter properties")
   (prompter:multi-selection-p t)
   (prompter:suggestion-maker 'make-property-suggestion)))

(define-command toggle-properties-display (&optional (prompt-buffer (current-prompt-buffer)))
  "Prompt for which prompter properties to display."
  (let ((properties (prompt
                     :prompt "Properties to display"
                     :sources (list (make-instance 'property-source
                                                   :marks (prompter:active-properties (current-source prompt-buffer))
                                                   :constructor (prompter:properties-non-default
                                                                 (current-source prompt-buffer)))))))
    (when properties
      (setf (prompter:active-properties (current-source prompt-buffer))
            properties)
      (update-suggestion-html prompt-buffer))))

(define-class prompt-buffer-command-source (prompter:source)
  ((prompter:name "List of prompt buffer commands")
   (parent-prompt-buffer (error "Parent prompt buffer required"))
   (prompter:must-match-p t)
   (prompter:suggestion-maker 'make-prompt-buffer-command-suggestion)
   (prompter:constructor (nyxt::get-commands (current-prompt-buffer))))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defun make-prompt-buffer-command-suggestion (command source &optional input)
  "Return a `suggestion' wrapping around COMMAND."
  (make-instance
   'prompter:suggestion
   :value command
   :properties (nyxt::command-properties command (parent-prompt-buffer source))
   :source source
   :input input))

(define-command run-prompt-buffer-command (&optional (prompt-buffer (current-prompt-buffer)))
  "Prompt for a command to call in PROMPT-BUFFER."
  (let ((command (first (prompt
                         :prompt "Command to run in current prompt buffer"
                         :sources (list (make-instance 'prompt-buffer-command-source
                                                       :parent-prompt-buffer prompt-buffer))))))
    (when command
      (funcall-safely command))))

(defun prompt-buffer-actions (&optional (window (current-window)))
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    (prompter:actions first-prompt-buffer)))

;; TODO: Should actions be commands?  For now, they can be either commands or symbols.
(defun make-action-suggestion (action source &optional input)
  "Return a `suggestion' wrapping around ACTION."
  (flet ((first-line (string)
           (first (str:split (string #\newline) string))))
    (make-instance
     'prompter:suggestion
     :value action
     ;; TODO: Include bindings in properties.
     :properties `(:name ,(symbol-name (typecase action
                                         (command (name action))
                                         (t action)))
                   :documentation ,(first-line (typecase action
                                                 (command (nyxt::docstring action))
                                                 (t (documentation action 'function)))))
     :source source
     :input input)))

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
      (hide-prompt-buffer prompt-buffer
                          (lambda ()
                            (prompter:return-selection prompt-buffer action))))))

(define-command run-persistent-action (&optional (prompt-buffer (current-prompt-buffer)))
  "Run persistent action over selected suggestion without closing PROMPT-BUFFER."
  (prompter:call-persistent-action prompt-buffer))

(define-command cancel-input (&optional (prompt-buffer (current-prompt-buffer))) ; TODO: Rename.
  "Close the prompt-buffer without further action."
  (hide-prompt-buffer prompt-buffer))

(define-command toggle-follow (&optional (prompt-buffer (current-prompt-buffer)))
  "Close the prompt-buffer without further action."
  (prompter:toggle-follow prompt-buffer))

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
  (update-suggestion-html prompt-buffer))

(define-command prompt-buffer-unmark-all (&optional (prompt-buffer (current-prompt-buffer)))
  "Unmark all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (prompter:unmark-all prompt-buffer)
  (update-suggestion-html prompt-buffer))

(define-command prompt-buffer-toggle-mark-all (&optional
                                               (prompt-buffer (current-prompt-buffer)))
  "Toggle the mark over all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (prompter:toggle-mark-all prompt-buffer)
  (update-suggestion-html prompt-buffer))

(define-command copy-selection (&optional (prompt-buffer (current-prompt-buffer)))
  "Copy default property of selection to clipboard."
  (let* ((marks (prompter:all-marks prompt-buffer))
         (props (if marks
                    (mapcar #'prompter:object-properties marks)
                    (list (prompter:properties (prompter:selected-suggestion
                                                prompt-buffer)))))
         ;; Reverse so that text is ordered from oldest mark to newest.
         (text (str:join (string #\newline) (mapcar #'second (reverse props)))))
    (unless (str:emptyp text)
      (trivial-clipboard:text text)
      (echo "Copied ~s to clipboard." text))))

(define-command prompt-buffer-paste (&optional (window (current-window)))
  "Paste clipboard text to input."
  (ffi-prompt-buffer-evaluate-javascript
   window
   (ps:ps
     (defun insert-at (tag input-text)
       (let ((begin (ps:chain tag selection-start))
             (end (ps:chain tag selection-end)))
         (setf (ps:chain tag value)
               (+ (ps:chain tag value (substring 0 begin))
                  input-text
                  (ps:chain tag value
                            (substring end
                                       (ps:chain tag value length)))))
         (if (= begin end)
             (progn
               (setf (ps:chain tag selection-start) (+ begin (ps:chain input-text length)))
               (setf (ps:chain tag selection-end) (ps:chain tag selection-start)))
             (progn
               (setf (ps:chain tag selection-start) begin)
               (setf (ps:chain tag selection-end) (+ begin (ps:chain input-text length)))))))
     (insert-at (ps:chain document (get-element-by-id "input"))
                (ps:lisp (ring-insert-clipboard (nyxt::clipboard-ring *browser*)))))))

(defun prompt-buffer-history-entries (&optional (window (current-window)))
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    ;; TODO: No need for delete-duplicates if we don't allow duplicates in the first place.
    (delete-duplicates (containers:container->list
                        (prompter:history first-prompt-buffer))
                       :test #'equal)))

(define-class prompt-buffer-history-source (prompter:source)
  ((prompter:name "Prompt buffer input history")
   (prompter:constructor (prompt-buffer-history-entries))))

(define-command prompt-buffer-history (&optional (prompt-buffer (current-prompt-buffer)))
  "Choose a prompt-buffer input history entry to insert as input."
  (if (prompter:history prompt-buffer)
      (let ((input (first (prompt
                           :prompt "Input history"
                           :sources (list (make-instance 'prompt-buffer-history-source))))))
        (unless (str:empty? input)
          (nyxt::set-prompt-buffer-input input)))
      (echo "Prompt buffer has no history.")))
