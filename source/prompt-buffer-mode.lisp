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
       "C-v" 'select-next-page
       "M-v" 'select-previous-page
       "M->" 'select-last
       "M-<" 'select-first
       "M-]" 'select-next-source
       "M-[" 'select-previous-source
       "return" 'return-selection
       "C-return" 'return-input         ; TODO: Bind to shift-return instead?
       "M-return" 'return-selection-over-action       ; TODO: Also bind to C-return?
       "C-j" 'run-persistent-action
       "C-c C-f" 'toggle-follow
       "C-space" 'prompt-buffer-toggle-mark
       "shift-space" 'prompt-buffer-toggle-mark-backwards
       "M-space" 'prompt-buffer-toggle-mark
       "M-a" 'prompt-buffer-mark-all
       "M-u" 'prompt-buffer-unmark-all
       "M-m" 'prompt-buffer-toggle-mark-all
       "C-w" 'copy-selection
       "C-v" 'prompt-buffer-paste
       "M-h" 'prompt-buffer-history))
    ;; TODO: We could have VI bindings for the minibuffer too.
    ;; But we need to make sure it's optional + to have an indicator
    ;; for the mode.
    )))

(define-command select-next (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next entry in prompt buffer."
  (prompter:select-next (prompter prompt-buffer))
  ;; TODO: Update display?  The library should probably decide when to update
  ;; it.  Drawback is that it maybe result in too many draws.  If the caller
  ;; decides when redraw, it has more control.
  (update-suggestion-html prompt-buffer))

(define-command select-previous (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next entry in prompt buffer."
  (prompter:select-previous (prompter prompt-buffer))
  (update-suggestion-html prompt-buffer))

(define-command select-first (&optional (prompt-buffer (current-prompt-buffer)))
  "Select first entry in prompt buffer."
  (prompter:select-first (prompter prompt-buffer))
  (update-suggestion-html prompt-buffer))

(define-command select-last (&optional (prompt-buffer (current-prompt-buffer)))
  "Select first entry in prompt buffer."
  (prompter:select-last (prompter prompt-buffer))
  (update-suggestion-html prompt-buffer))

(define-command select-next-source (&optional (prompt-buffer (current-prompt-buffer)))
  "Select next soruce in prompt buffer."
  (prompter:select-next-source (prompter prompt-buffer))
  (update-suggestion-html prompt-buffer))

(define-command select-previous-source (&optional (prompt-buffer (current-prompt-buffer)))
  "Select previous source in prompt buffer."
  (prompter:select-previous-source (prompter prompt-buffer))
  (update-suggestion-html prompt-buffer))

(define-command select-next-page (&key (prompt-buffer (current-prompt-buffer))
                                  (steps 1))
  "Select entry by STEPS next page in prompt buffer.
If STEPS is negative, go to previous pages instead."
  (unless (= 0 steps)
    (let ((step-page-index              ; TODO: Add multi-source support.
            (ffi-minibuffer-evaluate-javascript
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
        (prompter:select-next (prompter prompt-buffer)
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
                        (prompter:return-selection (prompter prompt-buffer)))))

(define-command return-input (&optional (prompt-buffer (current-prompt-buffer))) ; TODO: Remove if we remove `must-match-p'?
  "Have the PROMT-BUFFER return the selection, then quit."
  (hide-prompt-buffer prompt-buffer
                      (lambda () (prompter:return-input (prompter prompt-buffer)))))

(defun prompt-buffer-actions (&optional (window (current-window)))
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-minibuffers window))))
    (prompter:actions (prompter first-prompt-buffer))))

;; TODO: Should actions be commands?
(defun action-properties (action)
  "Return the name and documentation properties of the given ACTION symbol."
  ;; TODO: Return bindings.
  (flet ((first-line (string)
           (first (str:split (string #\newline) string))))
    `(:name ,(symbol-name action)
      :documentation ,(first-line (documentation action 'function)))))

(define-class action-source (prompter:prompter-source)
  ((prompter:name "List of actions")
   (prompter:initial-suggestions (prompt-buffer-actions))
   (prompter:suggestion-property-function #'action-properties)))

(define-command return-selection-over-action (&optional (prompt-buffer (current-prompt-buffer)))
  "Prompt for an action to run over PROMPT-BUFFER selection."
  (let ((action (prompt
                 :prompter (list
                            :prompt "Action to run on selection"
                            :sources (list (make-instance 'action-source))))))
    (when action
      (hide-prompt-buffer prompt-buffer
                          (lambda ()
                            (prompter:return-selection (prompter prompt-buffer) action))))))

(define-command run-persistent-action (&optional (prompt-buffer (current-prompt-buffer)))
  "Run persistent action over selected suggestion without closing PROMPT-BUFFER."
  (prompter:call-persistent-action (prompter prompt-buffer)))

(define-command cancel-input (&optional (prompt-buffer (current-prompt-buffer))) ; TODO: Rename.
  "Close the prompt-buffer without further action."
  (hide-prompt-buffer prompt-buffer))

(define-command toggle-follow (&optional (prompt-buffer (current-prompt-buffer)))
  "Close the prompt-buffer without further action."
  (prompter:toggle-follow (prompter prompt-buffer)))

(define-command prompt-buffer-toggle-mark (&key
                                           (prompt-buffer (current-prompt-buffer))
                                           (direction :forward))
  "Mark selection.
Only available if current prompt-buffer source `multi-selection-p' is non-nil.
DIRECTION can be `:forward' or `:backward' and specifies which suggestion to
select next."
  (when (prompter:multi-selection-p (current-source))
    (multiple-value-bind (suggestion source)
        (current-suggestion)
      (let ((suggestion-value (prompter:value suggestion)))
        (with-accessors ((marked-suggestions prompter:marked-suggestions)) source
          (match (find suggestion-value marked-suggestions)
            ((guard n n) (setf marked-suggestions (delete suggestion-value marked-suggestions)))
            (_ (push suggestion-value marked-suggestions))))))
    (match direction
      (:forward (select-next prompt-buffer))
      (:backward (select-previous prompt-buffer)))))

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
  (when (prompter:multi-selection-p (current-source))
    (alex:unionf (prompter:marked-suggestions (current-source))
                 (mapcar #'prompter:value (prompter:suggestions (current-source))))
    (update-suggestion-html prompt-buffer)))

(define-command prompt-buffer-unmark-all (&optional (prompt-buffer (current-prompt-buffer)))
  "Unmark all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (let ((source (current-source)))
    (when (prompter:multi-selection-p source)
      (with-accessors ((marked-suggestions prompter:marked-suggestions)
                       (suggestions prompter:suggestions))
          source
        (setf marked-suggestions
              (set-difference marked-suggestions
                              (mapcar #'prompter:value suggestions))))
      (update-suggestion-html prompt-buffer))))

(define-command prompt-buffer-toggle-mark-all (&optional
                                               (prompt-buffer (current-prompt-buffer)))
  "Toggle the mark over all visible suggestions in current source.
Only available if `multi-selection-p' is non-nil."
  (let ((source (current-source)))
    (when (prompter:multi-selection-p source)
      (with-accessors ((suggestions prompter:suggestions)
                       (marked-suggestions prompter:marked-suggestions))
          source
        (let ((suggestion-values (mapcar #'prompter:value suggestions)))
          (setf marked-suggestions
                (cond
                  ((subsetp marked-suggestions suggestion-values)
                   (set-difference suggestion-values marked-suggestions))
                  ((subsetp suggestion-values marked-suggestions)
                   (set-difference marked-suggestions suggestion-values))
                  (t ; When the intersection of suggestion-values and marked-suggestions is non-trivial.
                   (set-difference
                    (union marked-suggestions suggestion-values)
                    (intersection marked-suggestions suggestion-values)))))))
      (update-suggestion-html prompt-buffer))))

(define-command copy-selection (&optional (prompt-buffer (current-prompt-buffer)))
  "Copy default property of selection to clipboard."
  (let* ((marks (all-marked-suggestions prompt-buffer))
         (props (if marks
                    (mapcar #'prompter:object-properties marks)
                    (list (prompter:properties (current-suggestion)))))
         ;; Reverse so that text is ordered from oldest mark to newest.
         (text (str:join (string #\newline) (mapcar #'second (reverse props)))))
    (unless (str:emptyp text)
      (trivial-clipboard:text text)
      (echo "Copied ~s to clipboard." text))))

(define-command prompt-buffer-paste (&optional (window (current-window)))
  "Paste clipboard text to input."
  (ffi-minibuffer-evaluate-javascript
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
  (sera:and-let* ((first-prompt-buffer (first (nyxt::active-minibuffers window))))
    ;; TODO: No need for delete-duplicates if we don't allow duplicates in the first place.
    (delete-duplicates (containers:container->list
                        (prompter:history (prompter first-prompt-buffer)))
                       :test #'equal)))

(define-class prompt-buffer-history-source (prompter:prompter-source)
  ((prompter:name "Prompt buffer input history")
   (prompter:initial-suggestions (prompt-buffer-history-entries))))

(define-command prompt-buffer-history (&optional (prompt-buffer (current-prompt-buffer)))
  "Choose a prompt-buffer input history entry to insert as input."
  (if (prompter:history (prompter prompt-buffer))
      (let ((input (prompt
                    :prompter
                    (list :prompt "Input history"
                          :sources (list (make-instance 'prompt-buffer-history-source))))))
        (unless (str:empty? input)
          (nyxt::set-prompt-buffer-input input)))
      (echo "Prompt buffer has no history.")))
