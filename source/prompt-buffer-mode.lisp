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
       "return" 'return-selection
       "C-return" 'return-input
       "C-space" 'prompt-buffer-toggle-mark
       "shift-space" 'prompt-buffer-toggle-mark-backwards
       "M-space" 'prompt-buffer-toggle-mark
       "C-w" 'copy-selection
       "C-v" 'prompt-buffer-paste))
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
  (prompter:return-selection (prompter prompt-buffer))
  (hide-prompt-buffer prompt-buffer))

(define-command return-input (&optional (prompt-buffer (current-prompt-buffer)))
  "Have the PROMT-BUFFER return the selection, then quit."
  (prompter:return-input (prompter prompt-buffer))
  (hide-prompt-buffer prompt-buffer))

(define-command cancel-input (&optional (prompt-buffer (current-prompt-buffer)))
  "Close the prompt-buffer without further action."
  (hide-prompt-buffer prompt-buffer))

(define-command prompt-buffer-toggle-mark (&key
                                           (prompt-buffer (current-prompt-buffer))
                                           (direction :forward))
  "Mark selection.
Only available if current prompt-buffer source `multi-selection-p' is non-nil.
DIRECTION can be `:forward' or `:backward' and specifies which suggestion to
select next."
  (when (prompter:multi-selection-p (current-source))
    (multiple-value-bind (suggestion source)
        (current-selection)
      (with-accessors ((marked-suggestions prompter:marked-suggestions)) source
        (match (find (prompter:value suggestion) marked-suggestions)
          ((guard n n) (setf marked-suggestions (delete (prompter:value suggestion) marked-suggestions)))
          (_ (push (prompter:value suggestion) marked-suggestions)))))
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

(define-command copy-selection (&optional (prompt-buffer (current-prompt-buffer)))
  "Copy default property of selection to clipboard."
  (let* ((marks (prompt-buffer-marked-suggestions))
         (props (if marks
                    (mapcar #'prompter:object-properties marks)
                    (list (prompter:properties (current-selection)))))
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
