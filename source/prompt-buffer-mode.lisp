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
       ;; "M-v" 'select-previous-page
       "return" 'return-selection))
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

(define-command select-next-page (&optional (prompt-buffer (current-prompt-buffer)))
  "Select entry on next page in prompt buffer."
  (let ((next-page-index                ; TODO: Add multi-source support.
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
             (defun next-row (row)
               (ps:chain
                (aref (ps:chain row parent-node rows)
                      (1+ (ps:chain row row-index)))))
             (defun find-first-element-out-of-view (row)
               (if (element-in-view-port-p row)
                   (find-first-element-out-of-view (next-row row))
                   row))
             (ps:chain
              (find-first-element-out-of-view (ps:chain document (get-element-by-id "cursor")))
              row-index)))))
    (sera:and-let* ((next-page-index (parse-integer next-page-index :junk-allowed t)))
      (prompter:select-next (prompter prompt-buffer)
                            (- next-page-index
                               (second (prompter:selection (prompter prompt-buffer)))))))
  ;; TODO: Update display?  The library should probably decide when to update
  ;; it.  Drawback is that it maybe result in too many draws.  If the caller
  ;; decides when redraw, it has more control.
  (update-suggestion-html prompt-buffer))

(define-command return-selection (&optional (prompt-buffer (current-prompt-buffer)))
  "Have the PROMT-BUFFER return the selection, then quit."
  (prompter:return-selection (nyxt:prompter prompt-buffer))
  (hide-prompt-buffer prompt-buffer))

(define-command cancel-input (&optional (prompt-buffer (current-prompt-buffer)))
  "Close the prompt-buffer without further action."
  (hide-prompt-buffer prompt-buffer))
