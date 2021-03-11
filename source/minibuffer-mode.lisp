;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/minibuffer-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(define-mode minibuffer-mode ()
  "Mode for the minibuffer."
  ((keymap-scheme
    (define-scheme "minibuffer"
      scheme:cua
      (list
       "hyphen" 'self-insert-minibuffer
       "space" 'self-insert-minibuffer
       "C-f" 'cursor-forwards
       "M-f" 'cursor-forwards-word
       "C-right" 'cursor-forwards-word
       "C-b" 'cursor-backwards
       "M-b" 'cursor-backwards-word
       "C-left" 'cursor-backwards-word
       "M-d" 'delete-forwards-word
       "M-backspace" 'delete-backwards-word
       "right" 'cursor-forwards
       "left" 'cursor-backwards
       "C-d" 'delete-forwards
       "delete" 'delete-forwards
       "backspace" 'delete-backwards
       "C-a" 'cursor-beginning
       "home" 'cursor-beginning
       "C-e" 'cursor-end
       "end" 'cursor-end
       "C-k" 'kill-line
       "return" 'return-selection
       "C-return" 'return-input
       "C-g" 'cancel-input
       "escape" 'cancel-input
       "C-n" 'select-next
       "C-p" 'select-previous
       "button4" 'select-previous
       "button5" 'select-next
       "down" 'select-next
       "up" 'select-previous
       "C-v" 'minibuffer-paste
       "C-y" 'minibuffer-paste
       "s-insert" 'minibuffer-paste
       "C-w" 'copy-suggestion
       "C-c" 'copy-suggestion
       "C-insert" 'copy-suggestion
       "tab" 'insert-suggestion
       "M-h" 'minibuffer-history
       "C-space" 'minibuffer-toggle-mark
       "shift-space" 'minibuffer-toggle-mark-backwards
       "M-space" 'minibuffer-toggle-mark
       "M-a" 'minibuffer-mark-all
       "M-u" 'minibuffer-unmark-all
       "M-m" 'minibuffer-toggle-mark-all))
    ;; TODO: We could have VI bindings for the minibuffer too.
    ;; But we need to make sure it's optional + to have an indicator
    ;; for the mode.
    )))

(define-mode set-url-mode (nyxt/minibuffer-mode:minibuffer-mode)
  "Minibuffer mode for setting the URL of a buffer."
  ((keymap-scheme
    (define-scheme "set-url"
      scheme:cua
      (list
       "tab" 'insert-suggestion-or-search-engine)))))

(define-command insert-suggestion-or-search-engine (&optional (minibuffer (current-minibuffer)))
  "Paste selected suggestion or search engine to input.
If minibuffer input is not empty and the selection is on first position,
complete against a search engine."
  (cond
    ;; Complete a search engine name.
    ((and (not (str:emptyp (input-buffer minibuffer)))
          (zerop (nyxt::suggestion-cursor minibuffer)))
     (let* ((engines (search-engines (current-buffer)))
            (matching-engines
              (remove-if (complement (alex:curry #'str:starts-with-p (input-buffer minibuffer)))
                         engines
                         :key #'shortcut)))
       (match (length matching-engines)
         (1
          (kill-whole-line minibuffer)
          (insert minibuffer (str:concat (shortcut (first matching-engines)) " ")))
         (match-count
          (let ((engine (prompt-minibuffer
                         :input-prompt "Search engine"
                         :input-buffer (if (zerop match-count) "" (input-buffer minibuffer))
                         :suggestion-function #'nyxt:search-engine-suggestion-filter)))
            (when engine
              (kill-whole-line minibuffer)
              (insert minibuffer (str:concat (shortcut engine) " "))))))))
    (t
     (insert-suggestion minibuffer))))

(define-command return-selection (&optional (minibuffer (current-minibuffer)))
  "Return with minibuffer selection."
  (let ((result))
    (with-slots (must-match-p nyxt::suggestions nyxt::suggestion-cursor
                 invisible-input-p
                 multi-selection-p nyxt::marked-suggestions)
        minibuffer
      (match (or nyxt::marked-suggestions
                 (and nyxt::suggestions
                      (list (nth nyxt::suggestion-cursor nyxt::suggestions)))
                 (and (not must-match-p)
                      (list (input-buffer minibuffer))))
        ((guard nyxt::suggestions nyxt::suggestions)
         ;; Note that "immediate input" is also in suggestions, so it's caught here.
         (setf nyxt::suggestions
               (mapcar (lambda (suggestion) (if (stringp suggestion)
                                                (str:replace-all " " " " suggestion)
                                                suggestion))
                       nyxt::suggestions))
         (setf result (if multi-selection-p
                          nyxt::suggestions
                          (first nyxt::suggestions))))
        (nil (when invisible-input-p
               (setf result (input-buffer minibuffer))))))
    (quit-minibuffer minibuffer)
    (calispel:! (channel minibuffer) result)))

(define-command return-input (&optional (minibuffer (current-minibuffer)))
  "Return with minibuffer input, ignoring the selection."
  (calispel:! (channel minibuffer) (input-buffer minibuffer))
  (quit-minibuffer minibuffer))

(defun quit-minibuffer (&optional (minibuffer (current-minibuffer)))
  (unless (or (null (history minibuffer))
              (str:empty? (input-buffer minibuffer)))
    (containers:insert-item (history minibuffer) (input-buffer minibuffer)))
  (hide minibuffer))

(define-command cancel-input (&optional (minibuffer (current-minibuffer)))
  "Close the minibuffer query without further action."
  (calispel:! (interrupt-channel minibuffer) t)
  (hide minibuffer))

(define-command self-insert-minibuffer ()
  "Self insert with the current minibuffer."
  (self-insert (nyxt:current-minibuffer)))

(define-command self-insert (receiver)
  "Insert last key from current window to the receiver."
  (let ((key-string (keymap:key-value (nyxt::last-key (current-window))))
        (translation-table '(("hyphen" "-")
                             ;; Regular spaces are concatenated into a single
                             ;; one by HTML rendering, so we use a non-breaking
                             ;; space to avoid confusing the user.
                             ("space" " "))))
    (setf key-string (or (cadr (assoc key-string translation-table :test #'string=))
                         key-string))
    (insert receiver key-string)))

(define-command delete-forwards (&optional (minibuffer (current-minibuffer)))
  "Delete character after cursor."
  (cluffer:delete-item (input-cursor minibuffer))
  (reset-suggestion-state minibuffer)
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command delete-backwards (&optional (minibuffer (current-minibuffer)))
  "Delete character before cursor."
  (text-buffer::delete-item-backward (input-cursor minibuffer))
  (reset-suggestion-state minibuffer)
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command cursor-forwards (&optional (minibuffer (current-minibuffer)))
  "Move cursor forward by one."
  (text-buffer::safe-forward (input-cursor minibuffer))
  (state-changed minibuffer)
  (update-input-buffer-display minibuffer))

(define-command cursor-backwards (&optional (minibuffer (current-minibuffer)))
  "Move cursor backwards by one."
  (text-buffer::safe-backward (input-cursor minibuffer))
  (state-changed minibuffer)
  (update-input-buffer-display minibuffer))

(define-command cursor-beginning (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the beginning of the input area."
  (cluffer:beginning-of-line (input-cursor minibuffer))
  (state-changed minibuffer)
  (update-input-buffer-display minibuffer))

(define-command cursor-end (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the end of the input area."
  (cluffer:end-of-line (input-cursor minibuffer))
  (state-changed minibuffer)
  (update-input-buffer-display minibuffer))

(define-command cursor-forwards-word (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the end of the word at point."
  (text-buffer::move-forward-word (input-cursor minibuffer)
                                  :conservative-word-move
                                  (conservative-word-move (current-buffer)))
  (state-changed minibuffer)
  (update-input-buffer-display minibuffer)
  (cluffer:cursor-position (input-cursor minibuffer)))

(define-command cursor-backwards-word (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the beginning of the word at point."
  (text-buffer::move-backward-word (input-cursor minibuffer)
                                   :conservative-word-move
                                   (conservative-word-move (current-buffer)))
  (state-changed minibuffer)
  (update-input-buffer-display minibuffer)
  (cluffer:cursor-position (input-cursor minibuffer)))

(define-command delete-forwards-word (&optional (minibuffer (current-minibuffer)))
  "Delete characters from cursor position until the end of the word at point."
  (text-buffer::delete-forward-word (input-cursor minibuffer))
  (reset-suggestion-state minibuffer)
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command delete-backwards-word (&optional (minibuffer (current-minibuffer)))
  "Delete characters from cursor position until the beginning of the word at point."
  (text-buffer::delete-backward-word (input-cursor minibuffer))
  (reset-suggestion-state minibuffer)
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command kill-line (&optional (minibuffer (current-minibuffer)))
  "Delete all characters from cursor position until the end of the line."
  (text-buffer::kill-forward-line (input-cursor minibuffer))
  (reset-suggestion-state minibuffer)
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command kill-whole-line (&optional (minibuffer (current-minibuffer)))
  "Delete all characters in the input."
  (text-buffer::kill-line (input-cursor minibuffer))
  (reset-suggestion-state minibuffer)
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command select-next (&optional (minibuffer (current-minibuffer)))
  "Select next entry in minibuffer."
  (when (< (nyxt::suggestion-cursor minibuffer) (- (length (nyxt::suggestions minibuffer)) 1))
    (incf (nyxt::suggestion-cursor minibuffer))
    (state-changed minibuffer)
    (update-suggestions-display minibuffer)
    (evaluate-script minibuffer
                     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                                      (scroll-into-view false))))))

(define-command select-previous (&optional (minibuffer (current-minibuffer)))
  "Select previous entry in minibuffer."
  (when (> (nyxt::suggestion-cursor minibuffer) 0)
    (decf (nyxt::suggestion-cursor minibuffer))
    (state-changed minibuffer)
    (update-suggestions-display minibuffer)
    (evaluate-script minibuffer
                     (ps:ps (ps:chain (ps:chain document (get-element-by-id "head"))
                                      (scroll-into-view false))))))

(define-command minibuffer-paste (&optional (minibuffer (current-minibuffer)))
  "Paste clipboard text to input."
  ;; See `paste' comment on why we use a new thread here.
  (bt:make-thread
   (lambda ()
     (trivial-clipboard:text (or (ignore-errors (trivial-clipboard:text)) ""))
     (ffi-within-renderer-thread
      *browser*
      (lambda ()
        (insert minibuffer (ring-insert-clipboard (nyxt::clipboard-ring *browser*))))))))

(define-command copy-suggestion (&optional (minibuffer (current-minibuffer)))
  "Copy suggestion to clipboard."
  (let ((suggestion (if (and (multi-selection-p minibuffer)
                            (not (null (nyxt::marked-suggestions minibuffer))))
                       (str:join (string #\newline) (get-marked-suggestions minibuffer))
                       (get-suggestion minibuffer))))
    (unless (str:emptyp suggestion)
      (trivial-clipboard:text suggestion))))

(define-command insert-suggestion (&optional (minibuffer (current-minibuffer)))
  "Paste selected suggestion to input.
As a special case, if the inserted suggestion is a URI, we decode it to make it
readable."
  (let ((suggestion (get-suggestion minibuffer)))
    (when suggestion
      (kill-whole-line minibuffer)
      (insert minibuffer
              (if (valid-url-p suggestion)
                  (quri:url-decode suggestion :lenient t)
                  suggestion)))))

(declaim (ftype (function (containers:ring-buffer-reverse))
                minibuffer-history-suggestion-filter))
(defun minibuffer-history-suggestion-filter (history)
  (when history
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer)
                   (delete-duplicates (containers:container->list history)
                                      :test #'equal)))))

(define-command minibuffer-history (&optional (minibuffer (current-minibuffer)))
  "Choose a minibuffer input history entry to insert as input."
  (when (history minibuffer)
    (let ((input (object-string
                  (prompt-minibuffer
                   :input-prompt "Input history"
                   :history nil
                   :suggestion-function (minibuffer-history-suggestion-filter (history minibuffer))))))
      (unless (str:empty? input)
        (log:debug input minibuffer)
        (text-buffer::kill-line (input-cursor minibuffer))
        (insert minibuffer input)))))

(define-command minibuffer-toggle-mark (&key
                                        (minibuffer (current-minibuffer))
                                        (direction :next))
  "Toggle suggestion.
Only available if minibuffer `multi-selection-p' is non-nil.  DIRECTION can be
:next or :previous and specifies which suggestion to select once done."
  (when (multi-selection-p minibuffer)
    (with-slots (nyxt::suggestions nyxt::suggestion-cursor nyxt::marked-suggestions) minibuffer
      (let ((suggestion (nth nyxt::suggestion-cursor nyxt::suggestions)))
        (match (member suggestion nyxt::marked-suggestions)
          ((guard n n) (setf nyxt::marked-suggestions (delete suggestion nyxt::marked-suggestions)))
          (_ (push suggestion nyxt::marked-suggestions)))))
    (state-changed minibuffer)
    (update-display minibuffer)
    (match direction
      (:next (select-next minibuffer))
      (:previous (select-previous minibuffer)))))

(define-command minibuffer-toggle-mark-backwards (&key (minibuffer (current-minibuffer)))
  "Toggle suggestion and select previous suggestion.
See `minibuffer-toggle-mark'. "
  (minibuffer-toggle-mark :minibuffer minibuffer :direction :previous))

(define-command minibuffer-mark-all (&optional (minibuffer (current-minibuffer)))
  "Mark all visible suggestions.
Only available if minibuffer `multi-selection-p' is non-nil."
  (when (multi-selection-p minibuffer)
    (with-slots (nyxt::suggestions nyxt::marked-suggestions) minibuffer
      (setf nyxt::marked-suggestions (union nyxt::suggestions nyxt::marked-suggestions)))
    (state-changed minibuffer)
    (update-display minibuffer)))

(define-command minibuffer-unmark-all (&optional (minibuffer (current-minibuffer)))
  "Unmark all visible suggestions.
Only available if minibuffer `multi-selection-p' is non-nil."
  (when (multi-selection-p minibuffer)
    (with-slots (nyxt::suggestions nyxt::marked-suggestions) minibuffer
      (setf nyxt::marked-suggestions (set-difference nyxt::marked-suggestions nyxt::suggestions)))
    (state-changed minibuffer)
    (update-display minibuffer)))

(define-command minibuffer-toggle-mark-all (&optional
                                            (minibuffer (current-minibuffer)))
  "Toggle the mark over all visible suggestions.
Only available if minibuffer `multi-selection-p' is non-nil."
  (when (multi-selection-p minibuffer)
    (with-slots ((suggestions nyxt::suggestions)
                 (marked-suggestions nyxt::marked-suggestions))
        minibuffer
      (cond ((subsetp marked-suggestions suggestions)
             (setf marked-suggestions
                   (set-difference suggestions marked-suggestions)))
            ((subsetp suggestions marked-suggestions)
             (setf marked-suggestions
                   (set-difference marked-suggestions suggestions)))
            (t ; the intersection of suggestions and marked-suggestions
               ; is non-trivial
             (setf marked-suggestions
                   (union suggestions marked-suggestions))))))
  (state-changed minibuffer)
  (update-display minibuffer))
