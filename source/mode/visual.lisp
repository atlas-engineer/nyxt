;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/visual
  (:documentation "Package for `visual-mode', which enables keyboard-driven HTML
selection."))
(in-package :nyxt/mode/visual)

(define-mode visual-mode (nyxt/mode/hint:hint-mode)
  "Browse the page with a cursor to select text."
  ((nyxt/mode/hint:hints-selector
    "a, b, p, del, h1, h2, h3, h4, h5, h6, i, option, strong, sub, sup, listing,
xmp, plaintext, basefont, big, blink, center, font, marquee, multicol, nobr, s,
spacer, strike, tt, u, wbr, code, cite, pre"
    :type string)
   (keyscheme-map
    (define-keyscheme-map "visual-mode" ()
      keyscheme:cua
      (list
       "up" 'backward-line
       "down" 'forward-line
       "left" 'backward-char
       "right" 'forward-char
       "backspace" 'backward-char
       "space" 'forward-char
       "shift-left" 'backward-char-with-selection
       "shift-right" 'forward-char-with-selection
       "C-shift-left" 'backward-word-with-selection
       "C-shift-right" 'forward-word-with-selection
       "keypadhome" 'beginning-line
       "keypadend" 'end-line
       "shift-up" 'backward-line-with-selection
       "shift-down" 'forward-line-with-selection
       "C-shift-up" 'beginning-line-with-selection
       "C-shift-down" 'end-line-with-selection
       "escape" 'visual-mode
       "delete" 'clear-selection
       "C-c" 'visual-mode)
      keyscheme:emacs
      (list
       "C-p" 'backward-line
       "C-n" 'forward-line
       "C-b" 'backward-char
       "C-f" 'forward-char
       "M-b" 'backward-word
       "M-f" 'forward-word
       "M-a" 'backward-sentence
       "M-e" 'forward-sentence
       "M-{" 'backward-paragraph
       "M-}" 'forward-paragraph
       "C-h" 'select-paragraph
       "M-<" 'backward-document
       "M->" 'forward-document
       "C-a" 'beginning-line
       "C-e" 'end-line
       "C-g" 'visual-mode
       "shift-space" 'toggle-mark)
      ;; vi keybindings only enable use of vim's plain "visual" mode for now
      keyscheme:vi-normal
      (list
       "c" 'clear-selection
       "h" 'backward-char
       "l" 'forward-char
       "p" 'query-selection-in-search-engine
       "k" 'backward-line
       "j" 'forward-line
       "b" 'backward-word
       "w" 'forward-word
       "(" 'backward-sentence
       ")" 'forward-sentence
       "{" 'backward-paragraph
       "}" 'forward-paragraph
       "escape" 'visual-mode
       "g g" 'backward-document
       "G" 'forward-document
       "0" 'beginning-line
       "$" 'end-line
       "v" 'toggle-mark
       "y" 'nyxt/mode/document:copy
       "C-c" 'visual-mode)))
   (mark-set nil)))

(defmethod enable ((mode visual-mode) &key)
  (let ((buffer (buffer mode)))
    (make-page-editable buffer)
    (block-page-keypresses buffer)
    (select-paragraph mode)
    ;; imitating visual mode in vim
    (when (equal (keyscheme buffer) keyscheme:vi-normal)
      (setf (mark-set mode) t))))

(defmethod disable ((mode visual-mode) &key)
  (let ((buffer (buffer mode)))
    (make-page-uneditable buffer)
    (unlock-page-keypresses buffer)
    (setf (mark-set mode) nil)))

(defmethod prompter:object-attributes ((element nyxt/dom:text-element)
                                       (source prompter:source))
  `(("Hint" ,(plump:attribute element "nyxt-hint"))
    ("Text" ,(plump:text element))))

(defmethod %follow-hint (buffer (element nyxt/dom:text-element))
  (nyxt/dom:set-caret-on-start buffer element))

(defmethod caret-action ((mode visual-mode))
  (if (mark-set mode)
      :extend
      :move))

(define-parenscript block-page-keypresses (buffer)
  (setf (ps:@ window block-keypresses)
        (lambda (event)
          (ps:chain event (prevent-default))))
  (ps:chain window
            (add-event-listener "keydown"
                                (ps:@ window block-keypresses)
                                false)))

(define-parenscript unlock-page-keypresses (buffer)
  (ps:chain window
            (remove-event-listener "keydown"
                                   (ps:@ window block-keypresses)
                                   false)))

(define-parenscript make-page-editable (buffer)
  (when (ps:@ document body)
    (setf (ps:@ document body content-editable) "true")))

(define-parenscript make-page-uneditable (buffer)
  (when (ps:@ document body)
    (setf (ps:@ document body content-editable) "false")))

(define-command select-paragraph ((mode visual-mode))
  "Add hints to text elements on the page and query them."
  (let ((buffer (buffer mode)))
    (nyxt/mode/hint:query-hints buffer
                                "Set caret on element"
                                (lambda (results) (%follow-hint buffer (first results)))
                                :selector (nyxt/mode/hint:hints-selector mode))))

(define-parenscript collapsed-p (buffer)
  "Return T if mark's start and end are the same value, nil otherwise."
  (defun collapsed-p ()
    (let ((sel (ps:chain window (get-selection))))
      (ps:@ sel is-collapsed)))
  (collapsed-p))

(define-parenscript collapse-to-focus (buffer)
  "Collapse the selection"
  (let ((sel (ps:chain window (get-selection))))
    (ps:chain sel
              (collapse (ps:@ sel focus-node)
                        (ps:@ sel focus-offset)))))

(define-command toggle-mark ((mode visual-mode))
  "Toggle the mark."
  (let ((buffer (buffer mode)))
    (if (collapsed-p buffer)
        (progn
          (setf (mark-set mode) (not (mark-set mode)))
          (if (mark-set mode)
              (echo "Mark set")
              (echo "Mark deactivated")))
        (progn
          (collapse-to-focus buffer)
          (echo "Mark set")))))

(define-command clear-selection ((mode visual-mode))
  "Clear the selection and unset the mark."
  (let ((buffer (buffer mode)))
    (unless (collapsed-p buffer) (collapse-to-focus buffer))
    (setf (mark-set mode) nil)
    (echo "Mark deactivated")))

(define-parenscript caret-move (buffer &key action direction scale (n 1))
  (let ((sel (ps:chain window (get-selection))))
    (when sel
      (when (and (not (ps:@ sel focus-node)) (ps:@ document body))
        (ps:chain sel (collapse (ps:@ document body) 0)))
      (dotimes (i (ps:lisp n))
        (ps:chain sel (modify (ps:lisp action)
                              (ps:lisp direction)
                              (ps:lisp scale))))
      (let ((parent-el (and (ps:@ sel focus-node)
                            (ps:@ sel focus-node parent-element))))
        (when parent-el
          (ps:chain parent-el (scroll-into-view (ps:create block "nearest"))))))))

(defmacro define-caret-command (name direction scale documentation)
  `(define-command ,name ((mode visual-mode))
     ,documentation
     (caret-move (buffer mode)
                 :action (caret-action mode)
                 :direction ,direction
                 :scale ,scale)))

(define-caret-command forward-char :forward :character
  "Move caret forward by a character.")
(define-caret-command backward-char :backward :character
  "Move caret backward by a character.")
(define-caret-command forward-word :forward :word
  "Move caret forward by a word.")
(define-caret-command backward-word :backward :word
  "Move caret backward by a word.")
(define-caret-command forward-line :forward :line
  "Move caret forward by a line.")
(define-caret-command backward-line :backward :line
  "Move caret backward by a line.")
(define-caret-command beginning-line :backward :lineboundary
  "Move caret to the beginning of the line.")
(define-caret-command end-line :forward :lineboundary
  "Move caret to the end of the line.")
(define-caret-command forward-sentence :forward :sentence
  "Move caret forward to next end of sentence.")
(define-caret-command backward-sentence :backward :sentence
  "Move caret backward to start of sentence.")
(define-caret-command forward-paragraph :forward :paragraph
  "Move caret forward by a paragraph.")
(define-caret-command backward-paragraph :backward :paragraph
  "Move caret backward by a paragraph.")
(define-caret-command forward-document :forward :documentboundary
  "Move caret forward to the end of the document.")
(define-caret-command backward-document :backward :documentboundary
  "Move caret backward to the beginning of the document.")

(defmacro define-command-with-selection (name args &body body)
  (declare (ignore args))
  (multiple-value-bind (body decls doc)
      (alex:parse-body body :documentation t)
    `(define-command ,name ((mode visual-mode))
       ,@decls ,@(sera:unsplice doc)
       (setf (mark-set mode) t)
       ,@body)))

(define-command-with-selection forward-line-with-selection ()
  "Set mark and move caret forward by a line."
  (forward-line mode))

(define-command-with-selection backward-line-with-selection ()
  "Set mark and move caret backward by a line."
  (backward-line mode))

(define-command-with-selection forward-char-with-selection ()
  "Set mark and move caret forward by a character."
  (forward-char mode))

(define-command-with-selection backward-char-with-selection ()
  "Set mark and move caret backward by a character."
  (backward-char mode))

(define-command-with-selection forward-word-with-selection ()
  "Set mark and move caret forward by a word."
  (forward-word mode))

(define-command-with-selection backward-word-with-selection ()
  "Set mark and move caret backward by a word."
  (backward-word mode))

(define-command-with-selection beginning-line-with-selection ()
  "Set mark and move caret to the beginning of the line."
  (beginning-line mode))

(define-command-with-selection end-line-with-selection ()
  "Set mark and move caret to the end of line."
  (end-line mode))
