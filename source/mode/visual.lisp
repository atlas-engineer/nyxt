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
    (select-paragraph buffer)
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

(define-command select-paragraph (buffer)
  "Add hints to text elements on the page and query them."
  (when-let ((mode (find-mode buffer 'visual-mode)))
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

(define-command toggle-mark (buffer)
  "Toggle the mark."
  (let ((mode (find-mode buffer 'visual-mode)))
    (if (collapsed-p buffer)
        (progn
          (setf (mark-set mode) (not (mark-set mode)))
          (if (mark-set mode)
              (echo "Mark set")
              (echo "Mark deactivated")))
        (progn
          (collapse-to-focus buffer)
          (echo "Mark set")))))

(define-command clear-selection (buffer)
  "Clear the selection and unset the mark."
  (let ((mode (find-mode buffer 'visual-mode)))
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

(define-command forward-char (buffer)
  "Move caret forward by a character."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :forward
                :scale :character)))

(define-command backward-char (buffer)
  "Move caret backward by a character."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :backward
                :scale :character)))

(define-command forward-word (buffer)
  "Move caret forward by a word."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :forward
                :scale :word)))

(define-command backward-word (buffer)
  "Move caret backward by a word."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :backward
                :scale :word)))

(define-command forward-line (buffer)
  "Move caret forward by a line."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :forward
                :scale :line)))

(define-command backward-line (buffer)
  "Move caret backward by a line."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :backward
                :scale :line)))

(define-command beginning-line (buffer)
  "Move caret to the beginning of the line."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :backward
                :scale :lineboundary)))

(define-command end-line (buffer)
  "Move caret to the end of the line."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :forward
                :scale :lineboundary)))

(define-command forward-sentence (buffer)
  "Move caret forward to next end of sentence."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :forward
                :scale :sentence)))

(define-command backward-sentence (buffer)
  "Move caret backward to start of sentence."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :backward
                :scale :sentence)))

(define-command forward-paragraph (buffer)
  "Move caret forward by a paragraph."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :forward
                :scale :paragraph)))

(define-command backward-paragraph (buffer)
  "Move caret backward by a paragraph."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :backward
                :scale :paragraph)))

(define-command forward-document (buffer)
  "Move caret forward to the end of the document."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :forward
                :scale :documentboundary)))

(define-command backward-document (buffer)
  "Move caret backward to the beginning of the document."
  (let ((mode (find-mode buffer 'visual-mode)))
    (caret-move buffer
                :action (caret-action mode)
                :direction :backward
                :scale :documentboundary)))

(defmacro define-command-with-selection (name args &body body)
  (declare (ignore args))
  (alex:with-gensyms (mode)
    (multiple-value-bind (body decls doc)
        (alex:parse-body body :documentation t)
      `(define-command ,name (buffer)
         ,@decls ,@(sera:unsplice doc)
         (let ((,mode (find-mode buffer 'visual-mode)))
           (setf (mark-set ,mode) t)
           ,@body)))))

(define-command-with-selection forward-line-with-selection ()
  "Set mark and move caret forward by a line."
  (forward-line buffer))

(define-command-with-selection backward-line-with-selection ()
  "Set mark and move caret backward by a line."
  (backward-line buffer))

(define-command-with-selection forward-char-with-selection ()
  "Set mark and move caret forward by a character."
  (forward-char buffer))

(define-command-with-selection backward-char-with-selection ()
  "Set mark and move caret backward by a character."
  (backward-char buffer))

(define-command-with-selection forward-word-with-selection ()
  "Set mark and move caret forward by a word."
  (forward-word buffer))

(define-command-with-selection backward-word-with-selection ()
  "Set mark and move caret backward by a word."
  (backward-word buffer))

(define-command-with-selection beginning-line-with-selection ()
  "Set mark and move caret to the beginning of the line."
  (beginning-line buffer))

(define-command-with-selection end-line-with-selection ()
  "Set mark and move caret to the end of line."
  (end-line buffer))
