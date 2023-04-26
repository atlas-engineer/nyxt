;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/visual
  (:documentation "Visual mode."))
(in-package :nyxt/mode/visual)

(define-mode visual-mode (nyxt/mode/hint:hint-mode)
  "Visual mode. For documentation on commands and keybindings, see the manual."
  ((rememberable-p nil)
   (nyxt/mode/hint:hints-selector
    "a, b, p, del, h1, h2, h3, h4, h5, h6, i, option,
strong, sub, sup, listing, xmp, plaintext, basefont, big, blink, center, font,
marquee, multicol, nobr, s, spacer, strike, tt, u, wbr, code, cite, pre"
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
       "shift-space" 'toggle-mark
       "C-space" 'toggle-mark)
      ;; vi keybindings only enable use of vim's plain "visual" mode for now
      keyscheme:vi-normal
      (list
       "h" 'backward-char
       "l" 'forward-char
       "k" 'backward-line
       "j" 'forward-line
       "b" 'backward-word
       "w" 'forward-word
       "(" 'backward-sentence
       ")" 'forward-sentence
       "{" 'backward-paragraph
       "}" 'forward-paragraph
       "g g" 'backward-document
       "G" 'forward-document
       "0" 'beginning-line
       "$" 'end-line
       "v" 'toggle-mark
       "C-c" 'visual-mode)))
   (mark-set nil)))

(defmethod enable ((mode visual-mode) &key)
  (make-page-editable)
  (block-page-keypresses)
  (select-paragraph mode)
  ;; imitating visual mode in vim
  (when (equal (keyscheme (buffer mode)) keyscheme:vi-normal)
    (setf (mark-set mode) t)))

(defmethod disable ((mode visual-mode) &key)
  (make-page-uneditable)
  (unlock-page-keypresses)
  (setf (mark-set mode) nil))

(defmethod prompter:object-attributes ((element nyxt/dom:text-element) (source prompter:source))
  `(("Hint" ,(plump:attribute element "nyxt-hint"))
    ("Text" ,(plump:text element))))

(defmethod %follow-hint ((element nyxt/dom:text-element))
  (nyxt/dom:set-caret-on-start element))

(defmethod caret-action ((mode visual-mode))
  (if (mark-set mode)
      :extend
      :move))

(define-parenscript block-page-keypresses ()
  (setf (ps:@ window block-keypresses)
        (lambda (event)
          (ps:chain event (prevent-default))))
  (ps:chain window
            (add-event-listener "keydown"
                                (ps:@ window block-keypresses)
                                false)))

(define-parenscript unlock-page-keypresses ()
  (ps:chain window
            (remove-event-listener "keydown"
                                   (ps:@ window block-keypresses)
                                   false)))

(define-parenscript make-page-editable ()
  (setf (ps:@ document body content-editable) "true"))

(define-parenscript make-page-uneditable ()
  (setf (ps:@ document body content-editable) "false"))

(define-command select-paragraph (&optional (mode (find-submode 'visual-mode)))
  "Add hints to text elements on the page and query them."
  (nyxt/mode/hint:query-hints "Set caret on element"
                              (lambda (results) (%follow-hint (first results)))
                              :selector (nyxt/mode/hint:hints-selector mode)))

(define-parenscript collapsed-p ()
  "Return T if mark's start and end are the same value, nil otherwise."
  (defun collapsed-p ()
    (let ((sel (ps:chain window (get-selection))))
      (ps:@ sel is-collapsed)))
  (collapsed-p))

(define-parenscript collapse-to-focus ()
  "Collapse the selection"
  (let ((sel (ps:chain window (get-selection))))
    (ps:chain sel
              (collapse (ps:@ sel focus-node)
                        (ps:@ sel focus-offset)))))

(define-command toggle-mark ()
  "Toggle the mark."
  (let ((mode (find-submode 'visual-mode)))
    (if (collapsed-p)
        (progn
          (setf (mark-set mode) (not (mark-set mode)))
          (if (mark-set mode)
              (echo "Mark set")
              (echo "Mark deactivated")))
        (progn
          (collapse-to-focus)
          (echo "Mark set")))))

(define-command clear-selection ()
  "Clear the selection and unset the mark."
  (let ((mode (find-submode 'visual-mode)))
    (unless (collapsed-p) (collapse-to-focus))
    (setf (mark-set mode) nil)
    (echo "Mark deactivated")))

(define-parenscript caret-move (&key action direction scale (n 1))
  (let ((sel (ps:chain window (get-selection)))
        (parent-el (ps:chain window (get-selection) focus-node parent-element)))
    (ps:chain parent-el (scroll-into-view (ps:create block "nearest")))
    (dotimes (i (ps:lisp n))
      (ps:chain sel (modify (ps:lisp action)
                            (ps:lisp direction)
                            (ps:lisp scale))))))

(define-command forward-char ()
  "Move caret forward by a character."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :forward
                :scale :character)))

(define-command backward-char ()
  "Move caret backward by a character."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :backward
                :scale :character)))

(define-command forward-word ()
  "Move caret forward by a word."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :forward
                :scale :word)))

(define-command backward-word ()
  "Move caret backward by a word."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :backward
                :scale :word)))

(define-command forward-line ()
  "Move caret forward by a line."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :forward
                :scale :line)))

(define-command backward-line ()
  "Move caret backward by a line."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :backward
                :scale :line)))

(define-command beginning-line ()
  "Move caret to the beginning of the line."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :backward
                :scale :lineboundary)))

(define-command end-line ()
  "Move caret to the end of the line."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :forward
                :scale :lineboundary)))

(define-command forward-sentence ()
  "Move caret forward to next end of sentence."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :forward
                :scale :sentence)))

(define-command backward-sentence ()
  "Move caret backward to start of sentence."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :backward
                :scale :sentence)))

(define-command forward-paragraph ()
  "Move caret forward by a paragraph."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :forward
                :scale :paragraph)))

(define-command backward-paragraph ()
  "Move caret backward by a paragraph."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :backward
                :scale :paragraph)))

(define-command forward-document ()
  "Move caret forward to the end of the document."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :forward
                :scale :documentboundary)))

(define-command backward-document ()
  "Move caret backward to the beginning of the document."
  (let ((mode (find-submode 'visual-mode)))
    (caret-move :action (caret-action mode)
                :direction :backward
                :scale :documentboundary)))

(defmacro define-command-with-selection (name args &body body)
  (declare (ignore args))
  (alex:with-gensyms (mode)
    (multiple-value-bind (body decls doc)
      (alex:parse-body body :documentation t)
        `(define-command ,name ()
           ,@decls ,@(sera:unsplice doc)
           (let ((,mode (find-submode 'visual-mode)))
             (setf (mark-set ,mode) t)
             ,@body)))))

(define-command-with-selection forward-line-with-selection ()
  "Set mark and move caret forward by a line."
  (forward-line))

(define-command-with-selection backward-line-with-selection ()
  "Set mark and move caret backward by a line."
  (backward-line))

(define-command-with-selection forward-char-with-selection ()
  "Set mark and move caret forward by a character."
  (forward-char))

(define-command-with-selection backward-char-with-selection ()
  "Set mark and move caret backward by a character."
  (backward-char))

(define-command-with-selection forward-word-with-selection ()
  "Set mark and move caret forward by a word."
  (forward-word))

(define-command-with-selection backward-word-with-selection ()
  "Set mark and move caret backward by a word."
  (backward-word))

(define-command-with-selection beginning-line-with-selection ()
  "Set mark and move caret to the beginning of the line."
  (beginning-line))

(define-command-with-selection end-line-with-selection ()
  "Set mark and move caret to the end of line."
  (end-line))
