;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/repl-mode
  (:use #:common-lisp #:nyxt)
  (:import-from #:keymap #:define-scheme)
  (:export :repl-mode))

(in-package :nyxt/repl-mode)

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "repl"
      scheme:cua
      (list
       "return" 'return-input
       "(" 'paren
       ")" 'closing-paren
       "tab" 'tab-complete-symbol
       "up" 'evaluation-history-previous
       "down" 'evaluation-history-next)
      scheme:emacs
      (list
       "C-f" 'nyxt/input-edit-mode:cursor-forwards
       "C-b" 'nyxt/input-edit-mode:cursor-backwards
       "M-f" 'nyxt/input-edit-mode:cursor-forwards-word
       "M-b" 'nyxt/input-edit-mode:cursor-backwards-word
       "C-d" 'nyxt/input-edit-mode:delete-forwards
       "M-backspace" 'nyxt/input-edit-mode:delete-backwards-word
       "M-d" 'nyxt/input-edit-mode:delete-forwards-word
       "M-p" 'evaluation-history-previous
       "M-n" 'evaluation-history-next)
      scheme:vi-normal
      (list
       ;; TODO: deleting chars/words
       ;; TODO: evaluation-history-(previous|next)
       "l" 'nyxt/input-edit-mode:cursor-forwards
       "h" 'nyxt/input-edit-mode:cursor-backwards
       "w" 'nyxt/input-edit-mode:cursor-forwards-word
       "b" 'nyxt/input-edit-mode:cursor-backwards-word
       "x" 'nyxt/input-edit-mode:delete-forwards
       "d b" 'nyxt/input-edit-mode:delete-backwards-word
       "d w" 'nyxt/input-edit-mode:delete-forwards-word)))
   (style (theme:themed-css (theme *browser*)
            (* :font-family "monospace,monospace")
            (body :margin-right "0")
            ("#container" :display "flex"
                          :flex-flow "column"
                          :height "100%"
                          :color theme:text
                          :background-color theme:background)
            ("#input" :display "grid"
                      :grid-template-columns "auto 1fr"
                      :width "100%"
                      :padding 0
                      :margin 0
                      :background-color theme:tertiary)
            ("#input-buffer" :width "100%"
                             :border "none"
                             :outline "none"
                             :padding "3px"
                             :background-color theme:quaternary
                             :autofocus "true")
            ("#evaluation-history"
             :font-size "12px"
             :flex-grow "1"
             :overflow-y "auto"
             :overflow-x "auto")
            ("#prompt" :padding-right "4px"
                       :padding-left "4px"
                       :line-height "30px"
                       :color theme:background)
            (ul :list-style "none"
                :padding "0"
                :margin "0")
            (li :padding "2px"))
          :documentation "The CSS applied to a REPL when it is set-up.")
   (evaluation-history (list)
                       :documentation "A list of pairs of (PACKAGE INPUT RESULTS).
INPUT is a string and RESULTS is a list of Lisp values.")
   (current-evaluation-history-element
    nil
    :type (or null integer)
    :documentation "Current element of history being edited in the REPL prompt.

Scroll history with `evaluation-history-previous' and `evaluation-history-next'."))
  (:toggler-command-p nil))

(defun package-short-name (package)
  (first (sort (append (package-nicknames package)
                       (list (package-name package)))
               #'string<)))

(defmethod input ((mode repl-mode))
  (with-current-buffer (buffer mode)
    (peval (ps:@ (nyxt/ps:qs document "#input-buffer") value))))

(defmethod (setf input) (new-text (mode repl-mode))
  (pflet ((set-input-text (text)
           (setf (ps:@ (nyxt/ps:qs document "#input-buffer") value) (ps:lisp text))))
    (with-current-buffer (buffer mode)
      (set-input-text new-text))))

(defmethod cursor ((mode repl-mode))
  (with-current-buffer (buffer mode)
    (let ((cursor (peval
                    (ps:chain (nyxt/ps:qs document "#input-buffer") selection-start))))
      (if (numberp cursor)
          cursor
          0))))

(defmethod (setf cursor) (new-position (mode repl-mode))
  (pflet ((selection-start (position)
           (setf (ps:chain (nyxt/ps:qs document "#input-buffer") selection-start)
                 (setf (ps:chain (nyxt/ps:qs document "#input-buffer") selection-end)
                       (ps:lisp position)))))
    (with-current-buffer (buffer mode)
      (selection-start new-position))))

(define-command return-input (&optional (repl (find-submode 'repl-mode)))
  "Return inputted text."
  (let ((input (input repl)))
    (add-object-to-evaluation-history repl
                                      (list (package-short-name *package*)
                                            input
                                            (nyxt::evaluate input)))
    ;; Reset history counter, as it doesn't make sense with new input.
    (setf (current-evaluation-history-element repl) nil)
    (reload-buffers (list (buffer repl)))))

(defmethod add-object-to-evaluation-history ((repl repl-mode) item)
  (push item (evaluation-history repl)))

(define-command paren (&optional (repl (find-submode 'repl-mode)))
  ;; FIXME: Not an intuitive behavior? What does Emacs do?
  "Inserts the closing paren after the opening one is inputted."
  (let ((input (input repl))
        (cursor (cursor repl)))
    (setf (input repl) (str:insert "()" cursor input)
          (cursor repl) (1+ cursor))))

(define-command closing-paren (&optional (repl (find-submode 'repl-mode)))
  "Automatically closes all the open parens before the cursor."
  (let* ((input (input repl))
         (cursor (cursor repl))
         (parens-to-complete (- (count #\( input :end cursor)
                                (count #\) input))))
    (when (plusp parens-to-complete)
      (setf (input repl) (str:concat input (make-string parens-to-complete :initial-element #\)))))
    (alexandria:when-let ((cursor (ignore-errors (1+ (position #\) (input repl) :start cursor)))))
      (setf (cursor repl) cursor))))

(define-command tab-complete-symbol (&optional (repl (find-submode 'repl-mode)))
  "Complete the current symbol and insert the completion into the REPL prompt."
  (let* ((input (input repl))
         (cursor (cursor repl))
         (previous-delimiter (unless (= cursor 0)
                               (position-if (lambda (c) (member c '(#\( #\) #\space))) input
                                           :end (1- cursor) :from-end t)))
         (previous-delimiter (if previous-delimiter (1+ previous-delimiter) 0))
         (symbol-to-complete (subseq input previous-delimiter cursor))
         (completion (handler-case
                         (prompt1 :prompt "Symbol to complete"
                           :input symbol-to-complete
                           :sources (list (make-instance
                                           'prompter:source
                                           :name "Completions"
                                           :constructor (first (swank:simple-completions
                                                                symbol-to-complete *package*)))))
                       (nyxt::nyxt-prompt-buffer-canceled () nil))))
    (when completion
      (setf (input repl) (str:concat (subseq input 0 previous-delimiter)
                                     completion (subseq input cursor))
            (cursor repl) (+ cursor (- (length completion) (- cursor previous-delimiter)))))))

(define-command evaluation-history-previous (&optional (repl (find-submode 'repl-mode)))
  "Fill REPL input with the value of the previous REPL history element."
  (let* ((current (1+ (or (current-evaluation-history-element repl) -1)))
         (elt (when (> (length (evaluation-history repl)) current)
                (elt (evaluation-history repl) current))))
    (if elt
        (setf (input repl) (second elt)
              (current-evaluation-history-element repl) current)
        (echo-warning "No more elements in evaluation history"))))

(define-command evaluation-history-next (&optional (repl (find-submode 'repl-mode)))
  "Fill REPL input with the value of the next REPL history element."
  (if (and (current-evaluation-history-element repl)
           (not (zerop (current-evaluation-history-element repl))))
      (setf (input repl) (elt (evaluation-history repl) (decf (current-evaluation-history-element repl))))
      (echo-warning "No more elements in evaluation history")))

(define-internal-page-command-global lisp-repl ()
    (repl-buffer "*Lisp REPL*" 'repl-mode)
  "Show Lisp REPL."
  (let ((repl-mode (find-submode 'nyxt/repl-mode:repl-mode repl-buffer)))
    (spinneret:with-html-string
      (:head (:style (style repl-mode)))
      (:body
       (:div :id "container"
             (:div :id "evaluation-history"
                   (:ul (loop
                          for (package input results) in (reverse (evaluation-history repl-mode))
                          collect (:li (:b package "> " input)
                                       (loop
                                         for result in results
                                         collect (:li (:raw
                                                       (value->html
                                                        result (or (typep result 'standard-object)
                                                                   (typep result 'structure-object))))))))))
             (:div :id "input"
                   (:span :id "prompt"
                          (format nil "~a>" (package-short-name *package*)))
                   (:input :type "text" :id "input-buffer"
                           :autofocus "autofocus")))))))
