;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/repl-mode
    (:documentation "Mode for programming in Common Lisp."))
(in-package :nyxt/repl-mode)

(define-class evaluation ()
  ((input
    nil
    :type (maybe string)
    :documentation "User input.")
   (eval-package
    *package*
    :type package
    :documentation "The package that the evaluation happens in.")
   (results
    nil
    :documentation "The results (as a list) of `input' evaluation.")
   (raised-condition
    nil
    :type (maybe condition)
    :documentation "The condition that was raised during the `input' execution."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "repl"
      scheme:cua
      (list
       "C-return" 'evaluate-cell
       "(" 'paren
       ")" 'closing-paren
       "tab" 'tab-complete-symbol
       ;; TODO: Check out the Jupyter notebook bindings.
       )
      scheme:emacs
      (list
       "C-f" 'nyxt/input-edit-mode:cursor-forwards
       "C-b" 'nyxt/input-edit-mode:cursor-backwards
       "M-f" 'nyxt/input-edit-mode:cursor-forwards-word
       "M-b" 'nyxt/input-edit-mode:cursor-backwards-word
       "C-d" 'nyxt/input-edit-mode:delete-forwards
       "M-backspace" 'nyxt/input-edit-mode:delete-backwards-word
       "M-d" 'nyxt/input-edit-mode:delete-forwards-word
       "C-M-x" 'evaluate-cell
       "M-p" 'previous-cell
       "M-n" 'next-cell)
      scheme:vi-normal
      (list
       ;; TODO: deleting chars/words
       "l" 'nyxt/input-edit-mode:cursor-forwards
       "h" 'nyxt/input-edit-mode:cursor-backwards
       "w" 'nyxt/input-edit-mode:cursor-forwards-word
       "b" 'nyxt/input-edit-mode:cursor-backwards-word
       "x" 'nyxt/input-edit-mode:delete-forwards
       "d b" 'nyxt/input-edit-mode:delete-backwards-word
       "d w" 'nyxt/input-edit-mode:delete-forwards-word
       ;; TODO: Check out the VI bindings for such cases.
       )))
   (style (theme:themed-css (theme *browser*)
            (* :font-family "monospace,monospace")
            (body :margin-right "0")
            ("#container" :display "flex"
                          :flex-flow "column"
                          :height "100%"
                          :color theme:text
                          :background-color theme:background)
            (.input :display "grid"
                    :grid-template-columns "auto 1fr"
                    :width "100%"
                    :padding 0
                    :margin 0
                    :background-color theme:tertiary)
            (.input-buffer :width "100%"
                           :border "none"
                           :outline "none"
                           :padding "3px"
                           :background-color theme:quaternary
                           :autofocus "true")
            ("#evaluations"
             :font-size "12px"
             :flex-grow "1"
             :overflow-y "auto"
             :overflow-x "auto")
            (.prompt :padding-right "4px"
                     :padding-left "4px"
                     :line-height "30px"
                     :color theme:background))
          :documentation "The CSS applied to a REPL when it is set-up.")
   (evaluations
    (list)
    :documentation "A list of `evaluation's representing the current state of the REPL."))
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
           (setf (ps:@ document active-element value) (ps:lisp text))))
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
           (setf (ps:@ document active-element selection-start)
                 (setf (ps:@ document active-element selection-end)
                       (ps:lisp position)))))
    (with-current-buffer (buffer mode)
      (selection-start new-position))))

(defmethod current-cell-id ((mode repl-mode))
  (pflet ((get-id ()
           (ps:chain document active-element (get-attribute "data-repl-id"))))
    (with-current-buffer (buffer mode)
      (ignore-errors (parse-integer (get-id))))))

(define-command evaluate-cell (&optional (repl (find-submode 'repl-mode)))
  "Evaluate the currently focused input cell."
  (let* ((input (input repl))
         (id (current-cell-id repl))
         (evaluation (make-instance 'evaluation
                                    :input input
                                    :results (nyxt::evaluate input))))
    (if id
        (setf (elt (evaluations repl) id) evaluation)
        (setf (evaluations repl) (append (evaluations repl) (list evaluation))))
    ;; Reset history counter, as it doesn't make sense with new input.
    (reload-buffers (list (buffer repl)))))

(define-parenscript focus (selector)
  (ps:chain (nyxt/ps:qs document (ps:lisp selector)) (focus))
  (when (functionp (ps:chain (nyxt/ps:qs document (ps:lisp selector)) select))
    (ps:chain (nyxt/ps:qs document (ps:lisp selector)) (select))))

(define-command previous-cell (&optional (repl (find-submode 'repl-mode)))
  "Move to the previous input cell."
  (let ((id (current-cell-id repl))
        (len (length (evaluations repl))))
    (cond
      ((zerop len)
       (focus ".input-buffer[data-repl-id=\"\"]"))
      ((or (null id) (zerop id))
       (focus ".input-buffer[data-repl-id=\"0\"]"))
      (t (focus (format nil ".input-buffer[data-repl-id=\"~a\"]" (1- id)))))))

(define-command next-cell (&optional (repl (find-submode 'repl-mode)))
  "Move to the next input cell."
  (let ((id (current-cell-id repl))
        (len (length (evaluations repl))))
    (cond
      ((or (zerop len)
           (null id)
           (= id (1- len)))
       (focus ".input-buffer[data-repl-id=\"\"]"))
      (t (focus (format nil ".input-buffer[data-repl-id=\"~a\"]" (1+ id)))))))

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
  "Complete the current symbol and insert the completion into the current input area."
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

(define-internal-page-command-global lisp-repl ()
    (repl-buffer "*Lisp REPL*" 'repl-mode)
  "Show Lisp REPL."
  (let ((repl-mode (find-submode 'nyxt/repl-mode:repl-mode repl-buffer)))
    (spinneret:with-html-string
      (:head (:style (style repl-mode)))
      (:body
       (:div :id "container"
             (:div :id "evaluations"
                   (loop
                     for evaluation in (evaluations repl-mode)
                     for order from 0 by 1
                     collect (:div :class "input"
                                   (:span :class "prompt" (package-short-name (eval-package evaluation)) "> ")
                                   (:input :class "input-buffer" :data-repl-id order :type "text"
                                           :value (input evaluation)))
                     collect (loop
                               for result in (results evaluation)
                               collect (:raw
                                        (value->html
                                         result (or (typep result 'standard-object)
                                                    (typep result 'structure-object))))
                               collect (:br)))
                   (:div :class "input"
                         (:span :class "prompt" (package-short-name *package*) "> ")
                         (:input :class "input-buffer"
                                 :data-repl-id ""
                                 :type "text" :value ""
                                 :placeholder "Input some Lisp expression here"))))))))
