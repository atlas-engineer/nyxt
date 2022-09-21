;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/repl-mode
    (:documentation "Mode for programming in Common Lisp.

It has a multi-cell/panel/input environment that evaluates the inputted code on `evaluate-cell'.

Features:
- Creating additional cells using the bottom cell.
- Moving cells with `move-cell-down', `move-cell-up', and dedicated cell UI buttons.
- Basic tab-completion of the inputted symbols.
- Multiple evaluation results.
- Standard output recording.
- Binding results to the automatically-generated variables.
- Inline debugging (similar to Nyxt-native debugging with `*debug-on-error*' on.)"))
(in-package :nyxt/repl-mode)

(define-class evaluation ()
  ((id
    (nyxt::new-id)
    :type alex:non-negative-integer
    :documentation "Unique evaluation identifier.")
   (input
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
   (output
    nil
    :documentation "The text printed out during evaluation.")
   (ready-p
    nil
    :documentation "The evaluation is terminated.")
   (raised-condition
    nil
    :type (maybe ndebug:condition-wrapper)
    :documentation "The condition that was raised during the `input' execution."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod initialize-instance :after ((evaluation evaluation) &key)
  (run-thread "repl cell evaluation"
    (let ((nyxt::*interactive-p* t)
          (*standard-output* (make-string-output-stream))
          (*package* (find-package :nyxt-user)))
      (ndebug:with-debugger-hook
          (:wrapper-class 'nyxt::debug-wrapper
           :ui-display (setf (ready-p evaluation) t
                             (raised-condition evaluation) %wrapper%)
           :ui-cleanup (lambda (wrapper)
                         (declare (ignore wrapper))
                         (setf (raised-condition evaluation) nil)
                         (reload-current-buffer)))
        (with-input-from-string (input (input evaluation))
          (alex:lastcar
           (mapcar (lambda (s-exp)
                     (setf (results evaluation) (multiple-value-list (eval s-exp))
                           (output evaluation) (get-output-stream-string *standard-output*)))
                   (safe-slurp-stream-forms input))))))
    (setf (ready-p evaluation) t)
    (ps-eval (setf (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "#evaluation-result-~a" (id evaluation))))
                             |innerHTML|)
                   (ps:lisp (html-result evaluation))))))

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((rememberable-p nil)
   (keyscheme-map
    (define-keyscheme-map "repl-mode" ()
      keyscheme:default
      (list
       "C-return" 'evaluate-cell
       "tab" 'tab-complete-symbol)
      keyscheme:emacs
      (list
       "C-M-x" 'evaluate-cell
       "C-b" 'nyxt/input-edit-mode:cursor-backwards
       "C-f" 'nyxt/input-edit-mode:cursor-forwards
       "C-d" 'nyxt/input-edit-mode:delete-forwards
       "M-b" 'nyxt/input-edit-mode:cursor-backwards-word
       "M-f" 'nyxt/input-edit-mode:cursor-forwards-word
       "M-backspace" 'nyxt/input-edit-mode:delete-backwards-word
       "C-backspace" 'nyxt/input-edit-mode:delete-backwards-word
       "M-d" 'nyxt/input-edit-mode:delete-forwards-word
       "M-p" 'previous-cell
       "M-n" 'next-cell
       ;; FIXME: Org uses C-c C-_ and C-c C-^, but those are shadowed by C-c in Nyxt.
       "C-^" 'move-cell-up
       "C-_" 'move-cell-down)
      keyscheme:vi-normal
      (list
       ;; TODO: deleting chars/words
       "h" 'nyxt/input-edit-mode:cursor-backwards
       "l" 'nyxt/input-edit-mode:cursor-forwards
       "x" 'nyxt/input-edit-mode:delete-forwards
       "b" 'nyxt/input-edit-mode:cursor-backwards-word
       "w" 'nyxt/input-edit-mode:cursor-forwards-word
       "d b" 'nyxt/input-edit-mode:delete-backwards-word
       "d w" 'nyxt/input-edit-mode:delete-forwards-word
       "k" 'previous-cell
       "j" 'next-cell
       "K" 'move-cell-up
       "J" 'move-cell-down)))
   (style (theme:themed-css (theme *browser*)
            (*
             :font-family "monospace,monospace")
            ("#container"
             :display "flex"
             :flex-flow "column"
             :height "100%")
            (.input
             :background-color theme:accent
             :display "grid"
             :grid-template-columns "auto 1fr"
             :width "100%"
             :padding 0
             :margin "1em 0")
            (.prompt
             :color theme:on-accent
             :padding-right "4px"
             :padding-left "4px"
             :line-height "30px")
            (.input-buffer
             :color theme:on-accent
             :opacity "0.9"
             :border "none"
             :outline "none"
             :padding "3px"
             :autofocus "true"
             :width "100%")
            (.button
             :display "block")
            ("#evaluations"
             :font-size "12px"
             :flex-grow "1"
             :overflow-y "auto"
             :overflow-x "auto"))
          :documentation "The CSS applied to a REPL when it is set-up.")
   (evaluations
    (list)
    :documentation "A list of `evaluation's representing the current state of the REPL.")
   (current-evaluation
    nil
    :type (maybe integer)
    :reader t
    :documentation "Index of the evaluation currently being focused."))
  (:toggler-command-p nil))

(defun package-short-name (package)
  (first (sort (append (package-nicknames package)
                       (list (package-name package)))
               #'string<)))

(defmethod input ((mode repl-mode))
  (ps-eval :buffer (buffer mode) (ps:@ document active-element value)))

(defmethod (setf input) (new-text (mode repl-mode))
  (ps-labels :async t :buffer (buffer mode)
    ((set-input-text
      (text)
      (setf (ps:@ document active-element value) (ps:lisp text))))
    (set-input-text new-text)))

(defmethod cursor ((mode repl-mode))
  (let ((cursor (ps-eval :buffer (buffer mode)
                  (ps:chain (nyxt/ps:qs document "#input-buffer") selection-start))))
    (if (numberp cursor)
        cursor
        0)))

(defmethod (setf cursor) (new-position (mode repl-mode))
  (ps-labels :buffer (buffer mode)
    ((selection-start
      (position)
      (setf (ps:@ document active-element selection-start)
            (setf (ps:@ document active-element selection-end)
                  (ps:lisp position)))))
    (selection-start new-position)))

(define-parenscript focus (selector)
  (ps:chain (nyxt/ps:qs document (ps:lisp selector)) (focus)))

(defmethod input-focus-p (&optional (repl (find-submode 'repl-mode)))
  (declare (ignore repl))
  (string= "input-buffer" (ps-eval (ps:@ document active-element class-name))))

(defmethod (setf current-evaluation) (new-index (mode repl-mode))
  (if new-index
      (focus (format nil ".input-buffer[data-repl-id=\"~a\"]" new-index))
      (focus ".input-buffer[data-repl-id=\"\"]"))
  (setf (slot-value mode 'current-evaluation) new-index))

(defmethod current-cell-id ((mode repl-mode))
  (ps-labels :buffer (buffer mode)
    ((get-id () (ps:chain document active-element (get-attribute "data-repl-id"))))
    (ignore-errors (parse-integer (get-id)))))

(sera:eval-always
  (define-command evaluate-cell (&optional (repl (find-submode 'repl-mode)))
    "Evaluate the currently focused input cell."
    (let* ((input (input repl))
           (id (current-cell-id repl))
           (evaluation (make-instance 'evaluation :input input)))
      (unless (uiop:emptyp input)
        (if id
            (setf (elt (evaluations repl) id) evaluation
                  (current-evaluation repl) id)
            (setf (current-evaluation repl) (length (evaluations repl))
                  (evaluations repl) (append (evaluations repl) (list evaluation))))
        (reload-buffer (buffer repl))))))

(define-command previous-cell (&optional (repl (find-submode 'repl-mode)))
  "Move to the previous input cell."
  (let ((id (current-evaluation repl))
        (len (length (evaluations repl))))
    (cond
      ((zerop len)
       (setf (current-evaluation repl) nil))
      ((or (null id) (zerop id))
       (setf (current-evaluation repl) (1- len)))
      (t (setf (current-evaluation repl) (1- id))))))

(define-command next-cell (&optional (repl (find-submode 'repl-mode)))
  "Move to the next input cell."
  (let ((id (current-evaluation repl))
        (len (length (evaluations repl))))
    (cond
      ((or (zerop len)
           (null id)
           (= id (1- len)))
       (setf (current-evaluation repl) nil))
      (t (setf (current-evaluation repl) (1+ id))))))

(define-command move-cell-up (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Move the current code cell up, swapping it with the one above."
  (when (and id (not (zerop id)))
    (let* ((evals (evaluations repl)))
      (psetf (elt evals (1- id)) (elt evals id)
             (elt evals id) (elt evals (1- id))
             (current-evaluation repl) (1- id)))
    (reload-buffer (buffer repl))))

(define-command move-cell-down (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Move the current code cell down, swapping it with the one below."
  (when (and id (< id (1- (length (evaluations repl)))))
    (let* ((evals (evaluations repl)))
      (psetf (elt evals (1+ id)) (elt evals id)
             (elt evals id) (elt evals (1+ id))
             (current-evaluation repl) (1+ id)))
    (reload-buffer (buffer repl))))

;; FIXME: Those (`paren' and `closing-paren') often fail.
(define-command paren (&optional (repl (find-submode 'repl-mode)))
  ;; FIXME: Not an intuitive behavior? What does Emacs do?
  "Inserts the closing paren after the opening one is inputted."
  (when (input-focus-p repl)
    (let ((input (input repl))
          (cursor (cursor repl)))
      (setf (input repl) (str:insert "()" cursor input)
            (cursor repl) (1+ cursor)))))

(define-command closing-paren (&optional (repl (find-submode 'repl-mode)))
  "Automatically closes all the open parentheses before the cursor."
  (when (input-focus-p repl)
    (let* ((input (input repl))
           (cursor (cursor repl))
           (parens-to-complete (- (count #\( input :end cursor)
                                  (count #\) input))))
      (when (plusp parens-to-complete)
        (setf (input repl) (str:concat input (make-string parens-to-complete :initial-element #\)))))
      (alexandria:when-let ((cursor (ignore-errors (1+ (position #\) (input repl) :start cursor)))))
        (setf (cursor repl) cursor)))))

(defparameter +delimiters+
  (uiop:strcat "()"
               sera:whitespace)
  "Non-symbol Lisp delimiters.")

(define-command tab-complete-symbol (&optional (repl (find-submode 'repl-mode)))
  "Complete the current symbol and insert the completion into the current input area."
  (when (input-focus-p repl)
    (let* ((input (input repl))
           (cursor (cursor repl))
           (previous-delimiter (unless (= cursor 0)
                                 (position-if (lambda (c) (find c +delimiters+)) input
                                              :end cursor :from-end t)))
           (previous-delimiter (if previous-delimiter (1+ previous-delimiter) 0))
           (symbol-to-complete (subseq input previous-delimiter cursor))
           (completion (handler-case
                           (prompt1
                            :prompt "Symbol to complete"
                            :input symbol-to-complete
                            :sources (make-instance
                                      'prompter:source
                                      :name "Completions"
                                      :constructor (first (swank:simple-completions
                                                           symbol-to-complete *package*))))
                         (nyxt::prompt-buffer-canceled () nil))))
      (when completion
        (setf (input repl) (str:concat (subseq input 0 previous-delimiter)
                                       completion (subseq input cursor))
              (cursor repl) (+ cursor (- (length completion) (- cursor previous-delimiter))))))))

(defun html-result (evaluation)
  (spinneret:with-html-string
    (unless (uiop:emptyp (output evaluation))
      (:pre (output evaluation)))
    (cond
      ((and (ready-p evaluation)
            (raised-condition evaluation))
       (let ((wrapper (raised-condition evaluation)))
         (:pre (format nil "~a" (ndebug:condition-itself wrapper)))
         (dolist (restart (ndebug:restarts wrapper))
           (:button :class "button"
                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                     (:title "condition")
                                     (ndebug:invoke wrapper restart)))
                    (format nil "[~a] ~a" (dissect:name restart) (dissect:report restart))))
         (:h3 "Backtrace:")
         (:raw (nyxt::backtrace->html wrapper))))
      ((ready-p evaluation)
       (if (results evaluation)
           (loop
             for result in (results evaluation)
             for sub-order from 0
             for name = (if (serapeum:single (results evaluation))
                            (intern (format nil "V~d" (id evaluation)))
                            (intern (format nil "V~d.~d" (id evaluation) sub-order)))
             do (setf (symbol-value name) result)
             collect (:div
                      (format nil "~(~a~) = " name)
                      (:raw
                       (value->html result (or (typep result 'standard-object)
                                               (typep result 'structure-object))))))
           (:span "No values.")))
      (t (:span "Calculating...")))))

(define-internal-page-command-global lisp-repl ()
    (repl-buffer "*Lisp REPL*" 'repl-mode)
  "Show Lisp REPL."
  (let* ((repl-mode (find-submode 'nyxt/repl-mode:repl-mode repl-buffer))
         (evaluate-binding (nyxt::binding-keys 'evaluate-cell :modes (list repl-mode))))
    (spinneret:with-html-string
      (:head (:style (style repl-mode)))
      (:body
       (:div :id "container"
             (:div :id "evaluations"
                   (loop
                     for evaluation in (evaluations repl-mode)
                     for order from 0
                     collect (:div
                              :class "evaluation"
                              :id (format nil "evaluation-~a" (id evaluation))
                              (:div :class "input"
                                    (:span :class "prompt"
                                           (:button
                                            :onclick (ps:ps (nyxt/ps:lisp-eval
                                                             (:title "move-cell-up")
                                                             (move-cell-up :id order)))
                                            :title "Move this cell up."
                                            "↑")
                                           (:button
                                            :onclick (ps:ps (nyxt/ps:lisp-eval
                                                             (:title "move-cell-down")
                                                             (move-cell-down :id order)))
                                            :title "Move this cell down."
                                            "↓"))
                                    (:textarea :class "input-buffer" :data-repl-id order
                                               :onfocus
                                               (ps:ps (nyxt/ps:lisp-eval
                                                       (:title "set-current-evaluation")
                                                       (setf (slot-value (nyxt:current-mode :repl)
                                                                         'current-evaluation)
                                                             order)))
                                               (input evaluation)))
                              (:div :class "evaluation-result"
                                    :id (format nil "evaluation-result-~a" (id evaluation))
                                    (:raw (html-result evaluation)))))
                   (:div :class "input"
                         (:span :class "prompt" ">")
                         (:textarea :class "input-buffer"
                                    :id "input-buffer"
                                    :data-repl-id ""
                                    :placeholder (format nil "Press ~a to evaluate the Lisp expression"
                                                         evaluate-binding)))))))))
