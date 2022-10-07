;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/repl-mode
    (:documentation "Mode for programming in Common Lisp.

It has a multi-cell/panel/input environment that evaluates the inputted code on `evaluate-cell'.

Features:
- Creating additional cells using the `add-cell' and a dedicated button.
- Reformatting the code using the compiler facilities for
  pretty-printing (`reformat-cell' and a dedicated button).
- Moving cells with `move-cell-down', `move-cell-up', and dedicated cell buttons.
- Cleaning the cell contents with `clean-cell' and a dedicated button.
- Removing unneeded cells with `delete-cell' and dedicated cell buttons.
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
    :accessor nil
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

(defmethod input ((evaluation evaluation) &key &allow-other-keys)
  (slot-value evaluation 'input))

(defmethod (setf input) ((value string) (evaluation evaluation) &key &allow-other-keys)
  (setf (slot-value evaluation 'input)
        value))

(defmethod initialize-instance :after ((evaluation evaluation) &key)
  (run-thread "repl cell evaluation"
    (let ((nyxt::*interactive-p* t)
          (*standard-output* (make-string-output-stream))
          (*package* (eval-package evaluation)))
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
       "C-_" 'move-cell-down
       "C-k" 'clean-cell
       "C-M-k" 'delete-cell
       "M-q" 'reformat-cell)
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
             :height "95vh"
             :width "95vw")
            (.input
             :background-color theme:secondary
             :width "99%"
             :padding 0
             :margin "1em 0")
            (.button
             :display "inline")
            (.input-buffer
             :color theme:on-accent
             :opacity "0.9"
             :border "none"
             :outline "none"
             :padding "3px"
             :margin "3px"
             :autofocus "true"
             :width "99%")
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
               #'<
               :key #'length)))

(defmethod input ((mode repl-mode) &key (id (current-evaluation mode)))
  (ps-eval :buffer (buffer mode)
    (ps:@ (or (nyxt/ps:qs document (ps:lisp (format nil ".input-buffer[data-repl-id=\"~a\"]" id)))
              (ps:@ document active-element))
          value)))

(defmethod (setf input) (new-text (mode repl-mode) &key (id (current-evaluation mode)))
  (ps-eval :async t :buffer (buffer mode)
    (setf (ps:@ (or (nyxt/ps:qs document (ps:lisp (format nil ".input-buffer[data-repl-id=\"~a\"]" id)))
                    (ps:@ document active-element))
                value)
          (ps:lisp new-text))))

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

(defun cell-package (id mode)
  (find-package
   (ps-eval :buffer (buffer mode)
     (ps:chain (nyxt/ps:qs document
                           (ps:lisp (format nil ".input-buffer[data-repl-id=\"~a\"] ~~ select"
                                            id)))
               value))))

(defmethod current-cell-package ((mode repl-mode))
  (cell-package (current-evaluation mode) mode))

(sera:eval-always
  (define-command evaluate-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
    "Evaluate the currently focused input cell."
    (let* ((input (input repl :id id))
           (evaluation (make-instance 'evaluation
                                      :input input
                                      :eval-package (cell-package id repl))))
      (unless (uiop:emptyp input)
        (setf (elt (evaluations repl) id) evaluation
              (current-evaluation repl) id)
        (reload-buffer (buffer repl))))))

(define-command add-cell (&key (repl (find-submode 'repl-mode)) id)
  "Add a new cell ready for evaluation and movement."
  (setf (evaluations repl)
        (if (and id (not (minusp id)))
            ;; Does CL have some kind of 'insert' function for that?
            (append (subseq (evaluations repl) 0 id)
                    (list (make-instance 'evaluation :input ""))
                    (subseq (evaluations repl) id))
            (append (evaluations repl)
                    (list (make-instance 'evaluation :input "")))))
  (reload-buffer (buffer repl)))

(define-command clean-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Clean the cell with ID (or current one, if not provided), removing all input and state is accumulated."
  (setf (elt (evaluations repl) id)
        (make-instance 'evaluation :input ""))
  (reload-buffer (buffer repl)))

(define-command delete-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Remove the cell with ID (or current one, if not provided) from the REPL."
  (setf (evaluations repl)
        (remove (elt (evaluations repl) id) (evaluations repl)))
  (reload-buffer (buffer repl)))

(defun format-form (form package)
  (let ((*print-readably* t)
        (*print-pretty* t)
        (*print-case* :downcase)
        (*package* package))
    (write-to-string form)))

(define-command reformat-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Reformat the cell input according to what compiler find aesthetically pleasing."
  (handler-case
      (progn
        (setf (input (elt (evaluations repl) id))
              (format-form (read-from-string (input repl :id id))
                           (eval-package (elt (evaluations repl) id))))
        (reload-buffer (buffer repl)))
    (reader-error ()
      (echo "The input appears malformed. Stop reformatting."))))

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
  (uiop:strcat "()" sera:whitespace)
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
           (:button.button
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

(define-internal-page-command-global repl ()
    (repl-buffer "*REPL*" 'repl-mode)
  "Show Nyxt REPL, a multi-pane environment to experiment with code."
  (let* ((repl-mode (find-submode 'nyxt/repl-mode:repl-mode repl-buffer))
         (evaluate-binding (nyxt::binding-keys 'evaluate-cell :modes (list repl-mode))))
    (spinneret:with-html-string
      (:style (style repl-mode))
      (:div :id "container"
            (:div :id "evaluations"
                  (loop
                    for evaluation in (evaluations repl-mode)
                    for order from 0
                    collect (:div
                             :class "evaluation"
                             :id (format nil "evaluation-~a" (id evaluation))
                             (:div :class "input"
                                   (:textarea :class "input-buffer" :data-repl-id order
                                              :rows (length (str:lines (input evaluation) :omit-nulls nil))
                                              :onfocus
                                              (ps:ps (nyxt/ps:lisp-eval
                                                      (:title "set-current-evaluation")
                                                      (setf (slot-value (nyxt:current-mode :repl)
                                                                        'current-evaluation)
                                                            order)))
                                              (input evaluation))
                                   (:br)
                                   (:button.button.accent
                                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                                     (:title "evaluate-cell")
                                                     (evaluate-cell :id order)))
                                    :title "Evaluate the current cell and show the result below."
                                    "Evaluate")
                                   (:select.button
                                    :onchange (ps:ps (nyxt/ps:lisp-eval
                                                      (:title "change-evaluation-package")
                                                      (setf (eval-package (elt (evaluations repl-mode) order))
                                                            (cell-package order repl-mode))))
                                    (dolist (package (append (nyxt::nyxt-packages)
                                                             (nyxt::nyxt-user-packages)
                                                             (sort
                                                              (set-difference
                                                               (set-difference
                                                                (list-all-packages)
                                                                (nyxt::nyxt-packages))
                                                               (nyxt::nyxt-user-packages))
                                                              #'string<
                                                              :key #'package-name)))
                                      (:option :value (package-name package)
                                               :selected (eq package (eval-package evaluation))
                                               (package-short-name package))))
                                   (:button.button
                                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                                     (:title "reformat-cell")
                                                     (reformat-cell :id order)))
                                    :title "Re-indent the cell contents in accordance with compiler aesthetics."
                                    "Reformat")
                                   (:button.button
                                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                                      (:title "add-cell-below")
                                                      (add-cell :id (1+ order))))
                                     :title "Add a new empty cell below this one."
                                     "Add cell below")
                                    (:button.button
                                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                                      (:title "clean-cell")
                                                      (clean-cell :id order)))
                                     :title "Clean the cell contents."
                                     "🧹 Clean")
                                   (:button.button
                                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                                     (:title "move-cell-up")
                                                     (move-cell-up :id order)))
                                    :title "Move this cell up."
                                    "↑ Up")
                                   (:button.button
                                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                                     (:title "move-cell-down")
                                                     (move-cell-down :id order)))
                                    :title "Move this cell down."
                                    "↓ Down")
                                   (:button.button
                                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                                     (:title "delete-cell")
                                                     (delete-cell :id order)))
                                    :title "Remove this cell from the REPL."
                                    "✕ Delete"))
                             (:div :class "evaluation-result"
                                   :id (format nil "evaluation-result-~a" (id evaluation))
                                   (:raw (html-result evaluation)))))
                  (:button.button
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    (:title "add-cell")
                                    (add-cell)))
                   :title "Add a new cell for you to evaluate code in."
                   "+ Add a cell")
                  (:button.button
                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                     (:title "delete-all-cells")
                                     (setf (evaluations repl-mode) nil)
                                     (reload-buffer (buffer repl-mode))))
                    :title "Delete all cells in this pane."
                    "✕ Delete all"))))))
