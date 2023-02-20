;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/repl-mode
    (:documentation "Common Lisp REPL mode for interactive programming.

The interface is composed by cell pairs of input and output that evaluate CL
symbolic expressions.

Features:
- Input/output cells pairs can be added and removed.
- Reformat code using the compiler facilities.
- Re-order cell pairs.
- Clean cell input contents.
- Symbol completion.
- Multiple evaluation results.
- Standard output recording.
- Binding results to the automatically-generated variables.
- Inline debugging (similar to Nyxt-native debugging when `*debug-on-error*')
- Both keyboard and mouse oriented UIs."))
(in-package :nyxt/repl-mode)

(define-class evaluation ()
  ((id
    (nyxt::new-id)
    :type alex:non-negative-integer
    :documentation "Unique evaluation identifier.")
   (mode-instance
    ;; If we use (find-submode 'repl-mode), compiler will scream at us that
    ;; 'repl-mode is not a mode symbol, because repl-mode is not yet defined.
    (current-mode :repl)
    :type mode
    :documentation "Repl-mode instance this evaluation belong to")
   (input
    nil
    :accessor nil
    :type (maybe string)
    :documentation "User input.")
   (eval-package
    *package*
    :type package
    :documentation "The package that the evaluation happens in.")
   (thread
    nil
    :export nil
    :type (maybe bt:thread)
    :documentation "The thread that evaluation happens on.")
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
    :documentation "The condition raised during the `input' execution."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod input ((evaluation evaluation) &key &allow-other-keys)
  (slot-value evaluation 'input))

(defmethod (setf input) ((value string) (evaluation evaluation) &key &allow-other-keys)
  (setf (slot-value evaluation 'input)
        value))

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((keyscheme-map
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
            `(*
              :font-family "monospace,monospace")
            `(body
              :margin "0")
            `("#container"
              :display "flex"
              :flex-flow "column"
              :height "95vh"
              :width "97vw"
              :margin "1em")
            `(.input-area
              :background-color ,theme:secondary
              :width "99%"
              :padding 0
              :margin "1em 0")
            `(.button
              :display "inline")
            `(.input-buffer
              :color ,theme:on-accent
              :opacity "0.9"
              :border "none"
              :outline "none"
              :padding "3px"
              :margin "3px"
              :autofocus "true"
              :width "99%")
            `("#evaluations"
              :font-size "12px"
              :flex-grow "1"
              :overflow-y "auto"
              :overflow-x "auto")
            `(.controls
              :position "absolute"
              :bottom "1em"
              :right "1em"))
          :documentation "The CSS applied to a REPL when it is set-up.")
   (evaluations
    (list (make-instance 'evaluation :input "\"Hello, Nyxt!\""))
    :documentation "A list of `evaluation's representing the current state of the REPL.")
   (current-evaluation
    nil
    :type (maybe integer)
    :reader t
    :documentation "Index of the evaluation currently being focused."))
  (:toggler-command-p nil))

(defmethod initialize-instance :after ((evaluation evaluation) &key &allow-other-keys)
  (unless (ready-p evaluation)
    (setf (thread evaluation)
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
                                 (reload-buffer (buffer (mode-instance evaluation)))))
                (with-input-from-string (input (input evaluation))
                  (alex:lastcar
                   (mapcar (lambda (s-exp)
                             (setf (results evaluation) (multiple-value-list (eval s-exp))
                                   (output evaluation) (get-output-stream-string *standard-output*)))
                           (safe-slurp-stream-forms input))))))
            (setf (ready-p evaluation) t)
            (reload-buffer (buffer (mode-instance evaluation)))))))

(defun package-short-name (package)
  (first (sort (append (package-nicknames package)
                       (list (package-name package)))
               #'<
               :key #'length)))

(defmethod input ((mode repl-mode) &key (id (current-evaluation mode)))
  (ps-eval :buffer (buffer mode)
    (let ((result (ps:@ (or (nyxt/ps:qs document (ps:lisp (format nil ".input-buffer[data-repl-id=\"~a\"]" id)))
                            (nyxt/ps:active-element document))
                        value)))
      (if (or (null result) (ps:undefined result))
          ""
          result))))

(defmethod (setf input) (new-text (mode repl-mode) &key (id (current-evaluation mode)))
  (ps-eval :async t :buffer (buffer mode)
    (setf (ps:@ (or (nyxt/ps:qs document (ps:lisp (format nil ".input-buffer[data-repl-id=\"~a\"]" id)))
                    (nyxt/ps:active-element document))
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
      (setf (ps:@ (nyxt/ps:active-element document) selection-start)
            (setf (ps:@ (nyxt/ps:active-element document) selection-end)
                  (ps:lisp position)))))
    (selection-start new-position)))

(define-parenscript focus (selector)
  (ps:chain (nyxt/ps:qs document (ps:lisp selector)) (focus)))

(defmethod input-focus-p (&optional (repl (find-submode 'repl-mode)))
  (declare (ignore repl))
  (string= "input-buffer" (ps-eval (ps:@ (nyxt/ps:active-element document) class-name))))

(defmethod (setf current-evaluation) (new-index (mode repl-mode))
  (if new-index
      (focus (format nil ".input-buffer[data-repl-id=\"~a\"]" new-index))
      (focus ".input-buffer[data-repl-id=\"\"]"))
  (setf (slot-value mode 'current-evaluation) new-index))

(defmethod current-cell-id ((mode repl-mode))
  (ps-labels :buffer (buffer mode)
    ((get-id () (ps:chain (nyxt/ps:active-element document) (get-attribute "data-repl-id"))))
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

(defun focus-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  (when (<= 0 id (1- (length (evaluations repl))))
    (setf (slot-value repl 'current-evaluation) id)))

(sera:eval-always
  (define-command evaluate-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
    "Evaluate the currently focused input cell."
    (focus-cell :id id)
    (let* ((input (input repl :id id))
           (evaluation (make-instance 'evaluation
                                      :mode-instance repl
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
  (when id
    (focus-cell :id id))
  (reload-buffer (buffer repl)))

(define-command clean-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Clean the cell with ID (or current one, if not provided), removing all input and accumulated state."
  (focus-cell :id id)
  (setf (elt (evaluations repl) id)
        (make-instance 'evaluation :input ""))
  (reload-buffer (buffer repl)))

(define-command delete-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Remove the cell with ID (or current one, if not provided) from the REPL."
  (focus-cell :id (1- id))
  (setf (evaluations repl)
        (remove (elt (evaluations repl) id) (evaluations repl)))
  (reload-buffer (buffer repl)))

(defun format-form (form package)
  (prini-to-string form :readably t :package package))

(define-command reformat-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Reformat the cell's input.

Follows what the compiler finds aesthetically pleasing."
  (focus-cell :id id)
  (handler-case
      (progn
        (setf (input (elt (evaluations repl) id))
              (format-form (read-from-string (input repl :id id))
                           (eval-package (elt (evaluations repl) id))))
        (reload-buffer (buffer repl)))
    (error (e)
      (echo "The input appears malformed. Stop reformatting. Original message: ~a" e))))

(define-command add-cell-to-auto-config (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Add cell contents to auto-config for further loading."
  (let ((evaluation (elt (evaluations repl) id))
        (auto-config (files:expand nyxt::*auto-config-file*)))
    (ensure-file-exists auto-config)
    (alex:write-string-into-file +newline+ auto-config :if-exists :append)
    (alex:write-string-into-file
     (format-form (uiop:safe-read-from-string (input evaluation) :package (eval-package evaluation))
                  :nyxt-user)
     auto-config :if-exists :append)
    (echo "Saved form into ~a" auto-config)))

(define-command previous-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Navigate to the previous input cell."
  (unless (or (null id)
              (zerop (length (evaluations repl))))
    (focus-cell :id (1- id))))

(define-command next-cell (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Navigate to the next input cell."
  (unless (or (null id)
              (zerop (length (evaluations repl))))
    (focus-cell :id (1+ id))))

(define-command move-cell-up (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Move the current code cell up, swapping it with the one above."
  (when (and id (not (zerop id)))
    (let ((evals (evaluations repl)))
      (psetf (elt evals (1- id)) (elt evals id)
             (elt evals id) (elt evals (1- id))
             (current-evaluation repl) (1- id)))
    (reload-buffer (buffer repl))))

(define-command move-cell-down (&key (repl (find-submode 'repl-mode)) (id (current-evaluation repl)))
  "Move the current code cell down, swapping it with the one below."
  (when (and id (< id (1- (length (evaluations repl)))))
    (let ((evals (evaluations repl)))
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
                            ;; TODO: Make it re-compute on input.
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
                      (:raw (value->html result t))))
           (:span "No values.")))
      (t (:span (:button.button
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  (:title "abort-evaluation")
                                  (bt:destroy-thread (thread evaluation))
                                  (setf (ready-p evaluation) t)
                                  (reload-current-buffer)))
                 "Abort!")
                "Calculating...")))))

(define-internal-page-command-global repl (&key (form nil))
    (repl-buffer "*REPL*" 'repl-mode)
  "Create a Nyxt REPL buffer."
  (let ((repl-mode (find-submode 'repl-mode repl-buffer)))
    (if form
        (progn
          (setf (evaluations repl-mode)
                (cons (make-instance 'evaluation :input form
                                                 :mode-instance repl-mode
                                                 :ready-p t)
                      (evaluations repl-mode)))
          (buffer-load (nyxt-url 'repl) :buffer repl-buffer)
          "")
        (spinneret:with-html-string
          (:nstyle (style repl-mode))
          (:div :id "container"
                (:div :id "evaluations"
                      (loop
                        for evaluation in (evaluations repl-mode)
                        for order from 0
                        collect (let ((order order))
                                  (:div
                                   :class "evaluation"
                                   :id (format nil "evaluation-~a" (id evaluation))
                                   (:div :class "input-area"
                                         (:ninput
                                           :class "input-buffer"
                                           :data-repl-id order
                                           :autofocus (and (current-evaluation repl-mode)
                                                           (= order (current-evaluation repl-mode)))
                                           :onfocus (focus-cell :id order)
                                           :onchange (setf (input (elt (evaluations repl-mode) order))
                                                           (input repl-mode))
                                           (input evaluation))
                                         (:br)
                                         (:nbutton
                                           :class "accent"
                                           :text "Eval"
                                           :title "Run the current cell code and show the result below."
                                           (evaluate-cell :id order))
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
                                         (:nbutton
                                           :text "Reformat"
                                           :title "Re-indent the cell contents in accordance with compiler aesthetics."
                                           (reformat-cell :id order))
                                         (:nbutton
                                           :text "Save to auto-config"
                                           :title "Save this code to auto-config.lisp to be loaded in the next session."
                                           (add-cell-to-auto-config :id order))
                                         (:nbutton
                                           :text "Add cell below"
                                           :title "Add a new empty cell below this one."
                                           (add-cell :id (1+ order)))
                                         (:nbutton
                                           :text "Clean"
                                           :title "Clean the cell contents."
                                           (clean-cell :id order))
                                         (:nbutton
                                           :text "↑ Up"
                                           :title "Move this cell up."
                                           (move-cell-up :id order))
                                         (:nbutton
                                           :text "↓ Down"
                                           :title "Move this cell down."
                                           (move-cell-down :id order))
                                         (:nbutton
                                           :text "✕ Delete"
                                           :title "Remove this cell from the REPL."
                                           (delete-cell :id order)))
                                   (:div :class "evaluation-result"
                                         :id (format nil "evaluation-result-~a" (id evaluation))
                                         (:raw (html-result evaluation))))))))
          (:div.controls
           (:nbutton
             :text "+ Add a cell"
             :title "Add a new cell for you to evaluate code in."
             (add-cell))
           (:nbutton
             :text "Edit Nyxt function"
             :title "Edit the source of one of Nyxt commands in REPL."
             (let ((functions (prompt :prompt "Function to edit"
                                      :sources (make-instance
                                                'nyxt::function-source
                                                :actions-on-return #'identity))))
               (setf (evaluations repl-mode)
                     (append
                      (evaluations repl-mode)
                      (mapcar (lambda (sym)
                                (make-instance 'evaluation
                                               :input (function-lambda-string
                                                       (symbol-function sym))))
                              functions)))
               (reload-buffer (buffer repl-mode))))
           (:nbutton
             :text "✕ Delete all"
             :title "Delete all cells in the REPL buffer."
             (setf (evaluations repl-mode) nil)
             (reload-buffer (buffer repl-mode))))))))
