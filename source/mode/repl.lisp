;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/repl
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
(in-package :nyxt/mode/repl)

(define-class cell ()
  ((name
    "Cell"
    :type string
    :documentation "The name of the cell type.")
   (id
    (nyxt::new-id)
    :type alex:non-negative-integer
    :documentation "Unique evaluation identifier.")
   (mode-instance
    ;; If we use (find-submode 'repl-mode), compiler will scream at us that
    ;; 'repl-mode is not a mode symbol, because repl-mode is not yet defined.
    (current-mode :repl)
    :type (or null mode)
    :documentation "Repl-mode instance this cell belong to")
   (actions
    '(("Eval" evaluate-cell)
      ("Cancel" cancel-cell)
      ("Add cell below" add-cell)
      ("Clean" clean-cell)
      ("↑ Up" move-cell-up)
      ("↓ Down" move-cell-down)
      ("✕ Delete" delete-cell))
    :type list
    :documentation "A set of actions to manipulate the cell.
An undotted alist that maps an action name to a funcallable.  Each funcallable
has a single argument---the `cell'.")
   (thread
    nil
    :export nil
    :type (maybe bt:thread)
    :documentation "Thread where `evaluate'-ions run.")
   (input
    nil
    :type (maybe string)
    :documentation "User input.")
   (output
    nil
    :documentation "The text printed out during evaluation.")
   (ready-p
    nil
    :documentation "Whether `evaluate'-ion has terminated.")
   (results
    nil
    :documentation "The results (as a list) of cell `evaluate'-ion."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "The universal REPL cell, allowing to customize the REPL.
To make custom cell type, subclass `cell' and specify a set of methods:
- `evaluate',
- `render-results',
- `render-actons' (optional, defaults to drawing buttons),
- `render-input' (optional, defaults to textarea),
- or, in case of special need, `render-cell' to override all the default
  rendering.

With those in place, REPL should pick up on the new class and allows creating
new cells via UI."))

(defmethod mode-instance ((cell null))
  (current-mode :repl))

(defmethod mode-instance :around ((cell cell))
  (let ((result (call-next-method)))
    (or result (setf (slot-value cell 'mode-instance)
                     (current-mode :repl)))))

;;; Protocol for cells:
(export-always 'method-unimplemented)
(define-condition method-unimplemented (nyxt-error)
  ((unimplemented-method :initarg :unimplemented-method :accessor unimplemented-method)
   (cell :initarg :cell :accessor cell))
  (:report (lambda (c stream)
             (format stream "REPL method ~a not implemented for cell class ~a"
                     (unimplemented-method c) (sera:class-name-of (cell c)))))
  (:documentation "The condition that's thrown when the REPL mode method is not implemented."))

(export-always 'evaluate)
(defgeneric evaluate (cell)
  (:method ((cell cell))
    (cerror "Ignore the unimplemented method"
            'method-unimplemented :method 'evaluate :cell cell))
  (:documentation "Evaluate CELL and get its results.
Generic function to specialize for all the REPL cell types.
Use CELL (and, likely, its `input') and set its `results' and `output' to the
meaningful values."))

(export-always 'suggest)
(defgeneric suggest (cell)
  (:documentation "Get a single most intuitive suggestion string for CELL contents.
The suggestion listing may rely on the CELL's contents."))

(export-always 'render-input)
(defgeneric render-input (cell)
  (:method ((cell cell))
    (spinneret:with-html-string
      (:ninput
        :autofocus (eq (current-cell (current-mode :repl)) cell)
        :onfocus `(focus-cell ,cell)
        :onchange `(setf (input ,cell)
                         (ps-eval
                           (ps:@ (nyxt/ps:active-element document) value)))
        (input cell))))
  (:documentation "Generate HTML for the input area of the CELL.
Generic function to specialize against new REPL cell types.
The default implementation produces an `:ninput' field that updates `cell''s
`input' when modified."))

(export-always 'render-actions)
(defgeneric render-actions (cell)
  (:method ((cell cell))
    (spinneret:with-html-string
      (dolist (action (actions cell))
        (:nbutton :text (first action)
          `(funcall (quote ,(second action)) ,cell)))))
  (:documentation "Generate HTML for the `actions' of the CELL.
Generic function to specialize against new REPL cell types.
The default method simply draws buttons."))

(export-always 'render-results)
(defgeneric render-results (cell)
  (:method ((cell cell))
    (cerror "Ignore the unimplemented method"
            'method-unimplemented :method 'render-results :cell cell))
  (:documentation "Generate HTML for the `results' and `output' of the CELL.
Generic function to specialize against new REPL cell types."))

(export-always 'render-cell)
(defgeneric render-cell (cell)
  (:method ((cell cell))
    (spinneret:with-html-string
      (:div.cell
       :class (when (eq cell (current-cell (mode-instance cell)))
                "current")
       (:div.input-area
        (:div.cell-name
         (:code (name cell)))
        (:div.cell-input
         (:raw (render-input cell)))
        (:div.cell-actions
         (:raw (render-actions cell))))
       (:div.evaluation-result
        (:raw (render-results cell))))))
  (:documentation "Generate HTML for the CELL.
Overrides all the methods defined for the CELL type.
By default utilizes `render-input', `render-actions', and `render-results'.
Generic function to specialize against new REPL cell types."))

(defun reload-repl (repl)
  (with-current-buffer (buffer repl)
    (repl)))

(export-always 'cancel-cell)
(defmethod cancel-cell ((cell cell))
  "Destroy the evaluation thread and set `ready-p' to NIL."
  (destroy-thread* (thread cell))
  (setf (thread cell) nil
        (ready-p cell) nil)
  (reload-repl (mode-instance cell)))

;;; Lisp cell
(define-class lisp-cell (cell)
  ((name
    "Lisp expression"
    :writer t
    :reader nil)
   (eval-package
    *package*
    :type package
    :documentation "The package where Lisp cell reading and `evaluate'-ion happens.")
   (actions
    '(("Eval" evaluate-cell)
      ("Cancel" cancel-cell)
      ("Add cell below" add-cell)
      ("Clean" clean-cell)
      ("↑ Up" move-cell-up)
      ("↓ Down" move-cell-down)
      ("✕ Delete" delete-cell)
      ("Reformat" reformat-cell)
      ("Save to auto-config" add-cell-to-auto-config)
      ("Set package" set-cell-package)))
   (raised-condition
    nil
    :type (maybe ndebug:condition-wrapper)
    :documentation "The condition raised during the Lisp cell execution."))
  (:metaclass user-class)
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Cell intended for Lisp expressions evaluation.

The `input' should be a valid Lisp code `read'-able in the `eval-package'.
`evaluate'-ion simply
- `eval'-s the code,
- sets `results' to a `multiple-value-list' of evaluation,
- sets `output' to the `*standard-output*' contents,
- and sets `raised-condition' (if any) to a wrapper class managing raised
  condition debugging."))

(define-class cell-source (prompter:source)
  ((prompter:name "Cell types")
   (prompter:constructor (mopu:subclasses (find-class 'cell nil)))))

(defmethod prompter:object-attributes ((class standard-class) (source cell-source))
  `(("Name" ,(class-name class))
    ("Documentation" ,(documentation-line class 'type ""))))

(sera:defmethods lisp-cell
    (self
     (raised-condition #'raised-condition)
     (input #'input)
     (output #'output)
     (ready-p #'ready-p)
     (mode-instance #'mode-instance)
     (results #'results)
     (eval-package #'eval-package)
     (actions #'actions))
  (:method name (self)
    (setf (slot-value self 'name)
          (format nil "Lisp expression (in ~a)" (package-name eval-package))))
  (:method evaluate (self)
    (let* ((nyxt::*interactive-p* t)
           (*standard-output* (make-string-output-stream))
           (*error-output* *standard-output*)
           (*trace-output* *standard-output*)
           (*package* eval-package))
      (ndebug:with-debugger-hook
          (:wrapper-class 'nyxt::debug-wrapper
           :ui-display (setf ready-p t
                             raised-condition %wrapper%)
           :ui-cleanup (lambda (wrapper)
                         (declare (ignore wrapper))
                         (setf raised-condition nil)
                         (reload-repl mode-instance)))
        (with-input-from-string (in input)
          (alex:lastcar
           (mapcar (lambda (s-exp)
                     (setf results (multiple-value-list (eval s-exp))
                           output (get-output-stream-string *standard-output*)))
                   (safe-slurp-stream-forms in)))))))
  (:method suggest (self)
    (alex:when-let ((sugestion (prompt1 :prompt "Symbol to insert"
                                        :sources '(nyxt::function-source
                                                   nyxt::variable-source
                                                   nyxt::class-source
                                                   nyxt::function-non-nyxt-source
                                                   nyxt::variable-non-nyxt-source
                                                   nyxt::class-non-nyxt-source
                                                   nyxt::function-internal-source
                                                   nyxt::variable-internal-source
                                                   nyxt::class-internal-source))))
      (prini-to-string sugestion :package (eval-package self))))
  (:method render-results (self)
    (spinneret:with-html-string
      (unless (uiop:emptyp output)
        (:pre output))
      (cond
        ((and ready-p
              raised-condition)
         (let ((wrapper raised-condition))
           (:pre (format nil "~a: ~a"
                         (type-of (ndebug:condition-itself wrapper))
                         (ndebug:condition-itself wrapper)))
           (dolist (restart (ndebug:restarts wrapper))
             (:button.button
              :onclick (ps:ps (nyxt/ps:lisp-eval
                               (:title "condition")
                               (ndebug:invoke wrapper restart)))
              (format nil "[~a] ~a" (dissect:name restart) (dissect:report restart))))
           (:h3 "Backtrace:")
           (:raw (nyxt::backtrace->html wrapper))))
        (ready-p
         (if results
             (loop
               for result in results
               for sub-order from 0
               for name = (if (serapeum:single results)
                              (intern (format nil "V~d" (id self)))
                              (intern (format nil "V~d.~d" (id self) sub-order)))
               do (setf (symbol-value name) result)
               collect (:div
                        (format nil "~(~a~) = " name)
                        (:raw (value->html result t))))
             (:span "No values.")))
        (t "Evaluating...")))))

;;; Shell cell
(define-class shell-cell (cell)
  ((name
    "Shell command"
    :writer t
    :reader nil)
   (shell
    '("bash" "-c")
    :documentation "Shell command and arguments, as a list or a string.")
   (working-directory
    (user-homedir-pathname)
    :documentation "The directory shell command runs in.")
   (actions
    `(("Eval" evaluate-cell)
      ("Cancel" cancel-cell)
      ("Add cell below" add-cell)
      ("Clean" clean-cell)
      ("↑ Up" move-cell-up)
      ("↓ Down" move-cell-down)
      ("✕ Delete" delete-cell)
      ("Set directory" ,(lambda (cell)
                          (declare (ignorable cell))
                          (setf (working-directory cell)
                                (prompt1 :prompt "Directory"
                                         :input (uiop:native-namestring (uiop:getcwd))
                                         :sources (make-instance 'nyxt/mode/file-manager:file-source
                                                                 :path-filter #'uiop:directory-pathname-p)))
                          (reload-repl (mode-instance cell)))))))
  (:metaclass user-class)
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A cell type for shell commands.
`evaluate' runs a `shell' with `input' in the `working-directory'.
`results' are a list of the error/success code.
`output' is the shell output."))

(defmethod name ((cell shell-cell))
  (setf (slot-value cell 'name)
        (format nil "Shell command (in ~a)" (working-directory cell))))

(sera:defmethods shell-cell
    (self
     (input #'input)
     (output #'output)
     (ready-p #'ready-p)
     (results #'results)
     (shell #'shell))
  (:method evaluate (self)
    (uiop:with-current-directory ((working-directory self))
      (setf (name self)
            (format nil "Shell command (in ~a)" (working-directory self)))
      (handler-case
          (multiple-value-bind (out error-out exit-code)
              (uiop:run-program (append (uiop:ensure-list shell)
                                        (list input))
                                :output :string)
            (declare (ignorable error-out))
            (setf results (list exit-code)
                  output out))
        (uiop:subprocess-error ()
          (continue)))))
  (:method render-results (self)
    (spinneret:with-html-string
      (cond
        (ready-p
         (if results
             (loop
               for result in results
               for sub-order from 0
               for name = (if (serapeum:single results)
                              (intern (format nil "V~d" (id self)))
                              (intern (format nil "V~d.~d" (id self) sub-order)))
               do (setf (symbol-value name) result)
               collect (:div
                        (format nil "~(~a~) = " name)
                        (:raw (value->html result t))))
             (:span "No values.")))
        (t "Running..."))
      (unless (uiop:emptyp output)
        (:pre output)))))

;;; REPL mode itself
(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((keyscheme-map
    (define-keyscheme-map "repl-mode" ()
      keyscheme:default
      (list
       "tab" 'suggest-into-cell
       "C-return" 'evaluate-cell)
      keyscheme:emacs
      (list
       "C-M-x" 'evaluate-cell
       "C-b" 'nyxt/mode/input-edit:cursor-backwards
       "C-f" 'nyxt/mode/input-edit:cursor-forwards
       "C-d" 'nyxt/mode/input-edit:delete-forwards
       "M-b" 'nyxt/mode/input-edit:cursor-backwards-word
       "M-f" 'nyxt/mode/input-edit:cursor-forwards-word
       "M-backspace" 'nyxt/mode/input-edit:delete-backwards-word
       "C-backspace" 'nyxt/mode/input-edit:delete-backwards-word
       "M-d" 'nyxt/mode/input-edit:delete-forwards-word
       "M-p" 'previous-cell
       "M-n" 'next-cell
       ;; FIXME: Org uses C-c C-_ and C-c C-^, but those are shadowed by C-c in Nyxt.
       "C-^" 'move-cell-up
       "C-_" 'move-cell-down
       "C-k" 'clean-cell
       "C-M-k" 'delete-cell
       "M-q" 'reformat-cell
       "C-i" 'suggest-into-cell)
      keyscheme:vi-normal
      (list
       ;; TODO: deleting chars/words
       "h" 'nyxt/mode/input-edit:cursor-backwards
       "l" 'nyxt/mode/input-edit:cursor-forwards
       "x" 'nyxt/mode/input-edit:delete-forwards
       "b" 'nyxt/mode/input-edit:cursor-backwards-word
       "w" 'nyxt/mode/input-edit:cursor-forwards-word
       "d b" 'nyxt/mode/input-edit:delete-backwards-word
       "d w" 'nyxt/mode/input-edit:delete-forwards-word
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
              :padding "0.5rem"
              :margin "1rem")
            `(.evaluation-result
              :margin "1rem")
            `(.button
              :display "inline")
            `("textarea,input"
              :color ,theme:on-accent
              :opacity "0.9"
              :border "none"
              :outline "none"
              :padding 0
              :margin 0
              :autofocus "true"
              :width "99%")
            `("#cells"
              :font-size "12px"
              :flex-grow "1"
              :overflow-y "auto"
              :overflow-x "auto")
            `(.controls
              :font-size "14px"
              :position "absolute"
              :bottom "1em"
              :right "1em")
            `(.cell-actions
              :display none)
            `(":focus-within ~ .cell-actions"
              :display block)
            `(".current .cell-actions"
              :display block))
          :documentation "The CSS applied to a REPL when it is set-up.")
   (cells
    (list (make-instance 'lisp-cell :input "\"Hello, Nyxt!\""))
    :documentation "A list of `cell's representing the current state of the REPL.")
   (current-cell
    nil
    :documentation "The `cell' that currently has the focus."))
  (:toggler-command-p nil))

(defmethod (setf cells) :after ((new-value null) (mode repl-mode))
  "A watcher to set `current-cell' to NIL when `cells' are empty."
  (setf (current-cell mode) nil)
  (reload-repl mode))

(defmethod focus-cell ((cell cell))
  (setf (current-cell (mode-instance cell)) cell))

(define-command evaluate-cell (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Evaluate the CELL."
  (let ((repl (mode-instance cell)))
    (focus-cell cell)
    (unless (uiop:emptyp (input cell))
      (setf (current-cell repl) cell
            (thread cell) (run-thread "repl cell evaluation"
                            (evaluate cell)
                            (setf (ready-p cell) t)
                            (reload-repl repl)))
      (reload-repl repl))))

(define-command suggest-into-cell (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Paste the chosen symbol into the CELL.
Relies on `suggest' of the CELL class."
  (ffi-buffer-paste (buffer (mode-instance cell)) (suggest cell)))

(define-command add-cell (&optional (cell (current-cell (find-submode 'repl-mode)))
                          (class (let ((nyxt::*interactive-p* t))
                                   (prompt1 :prompt "Cell type"
                                            :sources 'cell-source))))
  "Add a new CLASS cell below CELL.
If CELL is not provided, add the new cell below all the cells in REPL."
  (let* ((repl (mode-instance cell))
         (new-cell (make-instance class :input ""))
         (cell-position (if cell
                            (1+ (position cell (cells repl)))
                            (length (cells repl)))))
    (setf (cells repl)
          (append (subseq (cells repl) 0 cell-position)
                  (list new-cell)
                  (subseq (cells repl) cell-position)))
    (focus-cell new-cell)
    (reload-repl repl)))

(define-command clean-cell (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Clean the CELL, removing all input and accumulated state.
If CELL is not provided, clean the current cell."
  (let ((repl (mode-instance cell)))
    (focus-cell cell)
    (setf (cells repl)
          (substitute (make-instance (class-of cell) :input "") cell (cells repl)))
    (reload-repl repl)))

(define-command delete-cell (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Remove the CELL.
If CELL is not provided, remove the current cell."
  (let* ((repl (mode-instance cell))
         (position (position cell (cells repl))))
    (setf (cells repl) (remove cell (cells repl)))
    (focus-cell (elt (cells repl) (max 0 (1- position))))
    (reload-repl repl)))

(defun format-form (form package)
  (prini-to-string form :readably t :package package))

(define-command reformat-cell (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Reformat the CELL's input.

Follows what the compiler finds aesthetically pleasing."
  (focus-cell cell)
  (let ((repl (mode-instance cell)))
    (handler-case
        (progn
          (setf (input cell)
                (format-form (read-from-string (input cell))
                             (eval-package cell)))
          (reload-repl repl))
      (error (e)
        (echo "The input appears malformed. Stop reformatting. Original message: ~a" e)))))

(define-command add-cell-to-auto-config (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Add cell contents to auto-config for further loading."
  (let ((auto-config (files:expand nyxt::*auto-config-file*)))
    (ensure-file-exists auto-config)
    (alex:write-string-into-file +newline+ auto-config :if-exists :append)
    (alex:write-string-into-file
     (format-form (uiop:safe-read-from-string (input cell) :package (eval-package cell))
                  :nyxt-user)
     auto-config :if-exists :append)
    (echo "Saved form into ~a" auto-config)))

(define-command set-cell-package (&optional (cell (current-cell (find-submode 'repl-mode)))
                             (package (find-package
                                       (prompt1 :prompt "Evaluation package"
                                                :sources 'nyxt::package-source))))
  (when cell
    (setf (eval-package cell) package
          (name cell) (format nil "Lisp expression (~a)" (package-name package)))
    (reload-repl (mode-instance cell))))

(define-command previous-cell (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Navigate to the previous input cell."
  (let ((repl (mode-instance cell)))
    (when (cells repl)
      (focus-cell (elt (cells repl) (max 0 (1- (position cell (cells repl))))))
      (reload-repl repl))))

(define-command next-cell (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Navigate to the next input cell."
  (let ((repl (mode-instance cell)))
    (when (cells repl)
      (focus-cell (elt (cells repl)
                       (min (1- (length (cells repl)))
                            (1+ (position cell (cells repl))))))
      (reload-repl repl))))

(define-command move-cell-up (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Move the current code cell up, swapping it with the one above."
  (when cell
    (let* ((cells (cells (mode-instance cell)))
           (cell-position (position cell cells)))
      (focus-cell cell)
      (unless (eq cell (first cells))
        (psetf (elt cells (1- cell-position)) cell
               (elt cells cell-position) (elt cells (1- cell-position)))
        (reload-repl (mode-instance cell))))))

(define-command move-cell-down (&optional (cell (current-cell (find-submode 'repl-mode))))
  "Move the current code cell down, swapping it with the one below."
  (when cell
    (let* ((cells (cells (mode-instance cell)))
           (cell-position (position cell cells)))
      (focus-cell cell)
      (unless (eq cell (alex:lastcar cells))
        (psetf (elt cells (1+ cell-position)) cell
               (elt cells cell-position) (elt cells (1+ cell-position)))
        (reload-repl (mode-instance cell))))))

(define-internal-page-command-global repl (&key (class nil) (form nil))
    (repl-buffer "*REPL*" 'repl-mode)
  "Create a Nyxt REPL buffer."
  (let ((repl-mode (find-submode 'repl-mode repl-buffer)))
    (when form
      (setf (cells repl-mode)
            (cons (make-instance (or class 'lisp-cell)
                                 :input form
                                 :mode-instance repl-mode
                                 :ready-p t)
                  (cells repl-mode))))
    (with-current-buffer repl-buffer
      (spinneret:with-html-string
        (:nstyle (style repl-mode))
        (:div :id "container"
              (:div :id "cells"
                    (dolist (cell (cells repl-mode))
                      (:raw (render-cell cell)))))
        (:div.controls
         (:nbutton
           :text "+ Add a cell"
           :title "Add a new cell for you to evaluate code in."
           '(add-cell))
         (:nbutton
           :text "Edit Nyxt function"
           :title "Edit the source of one of Nyxt commands in REPL."
           ;; NOTE: Using a closure injection here because repl-mode is used in
           ;; too much places. Maybe rewrite to a let-binding later.
           `(funcall ,#'(lambda ()
                          (let ((functions (prompt :prompt "Function to edit"
                                                   :sources (make-instance
                                                             'nyxt::function-source
                                                             :actions-on-return #'identity))))
                            (setf (cells repl-mode)
                                  (append
                                   (cells repl-mode)
                                   (mapcar (lambda (sym)
                                             (make-instance 'lisp-cell
                                                            :input (function-lambda-string
                                                                    (symbol-function sym))))
                                           functions)))
                            (reload-repl repl-mode)))))
         (:nbutton
           :text "✕ Delete all"
           :title "Delete all cells in the REPL buffer."
           `(setf (cells ,repl-mode) nil)
           `(reload-repl ,repl-mode)))))))
