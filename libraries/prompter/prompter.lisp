;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

;; Same as `prompter-source' as to why we wrap in `eval-always'.
(sera:eval-always
  (define-class prompter ()
    ((input ""
            :accessor nil
            :reader input
            :documentation
            "User input.")

     (prompt ""
             :documentation
             "Prefix to the user input.")

     (sources '()
              :type (or null (cons prompter-source))
              :documentation "List of `prompter-source's.")

     (selection '() ; TODO: Add method to update `selection'.
                ;; TODO: Index by (source-index suggestion-index) instead?
                ;; TODO: Use structure?
                :type list
                :documentation "A pair of source and suggestion index.")

     (initializer nil
                  :type (or null function)
                  :documentation
                  "Function called with the prompter as argument.")

     (before-destructor nil
                     :type (or null function)
                     :documentation
                     "First function called with no parameters when calling the
`destructor' function over this prompter.
It's called before the sources are cleaned up.")
     (after-destructor nil
                    :type (or null function)
                    :documentation
                    "Last function called with no parameters when calling the
`destructor' function over this prompter.
It's called after the sources are cleaned up.

Note that the function is executed *before* performing an action.")

     ;; (history (make-history)              ; TODO: Move to `prompter' class?
     ;;     :type (or containers:ring-buffer-reverse null)
     ;;     :documentation
     ;;     "")

     (keymap nil
             :type (or null keymap:keymap)
             :documentation
             "Keymap for the prompter.
Useful, say, to switch source.
It takes precedence over individual source keymaps.")

     (help-message ""
                   :type (or string function)
                   :documentation
                   "Help message for this prompter.
It can be a function of one argument, the prompter, which returns a string.")
     (result-channel (make-channel 1)
                     :type calispel:channel
                     :documentation
                     "Channel to which the selection is sent on exit.
Also listen to `interrupt-channel' to know if the minibuffer is quitted.")
     (interrupt-channel (make-channel 1)
                        :type calispel:channel
                        :documentation
                        "Channel to which an arbitrary value is written on exit.
See also `result-channel'."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer #'class*:name-identity)
    (:documentation "The prompter is an interface for user interactions.
A prompter object holds multiple sources (of type `prompter-source') which
contain a list of `suggestion's.

You can call `destructor' to call the registered termination functions of the
prompter and its sources.

Suggestions are computed asynchronously when `input' is updated.
Use `ready-p' to know when the prompter is ready.
Sources suggestions can be retrieved even when the compution is not
finished.")))

(defmethod initialize-instance :after ((prompter prompter) &key)
  (setf (selection prompter) (list (first (sources prompter)) 0))
  (maybe-funcall (initializer prompter) prompter)
  prompter)

(export-always 'input)
(defmethod (setf input) (text (prompter prompter)) ; TODO: (str:replace-all " " " " input) in the caller.
  "Update PROMPTER sources and return TEXT."
  (let ((old-input (slot-value prompter 'input)))
    (unless (string= old-input text)
      (setf (slot-value prompter 'input) text)
      (mapc (lambda (source) (update source text)) (sources prompter))
      ;; TODO: Update `selection' when `update' is done.
      (setf (selection prompter) (list (first (sources prompter)) 0))))
  text)

(export-always 'destructor)
(defmethod destructor ((prompter prompter))
  "First call `before-destructor', then clean up all sources, finally call
`after-destructor'.
Signal destruction by sending a value to PROMPTER's `interrupt-channel'."
  (maybe-funcall (before-destructor prompter))
  (mapc #'destructor (sources prompter))
  (maybe-funcall (after-destructor prompter))
  ;; TODO: Interrupt before or after desctructor?
  (calispel:! (interrupt-channel prompter) t))

(defun select (prompter steps &key wrap-over-p)
  "Select suggestion by jumping STEPS forward.
If STEPS is 0, do nothing.
If STEPS is negative, go backward.
If the currently selected suggestion is the last one of the current source, go
to next source, or previous source if STEPS is negative."
  (unless (= 0 steps)
    (labels ((index->source (index &optional (sources (sources prompter)))
               (let ((limit (length (suggestions (first sources)))))
                 (if (< index limit)
                     (first sources)
                     (index->source (- index limit) (rest sources)))))
             (source-length (sources)
               (reduce #'+ (mapcar (lambda (source) (length (suggestions source)))
                                   sources)))
             (previous-sources (source)
               (let ((current-source-position (position source (sources prompter))))
                 (subseq (sources prompter) 0 (max 0 (1- current-source-position))))))
      (let* ((limit (source-length (sources prompter)))
             (previous-sources (previous-sources (first (selection prompter))))
             (index (+ (second (selection prompter))
                       (source-length previous-sources)))
             (new-index (+ index steps)))
        (setf new-index
              (if wrap-over-p
                  (mod new-index limit)
                  (alex:clamp new-index 0 (1- limit))))
        (let* ((new-source (index->source new-index))
               (relative-index (- new-index
                                  (source-length (previous-sources new-source)))))
          (setf (selection prompter)
                (list new-source relative-index)))))))

(export-always 'select-next)
(defun select-next (prompter &optional (steps 1))
  "Select element by jumping STEPS forward.
If STEPS is 0, do nothing.
If STEPS is negative, go backward."
  (select prompter steps))

(export-always 'select-previous)
(defun select-previous (prompter &optional (steps 1))
  "Select element by jumping STEPS forward.
If STEPS is 0, do nothing.
If STEPS is negative, go forward."
  (select prompter (- steps)))

(export-always 'return-selection)
(defun return-selection (prompter)
  "Send selection to PROMPTER's `result-channel'.
The selection is the collection of marked suggestions across all sources.
If there is no marked suggestion, send the currently selected suggestion
instead."
  (let ((result (or (mapcar #'value (alex:mappend #'marked-suggestions (sources prompter)))
                    (value (selected-suggestion prompter))
                    ;; TODO: What if there is no result?
                    (and (not (must-match-p prompter))
                         (slot-value prompter 'input)))))
    (calispel:! (result-channel prompter) result)))

(export-always 'return-input)
(defun return-input (prompter)
  "Send input to PROMPTER's `result-channel'."
  (calispel:! (result-channel prompter) (input prompter)))

(export-always 'ready-p)
(defun ready-p (prompter &optional timeout)
  "Return non-nil when all prompter sources are ready.
After timeout has elapsed for one source, return nil."
  (every (lambda (source)
            (nth-value 1 (calispel:? (ready-notifier source) timeout)))
          (sources prompter)))

(export-always 'make)
(define-function make
    (append '(&rest args)
            `(&key ,@(initargs 'prompter)))
  "Return prompter object."
  (apply #'make-instance 'prompter args))

(export-always 'selected-source)
(defun selected-source (prompter)
  (first (selection prompter)))

(export-always 'selected-suggestion)
(defun selected-suggestion (prompter)
  "Return selected prompt-buffer suggestion.
Return source as second value."
  (let* ((source (first (selection prompter))))
    (values (nth (second (selection prompter)) (suggestions source)) source)))

(export-always 'all-marked-suggestions)
(defun all-marked-suggestions (prompter)
  "Return the list of the marked suggestion values in the prompter."
  (alex:mappend #'marked-suggestions (sources prompter)))
