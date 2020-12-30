;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

(define-class sync-queue ()
  ((ready-sources '()
                  :type list
                  :export nil
                  :documentation
                  "List of ready sources.")
   (ready-channel (make-channel nil)
                  :type calispel:channel
                  :export nil
                  :documentation
                  "Communication channel with the `update' thread."))
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "This object is used to memorize which sources are ready for a
given input.
A new object is created on every new input."))

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

     (selection '()
                ;; TODO: Index by (source-index suggestion-index) instead?
                ;; TODO: Use structure?
                :type list
                :export nil
                :reader selection
                :documentation "A pair of source and suggestion index.")

     (initializer nil
                  :type (or null function)
                  :documentation
                  "Function called with the prompter as argument.")

     (before-destructor nil
                        :type (or null function)
                        :documentation
                        "First function called with no parameters when calling the
`destroy' function over this prompter.
It's called before the sources are cleaned up.")
     (after-destructor nil
                       :type (or null function)
                       :documentation
                       "Last function called with no parameters when calling the
`destroy' function over this prompter.
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

     (help-message ""                   ; TODO: Use.
                   :type (or string function)
                   :documentation
                   "Help message for this prompter.
It can be a function of one argument, the prompter, which returns a string.")
     (result-channel (make-channel 1)
                     :type calispel:channel
                     :documentation
                     "Channel to which the selection is sent on exit.
Caller should also listen to `interrupt-channel' to know if the minibuffer is quitted.")
     (interrupt-channel (make-channel 1)
                        :type calispel:channel
                        :documentation
                        "Channel to which an arbitrary value is written on exit.
See also `result-channel'.")

     (sync-queue nil
                 :type (or null sync-queue)
                 :export nil
                 :documentation "See `sync-queue' class documentation.")

     (returned-p nil
                 :type boolean
                 :documentation
                 "If non-nil, prompter has returned.
This is useful to know if prompter was cancelled or not."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer #'class*:name-identity)
    (:documentation "The prompter is an interface for user interactions.
A prompter object holds multiple sources (of type `prompter-source') which
contain a list of `suggestion's.

You can call `destroy' to call the registered termination functions of the
prompter and its sources.

Suggestions are computed asynchronously when `input' is updated.
Use `all-ready-p' and `next-ready-p' to know when the prompter is ready.
Sources suggestions can be retrieved, possibly partially, even when the
compution is not finished.")))

(defmethod initialize-instance :after ((prompter prompter) &key)
  (setf (selection prompter) (list (first (sources prompter)) 0))
  (maybe-funcall (initializer prompter) prompter)
  prompter)

(defmethod (setf selection) (value (prompter prompter))
  (setf (slot-value prompter 'selection) value)
  (let ((source (selected-source prompter)))
    (when (follow-p source)
      (if (< 0 (follow-delay source))
          (bt:make-thread
           (lambda ()
             (sleep (follow-delay source))
             (call-persistent-action prompter)))
          (call-persistent-action prompter)))))

(export-always 'input)
(defmethod (setf input) (text (prompter prompter))
  "Update PROMPTER sources and return TEXT."
  (let ((old-input (slot-value prompter 'input)))
    (unless (string= old-input text)
      (setf (slot-value prompter 'input) text)
      (setf (sync-queue prompter) (make-instance 'sync-queue))
      (mapc (lambda (source) (update source text (ready-channel (sync-queue prompter))))
            (sources prompter))
      ;; TODO: Update `selection' when `update' is done.
      (setf (selection prompter) (list (first (sources prompter)) 0))))
  text)

(export-always 'destroy)
(defun destroy (prompter)
  "First call `before-destructor', then call all the source destructors, finally call
`after-destructor'.
Signal destruction by sending a value to PROMPTER's `interrupt-channel'."
  (maybe-funcall (before-destructor prompter))
  (mapc (lambda (source) (maybe-funcall (destructor source) prompter source))
        (sources prompter))
  (maybe-funcall (after-destructor prompter))
  ;; TODO: Interrupt before or after desctructor?
  (calispel:! (interrupt-channel prompter) t))

(export-always 'call-persistent-action)
(defun call-persistent-action (prompter)
  (sera:and-let* ((action (persistent-action (selected-source prompter))))
    (funcall action
             (value (selected-suggestion prompter)))))

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
                 (subseq (sources prompter) 0 current-source-position))))
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

(export-always 'select-next-source)
(defun select-next-source (prompter &optional (steps 1))
  "Jumping STEPS source forward and select first suggestion.
If STEPS is 0, do nothing.
If STEPS is negative, go backward and select last suggestion."
  (unless (= 0 steps)
    (let* ((source-index (position (selected-source prompter)
                                   (sources prompter)))
           (new-source (nth (alex:clamp (+ steps source-index) 0 (1- (length (sources prompter))))
                            (sources prompter)))
           (suggestion-index (if (< 0 steps)
                                 0
                                 (1- (length (suggestions new-source))))))
      (setf (selection prompter)
            (list new-source suggestion-index)))))

(export-always 'select-previous-source)
(defun select-previous-source (prompter &optional (steps 1))
  "Jumping STEPS source backward and select last suggestion.
If STEPS is 0, do nothing.
If STEPS is negative, go forward and selection first suggestion."
  (unless (= 0 steps)
    (select-next-source prompter (- steps))))

(export-always 'select-first)
(defun select-first (prompter)
  "Select first element."
  (setf (selection prompter)
        (list (first (sources prompter)) 0)))

(export-always 'select-last)
(defun select-last (prompter)
  "Select last element."
  (let ((last-source (first (last (sources prompter)))))
    (setf (selection prompter)
          (list last-source
                (1- (length (suggestions last-source)))))))

(defun resolve-selection (prompter)
  (or (mapcar #'value (all-marked-suggestions prompter))
      (value (selected-suggestion prompter))
      ;; TODO: What if there is no result?
      (and (not (must-match-p prompter))
           (slot-value prompter 'input))))

(export-always 'actions)
(defun actions (prompter)
  "Return the list of contextual actions.
Without marks, it's the list of actions for the current source.
With marks, it's the intersection of the actions of the sources that contain the
marked elements."
  (let ((marked-sources
          (remove-if (complement #'marked-suggestions) (sources prompter))))
    (if marked-sources
        (reduce #'intersection (mapcar (lambda (source)
                                         (slot-value source 'actions))
                                       (sources prompter)))
        (slot-value (selected-source prompter) 'actions))))

(export-always 'return-selection)
(defun return-selection (prompter &optional (action (default-action prompter)))
  "Call action over selection and send the results to PROMPTER's `result-channel'.
The selection is the collection of marked suggestions across all sources.
If there is no marked suggestion, send the currently selected suggestion
instead."
  (unless action
    (setf action #'identity))
  ;; TODO: Catch conditions.
  (setf (returned-p prompter) t)
  (let ((action-result (funcall action (resolve-selection prompter))))
    (calispel:! (result-channel prompter) action-result)))

(export-always 'return-input)
(defun return-input (prompter)
  "Send input to PROMPTER's `result-channel'."
  (setf (returned-p prompter) t)
  (calispel:! (result-channel prompter) (input prompter)))

(export-always 'toggle-follow)
(defun toggle-follow (prompter &optional (source (selected-source prompter)))
  "Toggle `follow-p' in SOURCE."
  (setf (follow-p source) (not (follow-p source))))

(export-always 'next-ready-p)
(defun next-ready-p (prompter &optional timeout)
  "Block and return next PROMPTER ready source.
It's the next source that's done updating.
If all sources are done, return t.
If timeout expires for one source, return nil."
  ;; We copy `sync-queue' here so that it remains the same object throughout
  ;; this function, since the slot is subject to be changed concurrently when
  ;; the input is edited.
  (let ((sync-queue (sync-queue prompter)))
    (if sync-queue
        (if (= (length (ready-sources sync-queue))
               (length (sources prompter)))
            t
            (multiple-value-bind (next-source ok)
                (calispel:? (ready-channel sync-queue) timeout)
              (cond
                ((not ok)
                 nil)
                ((null next-source)
                 nil)
                (t
                 (push next-source (ready-sources sync-queue))
                 next-source))))
        ;; No sync-queue if no input was ever set.
        t)))

(export-always 'all-ready-p)
(defun all-ready-p (prompter &optional timeout)
  "Return non-nil when all prompter sources are ready.
After timeout has elapsed for one source, return nil."
  (sera:nlet check ((next-source (next-ready-p prompter timeout)))
    (cond
      ((eq t next-source)
       t)
      ((null next-source)
       nil)
      (t
       (check (next-ready-p prompter timeout))))))

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

(export-always 'selected-suggestion-position)
(defun selected-suggestion-position (prompter)
  "Return selected prompt-buffer suggestion position among current source
suggestions."
  (second (selection prompter)))

(export-always 'all-marked-suggestions)
(defun all-marked-suggestions (prompter)
  "Return the list of the marked suggestion values in the prompter."
  (alex:mappend #'marked-suggestions (sources prompter)))

(defun default-action (prompter)
  (first (slot-value (selected-source prompter) 'actions)))
