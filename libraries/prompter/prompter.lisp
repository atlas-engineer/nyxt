;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

(define-class sync-queue ()
  ((ready-sources
    '()
    :type list
    :export nil
    :documentation "List of ready sources.")
   (ready-channel
    (make-channel nil)
    :type calispel:channel
    :export nil
    :documentation "Communication channel with the `update' thread.")
   (sync-interrupt-channel
    (make-channel)
    :type calispel:channel
    :export nil
    :documentation "This channel can be used to stop the queue listening."))
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "This object is used to memorize which sources are ready for a
given input.
A new object is created on every new input."))

(defvar *default-history-size* 1000)    ; TODO: Export?

(declaim (ftype (function (&key (:size fixnum)) containers:ring-buffer-reverse) make-history))
(defun make-history (&key (size *default-history-size*))
  "Return a new ring buffer."
  (the (values cl-containers:ring-buffer-reverse &optional)
       (containers:make-ring-buffer size :last-in-first-out)))

;; Eval at read-time because `make' is generated using the class' initargs.
(sera:eval-always
  (define-class prompter ()
    ((input
      ""
      :accessor nil
      :reader input
      :documentation "User input.")

     (prompt
      ""
      :documentation "Prefix to the user input.")

     (sources
      '()
      :initarg nil
      :type (or null source (cons source))
      :documentation "List of `source's.
For convenience, if the initarg is a single source (that is, not inside a list),
it is automatically wrapped into a list upon initialization.

If the source is designated by a symbol, then it is automatically instantiated
with `make-instance' and the result is used in this slot.

See also `make'.")

     (current-suggestion
      '()
      ;; TODO: Index by (source-index suggestion-index) instead?
      ;; TODO: Use structure?
      :type list
      :export nil
      :reader current-suggestion
      :documentation "A pair of source and suggestion index.")

     (constructor
      nil
      :type (or null function)
      :documentation "Function called with the prompter as argument.")

     (before-destructor
      nil
      :type (or null function)
      :documentation "First function called with no parameters when calling the
`destroy' function over this prompter.
It's called before the sources are cleaned up.")

     (after-destructor
      nil
      :type (or null function)
      :documentation "Last function called with no parameters when calling the
`destroy' function over this prompter.
It's called after the sources are cleaned up.")

     (auto-return-p
      nil
      :type boolean
      :documentation "Whether the action returned by `default-action-on-return'
automatically runs when the suggestions are narrowed down to just one item.")

     (history
      (make-history)
      :type (or containers:ring-buffer-reverse null)
      :documentation "History of inputs for the prompter.
If nil, no history is used.")

     (result-channel
      (make-channel 1)
      :type calispel:channel
      :documentation "Channel to which the `current-suggestion' is sent on exit.
Caller should also listen to `interrupt-channel' to know if the prompter was cancelled.")

     (interrupt-channel
      (make-channel 1)
      :type calispel:channel
      :documentation "Channel to which an arbitrary value is written on exit.
See also `result-channel'.")

     (sync-queue
      nil
      :type (or null sync-queue)
      :export nil
      :documentation "See `sync-queue' class documentation.")

     (returned-p
      nil
      :type boolean
      :documentation "Whether the prompter has been cancelled."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer (class*:make-name-transformer name))
    (:documentation "The prompter is an interface for user interactions.
A prompter object holds multiple `source's which contain a list of
`suggestion's.

Call `destroy' to the register termination functions of the prompter and its
sources.

`suggestion's are computed asynchronously when `input' is updated.
Use `all-ready-p' and `next-ready-p' to access whether the prompter is ready.
Sources' suggestions can be retrieved, possibly partially, even when the
computation is not finished.")))

(defun update-sources (prompter &optional (text ""))
  (setf (sync-queue prompter) (make-instance 'sync-queue))
  (mapc (lambda (source) (update source text (ready-channel (sync-queue prompter))))
        (sources prompter)))

(defmethod initialize-instance :after ((prompter prompter) &key sources
                                       &allow-other-keys)
  (unless (stringp (prompt prompter))
    (setf (prompt prompter) (write-to-string (prompt prompter))))
  (unless (stringp (input prompter))
    (setf (input prompter) (write-to-string (input prompter))))
  (flet ((ensure-sources (specifiers)
           (mapcar (lambda (source-specifier)
                     (cond
                       ((source-p source-specifier)
                        source-specifier)
                       ((and (symbolp source-specifier)
                             (c2cl:subclassp source-specifier 'source))
                        (make-instance source-specifier))
                       (t (error "Bad source specifier ~s." source-specifier))))
                   (uiop:ensure-list specifiers))))
    (alex:appendf (sources prompter) (ensure-sources sources)))
  (first-suggestion prompter)
  (maybe-funcall (constructor prompter) prompter)
  (update-sources prompter (input prompter))
  prompter)

(defmethod (setf current-suggestion) (value (prompter prompter))
  (setf (slot-value prompter 'current-suggestion) value)
  (run-action-on-current-suggestion prompter))

(export-always 'run-action-on-current-suggestion)
(defmethod run-action-on-current-suggestion ((prompter prompter))
  (sera:and-let* ((source (current-source prompter))
                  (_ (actions-on-current-suggestion-enabled-p source))
                  (action (default-action-on-current-suggestion source))
                  (suggestion (%current-suggestion prompter)))
    (let ((delay (actions-on-current-suggestion-delay source)))
      (if (plusp delay)
          (run-thread "Prompter current suggestion action thread"
            (sleep delay)
            (funcall action (value suggestion)))
          (funcall action (value suggestion))))))

(export-always 'set-action-on-current-suggestion)
(defmethod set-action-on-current-suggestion (value (prompter prompter))
  (setf (actions-on-current-suggestion (current-source prompter))
        (cons value
              (delete value (actions-on-current-suggestion (current-source prompter))))))

(export-always 'input)
(defmethod (setf input) (text (prompter prompter))
  "Update PROMPTER sources and return TEXT."
  (let ((old-input (slot-value prompter 'input)))
    (unless (string= old-input text)
      (setf (slot-value prompter 'input) text)
      (update-sources prompter text)
      (first-suggestion prompter)))
  text)

(export-always 'destroy)
(defmethod destroy ((prompter prompter))
  "First call `before-destructor', then call all the source destructors, finally call
`after-destructor'.
Signal destruction by sending a value to PROMPTER's `interrupt-channel'."
  (maybe-funcall (before-destructor prompter))
  (mapc (lambda (source) (maybe-funcall (destructor source) prompter source))
        (sources prompter))
  (mapc #'destroy (sources prompter))
  (maybe-funcall (after-destructor prompter))
  ;; TODO: Interrupt before or after destructor?
  (calispel:! (sync-interrupt-channel (sync-queue prompter)) t)
  (calispel:! (interrupt-channel prompter) t))

(defun set-current-suggestion (prompter steps &key wrap-over-p)
  "Set PROMPTER's `current-suggestion' by jumping STEPS forward.
If STEPS is 0, do nothing.
If STEPS is negative, go backward.
When the current suggestion is the last (resp. first) one of the current source,
return the first (resp. last) suggestion of the next (resp. previous) source
when STEPS is positive (resp. negative)."
  (unless (= 0 steps)
    (labels ((index->source (index &optional (sources (sources prompter)))
               (let ((limit (length (suggestions (first sources)))))
                 (if (or (< index limit)
                         ;; Can happen when INDEX is beyond the total length.
                         (null (rest sources)))
                     (first sources)
                     (index->source (- index limit) (rest sources)))))
             (source-length (sources)
               (reduce #'+ sources :key (lambda (s) (length (suggestions s)))))
             (previous-sources (source)
               (let ((current-source-position (position source (sources prompter))))
                 (subseq (sources prompter) 0 current-source-position))))
      (let ((limit (source-length (sources prompter))))
        (declare (type unsigned-byte limit))
        (unless (= 0 limit)
          (let* ((previous-sources (previous-sources (first (current-suggestion prompter))))
                 (index (+ (second (current-suggestion prompter))
                           (source-length previous-sources)))
                 (new-index (+ index steps)))
            (setf new-index
                  (if wrap-over-p
                      (mod new-index limit)
                      (alex:clamp new-index 0 (max (1- limit) 0))))
            (let* ((new-source (index->source new-index))
                   (relative-index (- new-index
                                      (source-length (previous-sources new-source)))))
              (declare (type unsigned-byte relative-index))
              (setf (current-suggestion prompter)
                    (list new-source relative-index)))))))))

(export-always 'next-suggestion)
(defun next-suggestion (prompter &optional (steps 1))
  "See `set-current-suggestion'."
  (set-current-suggestion prompter steps))

(export-always 'previous-suggestion)
(defun previous-suggestion (prompter &optional (steps 1))
  "See `set-current-suggestion'."
  (set-current-suggestion prompter (- steps)))

(defun empty-source-p (source)
  (not (suggestions source)))

(export-always 'next-source)
(defun next-source (prompter &optional (steps 1))
  "Set `current-suggestion' after jumping STEPS non-empty sources.
If STEPS is 0, do nothing.
When STEPS is positive (resp. negative), set the current suggestion to be the
first (resp. last) one of destination source."
  (unless (= 0 steps)
    (sera:and-let* ((nonempty-sources (remove-if #'empty-source-p (sources prompter)))
                    (source-index (or (position (current-source prompter)
                                                nonempty-sources)
                                      0))
                    (new-source (nth (alex:clamp (+ steps source-index) 0 (1- (length nonempty-sources)))
                                     nonempty-sources))
                    (suggestion-index (if (< 0 steps)
                                          0
                                          (1- (length (suggestions new-source))))))
      (setf (current-suggestion prompter)
            (list new-source suggestion-index)))))

(export-always 'previous-source)
(defun previous-source (prompter &optional (steps 1))
  (next-source prompter (- steps)))

(defun nonempty-source-p (source)
  (suggestions source))

(export-always 'first-suggestion)
(defun first-suggestion (prompter)
  "Set `current-suggestion' to PROMPTER's first suggestion.
Empty sources are skipped."
  (let ((first-non-empty-source
          (or (find-if #'nonempty-source-p (sources prompter))
              (first (sources prompter)))))
    (setf (current-suggestion prompter)
          (list first-non-empty-source 0))
    (when (and (auto-return-p prompter)
               (sera:single (all-suggestions prompter)))
      (run-action-on-return prompter))))

(export-always 'last-suggestion)
(defun last-suggestion (prompter)
  "Set `current-suggestion' to PROMPTER's last suggestion."
  (let ((last-non-empty-source
          (or (find #'nonempty-source-p (sources prompter)
                    :from-end t)
              (first (last (sources prompter))))))
    (setf (current-suggestion prompter)
          (list last-non-empty-source
                (1- (length (suggestions last-non-empty-source)))))))

(export-always 'toggle-mark)
(defun toggle-mark (prompter)
  (when (enable-marks-p (current-source prompter))
    (multiple-value-bind (suggestion source)
        (%current-suggestion prompter)
      (with-accessors ((marks marks)) source
        (let ((value (value suggestion)))
          (if (find value marks)
              (setf marks (delete value marks))
              (push value marks)))))))

(export-always 'mark-all)
(defun mark-all (prompter)
  (let ((source (current-source prompter)))
    (when (enable-marks-p source)
      (alex:unionf (marks source)
                   (mapcar #'value (suggestions source))))))

(export-always 'unmark-all)
(defun unmark-all (prompter)
  (let ((source (current-source prompter)))
    (when (enable-marks-p source)
      (with-accessors ((marks marks)
                       (suggestions suggestions))
          source
        (setf marks
              (set-difference marks
                              (mapcar #'value suggestions)))))))

(export-always 'toggle-mark-all)
(defun toggle-mark-all (prompter)
  (let ((source (current-source prompter)))
    (when (enable-marks-p source)
      (with-accessors ((suggestions suggestions)
                       (marks marks))
          source
        (let ((suggestion-values (mapcar #'value suggestions)))
          (setf marks
                (cond
                  ((subsetp marks suggestion-values)
                   (set-difference suggestion-values marks))
                  ((subsetp suggestion-values marks)
                   (set-difference marks suggestion-values))
                  (t ; When the intersection of suggestion-values and marks is non-trivial.
                   (set-difference
                    (union marks suggestion-values)
                    (intersection marks suggestion-values))))))))))

(defun resolve-marks (prompter)     ; TODO: Write tests for this!
  "Return the list of marked `suggestion's.
When `marks' is nil, the current suggestion value is returned as a list of one
element.
For instance, if the current suggestion value is NIL, this returns '(NIL).  If
there is no element, NIL is returned."
  (or (all-marks prompter)
      (mapcar #'value (uiop:ensure-list (%current-suggestion prompter)))))

(export-always 'actions-on-return)
(defun actions-on-return (prompter)
  "Return the list of contextual `actions-on-return'.

When `marks' is non-nil, return the list of `actions-on-return' shared by every
marked element; otherwise return the list of `actions-on-return' for the current
`source'."
  (alex:if-let ((marked-sources (remove-if (complement #'marks) (sources prompter))))
    (reduce #'intersection
            (mapcar (lambda (source) (slot-value source 'actions-on-return))
                    marked-sources))
    (slot-value (current-source prompter) 'actions-on-return)))

(defun history-pushnew (history element &key (test #'equal) )
  (alex:when-let ((previous-element-index (containers:element-position history
                                                                       element
                                                                       :test test)))
    (containers:delete-item-at history previous-element-index))
  (containers:insert-item history element))


(defun add-input-to-history (prompter)
  "Add PROMPTER's current input to its history, if any.
If input is already in history, move to first position."
  (unless (or (null (history prompter))
              (str:empty? (input prompter)))
    (history-pushnew (history prompter)
                     (input prompter))))

(export-always 'run-action-on-return)
(defun run-action-on-return (prompter &optional (action-on-return
                                                 (default-action-on-return prompter)))
  "Call ACTION-ON-RETURN over `marks' and send the results to PROMPTER's
`result-channel'.
See `resolve-marks' for a reference on how `marks' are handled."
  (unless action-on-return (setf action-on-return #'identity))
  (setf (returned-p prompter) t)
  (add-input-to-history prompter)
  (alex:when-let ((marks (resolve-marks prompter)))
    (calispel:! (result-channel prompter)
                (funcall action-on-return marks)))
  (destroy prompter))

(export-always 'toggle-actions-on-current-suggestion-enabled)
(defun toggle-actions-on-current-suggestion-enabled
    (prompter &optional (source (current-source prompter)))
  "Toggle `actions-on-current-suggestion-enabled-p' in SOURCE."
  (setf (actions-on-current-suggestion-enabled-p source)
        (not (actions-on-current-suggestion-enabled-p source))))

(export-always 'next-ready-p)
(defun next-ready-p (prompter)
  "Block and return next PROMPTER ready source.
It's the next source that's done updating.
If all sources are done, return T.
This is unblocked when the PROMPTER is `destroy'ed."
  (when prompter
    ;; We let-bind `sync-queue' here so that it remains the same object throughout
    ;; this function, since the slot is subject to be changed concurrently when
    ;; the input is edited.
    (alex:if-let ((sync-queue (sync-queue prompter)))
      (if (= (length (ready-sources sync-queue))
             (length (sources prompter)))
          t
          (calispel:fair-alt
            ((calispel:? (ready-channel sync-queue) next-source)
             (cond
               ((null next-source)
                nil)
               (t
                (push next-source (ready-sources sync-queue))
                ;; Update current suggestion when update is done:
                (first-suggestion prompter)
                next-source)))
            ((calispel:? (sync-interrupt-channel sync-queue))
             nil)))
      ;; No sync-queue if no input was ever set.
      t)))

(export-always 'all-ready-p)
(defun all-ready-p (prompter)
  "Return non-nil when all PROMPTER sources are ready."
  (sera:nlet check ((next-source (next-ready-p prompter)))
    (if (typep next-source 'boolean)
        next-source
        (check (next-ready-p prompter)))))

(export-always 'make)
(define-function make
    (append '(&rest args)
            `(&key sources ,@(public-initargs 'prompter)))
  "Return `prompter' object.
The arguments are the initargs of the `prompter' class.

As a special case, the `:sources' keyword argument not only accepts `source'
objects but also symbols.

Example:
(prompter:make :sources 'prompter:raw-source)"
  (apply #'make-instance 'prompter args))

(export-always 'current-source)
(defun current-source (prompter)
  (first (current-suggestion prompter)))

(export-always '%current-suggestion)
(defun %current-suggestion (prompter)
  "Return PROMPTER's `current-suggestion'.
Return source as second value."
  (let ((source (first (current-suggestion prompter))))
    (values (nth (second (current-suggestion prompter)) (suggestions source))
            source)))

(export-always 'current-suggestion-position)
(defun current-suggestion-position (prompter)
  "Return PROMPTER's `current-suggestion' position among current `source'
suggestions."
  (second (current-suggestion prompter)))

(export-always 'all-marks)
(defun all-marks (prompter)
  "Return the list of `prompter''s `marks'.
Note that `marks' is a slot of `source', and `prompter' may have multiple
sources."
  (alex:mappend #'marks (sources prompter)))

(export-always 'all-suggestions)
(defun all-suggestions (prompter)
  "Return the list of PROMPTER's `suggestion's."
  (alex:mappend #'suggestions (sources prompter)))

(export-always 'default-action-on-return)
(defmethod default-action-on-return ((prompter prompter))
  (first (actions-on-return prompter)))

(export-always 'resume)
(defun resume (prompter)
  "Call each source `resumer' function over the source.
Meant to be called when PROMPTER is resumed."
  (mapc (lambda (source) (maybe-funcall (resumer source) source))
        (sources prompter)))
