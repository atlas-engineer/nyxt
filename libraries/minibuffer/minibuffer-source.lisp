;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :minibuffer)

;; TODO: Use methods instead of slots?  Probably no, because we must be able to
;; handle anonymous sources / minibuffers.
;; TODO: Memoize suggestion computation?
;; TODO: Pre-defined minibuffers: yes-no.
;; TODO: User classes?  Probably useful mostly for `minibuffer-source' since
;; they may be defined globally.  Conversely, `minibuffer' is mostly used
;; locally.
;; TODO: Implement minibuffer actions.

(deftype must-match-choices ()
  `(or (eql :always)
       (eql :ignore)
       (eql :confirm)))

(defvar *default-history-size* 1000)    ; TODO: Export?

(defun make-history (&key (size *default-history-size*))
  "Return a new ring buffer."
  (containers:make-ring-buffer size :last-in-first-out))

(export-always 'object-properties)
(defmethod object-properties ((object t))
  "Suitable as a `minibuffer-source' `suggestion-property-function'."
  (list :default (write-to-string object)))

(define-class suggestion ()
  ((value nil
          :type t)
   (properties '()
               :documentation "A plist of properties to structure the filtering.
The key is the property name and the value is a string.")
   (match-data nil
               :type t
               :documentation "Arbitrary data that can be used by the `filter'
function and its preprocessors.")
   (score 0.0
          :documentation "A score the can be set by the `filter' function and
used by the `sort-predicate'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Suggestions are processed and listed in `minibuffer-source'.
It wraps arbitrary object stored in the `value' slot.
The other slots are optional."))

;; We must eval the class at read-time because `make-source' is generated using
;; the initargs of the class.
(sera:eval-always
  (define-class minibuffer-source ()
    ((name ""
           :documentation
           "Name which can be used to differentiate sources from one
another.")

     (initializer nil
                  :type (or null function)
                  :documentation
                  "Function called with the source as argument.
It is useful for instance to create the list of `initial-suggestions'.")

     (cleanup nil ; TODO: Better name? `finalizer' is more fit to an after-init function.
              :type (or null function)
              :documentation
              "Function called with the source as parameter to clean it up.
It's called when `cleanup' is called over `minibuffer'.

Note that the function is executed *before* performing any action.")

     (initial-suggestions '()
                          :reader initial-suggestions
                          :documentation
                          "Suggestions used on initialization, before any
user input is processed.
On initialization this list is transformed to a list of `suggestion's
where properties are set from `suggestion-property-function'.
This list is never modified after initialization.")

     (suggestions '()
                  :reader suggestions
                  :export t
                  :documentation
                  "The current list of suggestions.
It's updated asynchronously every time the minibuffer input is changed.
The slot is readable even when the computation hasn't finished.
See `ready-notifier' to know when the list is final.
See `update-notifier' to know when it has been updated, to avoid polling the
list.")

     (marked-suggestions '()            ; TODO: Implement.
                         :documentation
                         "The list of suggestions which have been marked by the user.
Marking is only allowed when `multi-selection-p' is non-nil.
When suggestions are marked, the subsequent action is run over all marked suggestions.")

     (active-properties '()
                        :documentation "Suggestion properties to display and
process when filtering.  A suggestion `object-properties' method should return a
plist of property names and string values.  An empty list means all properties
are displayed.")

     (suggestion-property-function #'object-properties ; TODO: Better name?
                                   :documentation "Function called on the
suggestions to derive their list of properties.  To control which property to
display and match against, see `active-properties'.")

     (filter #'fuzzy-match
             :type function
             :documentation
             "Takes `input' and filters the suggestions.")

     (filter-preprocessor #'delete-inexact-matches
                          :type (or null function)
                          :documentation
                          "Function called over a copy of `initial-suggestions', when
input is modified, before filtering the suggestions.")

     (filter-postprocessor nil
                           :type (or null function)
                           :documentation
                           "Function called over the minibuffer-source and the input,
when input is modified, after filtering the suggestions.")

     (sort-predicate #'score>
                     :type (or null function)
                     :documentation "A predicate used to sort the suggestions once
filtered.  The predicate works the same as the `sort' predicate.")

     (actions '()                       ; TODO: Implement!
              :type list)

     (persistent-action nil
                        :type (or null function)
                        :documentation
                        "Function called with the selected candidate.")

     (persistent-help ""
                      :type (or string function)
                      :documentation
                      "A string to explain persistent-action of this source. It also
accepts a function which takes the source as argument.")

     (multiline nil
                :type (or boolean integer)
                :documentation
                "If non-nil, each candidate can span over multiple lines.
If an integer, it specifies the maximum number of lines allow per candidate.")

     (requires-pattern 0
                       :documentation
                       "Compute and display suggestions only if the pattern has
at least this number of characters.  When 0, always compute and display
suggestions.")

     (ready-notifier nil
                     :type (or null calispel:channel)
                     :documentation "A channel which is written to when `filter-postprocessor'.")

     (update-notifier nil
                      :type (or null calispel:channel)
                      :documentation "A channel which is written to when `filter'
commits a change to `suggetsions'.  A notification is only send if at least
`notification-delay' has passed.  This is useful so that clients don't have to
poll `suggestions' for changes.")

     (notification-delay 0.1
                         :documentation "Time in seconds after which to notify
`update-notifier' if `suggestions' was modified.")

     (update-thread nil
                    :type t
                    :export nil
                    :documentation "Thread where the `filter-preprocessor', `filter' and
`filter-postprocessor' are run.  We store it in a slot so that we can terminate it.")

     (suggestion-limit 0                ; TODO: Implement!
                       :documentation
                       "Don't display more suggestions than this.
If 0, there is no limit.")

     (multi-selection-p nil
                        :type boolean
                        :documentation
                        "Allow marking multiple candidates when this attribute is
present.")

     (resume nil
             :type (or null function)
             :documentation
             "Function called with the source as argument when the minibuffer is
resumed.")

     (follow nil                        ; TODO: Implement.
             :type boolean
             :documentation
             "When non-nil, automatically execute `persistent-action'.
Also see `follow-delay'.")

     (follow-delay 0.0                  ; TODO: Implement.
                   :documentation
                   "Execute `persistent-action' after this delay when `follow' i
non-nil.")

     (must-match-p :always
                   :type must-match-choices
                   :documentation
                   "Control what to do when input does not match anything.
- `:always': Reject input.
- `:confirm': Prompt before accepting the input.
- `:ignore': Exit and do nothing.")

     (history (make-history)            ; TODO: Move to `minibuffer' class?
              :type (or containers:ring-buffer-reverse null)
              :documentation
              "History of inputs for the minibuffer.
If nil, no history is used.")

     (keymap nil
             :type (or null keymap:keymap)
             :documentation
             "Keymap specific to this source.")

     (help-message ""
                   :type (or string function)
                   :documentation
                   "Help message for this source.
It can be a function of one argument, the minibuffer, which returns a string."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer #'class*:name-identity)
    (:documentation "")))

(export-always 'make-source)
(define-function make-source
    (append '(&rest args)
            `(&key ,@(initargs 'minibuffer-source)))
  "Return minibuffer source object."
  (apply #'make-instance 'minibuffer-source args))

(defmethod initialize-instance :after ((source minibuffer-source) &key)
  ;; TODO: Should we always do this?  What if initial-suggestions are already
  ;; suggestion objects?
  (setf (slot-value source 'initial-suggestions)
        (mapcar (lambda (suggestion-value)
                  (make-instance 'suggestion
                                 :value suggestion-value
                                 :properties (maybe-funcall (suggestion-property-function source)
                                                            suggestion-value)
                                 :match-data ""))
                (initial-suggestions source)))
  ;; TODO: Setting `suggestions' is not needed?
  (setf (slot-value source 'suggestions) (initial-suggestions source))
  ;; TODO: Run this in parallel. Must be done before first input can be processed.
  (maybe-funcall (initializer source) source)
  source)

(defun filtered-properties-suggestion (suggestion properties)
  "Return a new suggestion with only PROPERTIES."
  (uiop:remove-plist-keys (if properties
                              (set-difference
                               (sera:plist-keys (properties suggestion))
                               properties)
                              nil)
                          (properties suggestion)))

(defun copy-object (object)
  "Like `copy-structure' but also works for class instances."
  (let ((copy (make-instance (class-name (class-of object)))))
    (dolist (slot (mopu:slot-names object))
      (setf (slot-value copy slot) (slot-value object slot)))
    copy))

(defun maybe-funcall (fn &rest args)
  "Funcall FN over args.
If FN is nil, return ARGS as multiple values."
  (if fn
      (apply fn args)
      (apply #'values args)))

(defun insert-item-at (item pred sequence) ; TODO: Arg order? Name?
  "Insert ITEM in SEQUENCE after the last item FOO for which (PRED ITEM FOO) is
non-nil."
  (if sequence
      (let ((item-pos
              (or (position-if (lambda (e) (funcall pred item e)) sequence)
                  (length sequence))))
        (nconc (subseq sequence 0 item-pos)
               (list item)
               (subseq sequence item-pos)))
      (list item)))

(defun make-channel (&optional size)
  "Return a channel of capacity SIZE.
If SIZE is NIL, capicity is infinite."
  (cond
    ((null size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:unbounded-fifo-queue)))
    ((= 0 size)
     (make-instance 'calispel:channel))
    ((< 0 size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity size)))))

(defun update (source input)            ; TODO: Store `input' in the source?
  "Update SOURCE to narrow down the list of suggestions according to INPUT.
If a previous suggestion computation was not finished, it is forcefully terminated.

- First the `filter-preprocessor' is run over a copy of `initial-suggestions'.
- The resulting suggestions are passed one by one to `filter'.
  When filter returns non-nil, the result is added to `suggestions' and
  `update-notifier' is notified, if `notification-delay' has been exceeded.
- Last the `filter-postprocessor' is run the SOURCE and the INPUT.
- Finally, `ready-notifier' is fired up."
  (when (update-thread source)
    (bt:destroy-thread (update-thread source)))
  (unless (update-notifier source)
    (setf (update-notifier source)
          (make-channel)))
  (unless (ready-notifier source)
    (setf (ready-notifier source)
          (make-channel 1)))
  ;; Drain ready-notifier in case it wasn't read.
  (calispel:? (ready-notifier source) 0)
  (setf (update-thread source)
        (bt:make-thread
         (lambda ()
           (let ((last-notification-time (get-internal-real-time))
                 (suggestions (if (filter-preprocessor source)
                                  (funcall (filter-preprocessor source)
                                           (initial-suggestions source) source input)
                                  (mapcar #'copy-object (initial-suggestions source)))))
             ;; TODO: Should we really reset the suggestions here?
             (setf (slot-value source 'suggestions) '())
             (unless (or (str:empty? input)
                         (not (functionp (filter source))))
               (dolist (suggestion suggestions)
                 (sera:and-let* ((processed-suggestion
                                  (funcall (filter source) input suggestion)))
                   (setf (slot-value source 'suggestions)
                         (insert-item-at suggestion (sort-predicate source)
                                         (suggestions source)))
                   (let* ((now (get-internal-real-time))
                          (duration (/ (- now last-notification-time)
                                       internal-time-units-per-second)))
                     (when (> duration (notification-delay source))
                       (calispel:! (update-notifier source) t)
                       (setf last-notification-time now)))))))

           ;; TODO: Pass `filter-preprocessor' result to source in case filter is not run?
           (maybe-funcall (filter-postprocessor source) source input)
           (calispel:! (ready-notifier source) t)))))
