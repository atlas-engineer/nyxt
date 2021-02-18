;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

;; TODO: Use methods instead of slots?  Probably no, because we must be able to
;; handle anonymous sources / prompters.
;; TODO: Memoize suggestion computation?
;; TODO: User classes?  Probably useful mostly for `prompter-source' since
;; they may be defined globally.  Conversely, `prompter' is mostly used
;; locally.

;; TODO: Rename `mutex' to `lock'.

(deftype must-match-choices ()
  `(or (eql :always)
       (eql :ignore)
       (eql :confirm)))

(defvar *default-history-size* 1000)    ; TODO: Export?

(defun make-history (&key (size *default-history-size*))
  "Return a new ring buffer."
  (containers:make-ring-buffer size :last-in-first-out))

(defun exported-p (sym)
  (eq :external
      (nth-value 1 (find-symbol (string sym)
                                (symbol-package sym)))))

(defun object-public-slots (object-specifier)
  "Return the list of exported slots."
  (delete-if
   (complement #'exported-p)
   (mopu:slot-names object-specifier)))

(export-always 'object-properties)
(defmethod object-properties ((object t))
  "Return a plist of properties of OBJECT.
For sturcture and class instances, the plist is made of the exported slots: the
keys are the slot symbols and the values the slot values passed to
`write-to-string'.

Suitable as a `prompter-source' `suggestion-property-function'."
  (cond
    ((or (typep object 'standard-object)
         (typep object 'structure-object))
     (or
      (alex:mappend (lambda (slot)
                      (list (intern (string slot) "KEYWORD")
                            (write-to-string (slot-value object slot))))
                    (object-public-slots object))
      (write-to-string object)))
    (t (list :default (write-to-string object)))))

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
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Suggestions are processed and listed in `prompter-source'.
It wraps arbitrary object stored in the `value' slot.
The other slots are optional."))

;; We must eval the class at read-time because `make-source' is generated using
;; the initargs of the class.
(sera:eval-always
  (define-class prompter-source ()      ; TODO: Rename `source'?
    ((name (error "Source must have a name")
           :documentation
           "Name which can be used to differentiate sources from one
another.")

     (constructor nil
                  :type (or null function)
                  :documentation
                  "Function called with the source as argument.
The returned value is assigned to `initial-suggestions'.
It is useful for instance to create the list of `initial-suggestions'
asynchronously, without blocking the creation of the prompt buffer..")

     (destructor nil
                 :type (or null function)
                 :documentation
                 "Function called with the source as parameter to clean it up.
It's called when `destroy' is called over `prompter'.")

     (initial-suggestions '()
                          :reader initial-suggestions
                          :export t
                          :documentation
                          "Suggestions used on initialization, before any
user input is processed.
On initialization this list is transformed to a list of `suggestion's
where properties are set from `suggestion-property-function'.
This list is never modified after initialization.")

     (initial-suggestions-lock (bt:make-lock)
                               :type bt:lock
                               :export nil
                               :documentation "Protect `initial-suggestions'
access.")

     (suggestions '()
                  :reader suggestions
                  :export t
                  :documentation
                  "The current list of suggestions.
It's updated asynchronously every time the prompter input is changed.
The slot is readable even when the computation hasn't finished.
See `ready-notifier' to know when the list is final.
See `update-notifier' to know when it has been updated, to avoid polling the
list.")

     (marked-suggestions '()
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
             :type (or null function)
             :documentation
             "Takes `input' and a `suggestion' and return a new suggestion, or
nil if the suggestion is discarded.")

     (filter-preprocessor #'delete-inexact-matches
                          :type (or null function)
                          :documentation
                          "Function called over a copy of `initial-suggestions', when
input is modified, before filtering the suggestions.")

     (filter-postprocessor nil
                           :type (or null function)
                           :documentation
                           "Function called over the prompter-source and the input,
when input is modified, after filtering the suggestions.")

     (sort-predicate #'score>
                     :type (or null function)
                     :documentation "A predicate used to sort the suggestions once
filtered.  The predicate works the same as the `sort' predicate.")

     (actions '(identity)
              :type (cons (or function symbol)) ; TODO: Accept function symbols?  Commands?
              :accessor nil
              :export nil
              :documentation "List of functions that can be run on suggestions
of this source.")

     (persistent-action nil ; TODO: Should be a list so we can support as many persistent actions as we want.
                        :type (or null function)
                        :documentation
                        "Function called over the selection without returning
from the prompter.")

     (persistent-help ""                ; TODO: Implement.
                      :type (or string function)
                      :documentation
                      "A string to explain persistent-action of this source. It also
accepts a function which takes the source as argument.")

     (multiline nil                     ; TODO: Unused?
                :type (or boolean integer)
                :documentation
                "If non-nil, each candidate can span over multiple lines.
If an integer, it specifies the maximum number of lines allow per candidate.")

     (requires-pattern 0                ; TODO: Use!
                       :documentation
                       "Compute and display suggestions only if the pattern has
at least this number of characters.  When 0, always compute and display
suggestions.")

     (update-notifier (make-channel)
                      :type calispel:channel
                      :documentation "A channel which is written to when `filter'
commits a change to `suggetsions'.  A notification is only send if at least
`notification-delay' has passed.  This is useful so that clients don't have to
poll `suggestions' for changes.")

     (notification-delay 0.1
                         :documentation "Time in seconds after which to notify
`update-notifier' if `suggestions' was modified.")

     (ready-channel nil
                    :type (or null calispel:channel)
                    :export nil
                    :documentation "Notify listener that source is ready.
The source object is sent to the channel.
If update calculation is aborted, nil is sent instead.")

     (wrote-to-ready-channel-p nil
                               :type boolean
                               :export nil
                               :documentation "Becomes non-nil when calculation
is done and the source was sent to the `ready-channel'.")

     (wrote-to-ready-channel-mutex (bt:make-lock)
                                   :type bt:lock
                                   :export nil
                                   :documentation "Protect
`wrote-to-ready-channel-p' access.")

     (update-thread nil
                    :type t
                    :export nil
                    :documentation "Thread where the `filter-preprocessor', `filter' and
`filter-postprocessor' are run.  We store it in a slot so that we can terminate it.")

     (suggestion-limit 0                ; TODO: Implement.
                       :documentation
                       "Don't display more suggestions than this.
If 0, there is no limit.")

     (multi-selection-p nil
                        :type boolean
                        :documentation
                        "Allow marking multiple candidates when this attribute is
present.")

     (resume nil                        ; TODO: Implement, rename.
             :type (or null function)
             :documentation
             "Function called with the source as argument when the prompter is
resumed.")

     (follow-p nil
               :type boolean
               :documentation
               "When non-nil, automatically execute `persistent-action'.
Also see `follow-delay'.")

     (follow-delay 0.0
                   :documentation
                   "Execute `persistent-action' after this delay when `follow-p' is
non-nil.")

     (must-match-p :always ; TODO: Remove and use dedicated source instead?  Then remove `return-input'.
                   :type (or must-match-choices null)
                   :documentation
                   "Control what to do when input does not match anything.
- `:always': Reject input.
- `:confirm': Prompt before accepting the input.
- `:ignore': Exit and do nothing.")

     (history (make-history)            ; TODO: Move to `prompter' class?
              :type (or containers:ring-buffer-reverse null)
              :documentation
              "History of inputs for the prompter.
If nil, no history is used.")

     (keymap nil
             :type (or null keymap:keymap)
             :documentation
             "Keymap specific to this source.")

     (help-message ""                   ; TODO: Use.
                   :type (or string function)
                   :documentation
                   "Help message for this source.
It can be a function of one argument, the prompter, which returns a string."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:documentation "A prompter source instance is meant to be used by a
`prompter' object.  See its `sources' slot.  A source is a consistent collection
of suggestions, filters, actions.

When a `prompter' `input' is set, the `update' function is called over all
sources.  This function pipelines `initial-suggestions' through
`filter-preprocessor', `filter', and finally `filter-postprocessor'.  If any of
these functions is nil, it's equivalent to passing the suggestions unchanged.

`filter-preprocessor' and `filter-postprocessor' are passed the whole list of
suggestions; they only set the `suggestions' once they are done.  Conversely,
`filter' is passed one suggestion at a time and it updates `suggestions' on each
call.")))


(export-always 'make-source)
(define-function make-source            ; TODO: Useless?
    (append '(&rest args)
            `(&key ,@(initargs 'prompter-source)))
  "Return prompter source object."
  (apply #'make-instance 'prompter-source args))

(define-class yes-no-source (prompter-source)
  ((name "Confirm")
   (initial-suggestions '("yes" "no")))
  (:export-class-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompt for yes-no questions."))

(defmethod initialize-instance :after ((source prompter-source) &key)
  (let ((wait-channel (make-channel 1)))
    (calispel:! wait-channel t)
    (bt:make-thread
     (lambda ()
       (bt:acquire-lock (initial-suggestions-lock source))
       (calispel:? wait-channel)
       ;; `initial-suggestions' initialization must be done before first input can be processed.
       (when (constructor source)
         (setf (slot-value source 'initial-suggestions)
               (funcall (constructor source) source)))
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
       (bt:release-lock (initial-suggestions-lock source))))
    ;; Wait until above thread has acquired the `initial-suggestions-lock'.
    (calispel:! wait-channel t))
  (unless (filter source)
    ;; If we have no filter, then we have no suggestions beside
    ;; immediate input, so we must allow them as valid suggestion.
    (setf (must-match-p source) nil))
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

(defun update (source input new-ready-channel) ; TODO: Store `input' in the source?
  "Update SOURCE to narrow down the list of suggestions according to INPUT.
If a previous suggestion computation was not finished, it is forcefully terminated.

- First the `filter-preprocessor' is run over a copy of `initial-suggestions'.
- The resulting suggestions are passed one by one to `filter'.
  When filter returns non-nil, the result is added to `suggestions' and
  `update-notifier' is notified, if `notification-delay' has been exceeded.
- Last the `filter-postprocessor' is run the SOURCE and the INPUT.
- Finally, `ready-notifier' is fired up."
  (when (and (update-thread source)
             ;; TODO: This is prone to a race condition.
             (bt:thread-alive-p (update-thread source)))
    (bt:with-lock-held ((wrote-to-ready-channel-mutex source))
      (unless (wrote-to-ready-channel-p source)
        (calispel:! (ready-channel source) nil)))
    ;; Destroy thread _after_ holding the lock, otherwise the lock may be held
    ;; forever.
    (bt:destroy-thread (update-thread source)))
  (setf (ready-channel source) new-ready-channel)
  (setf (wrote-to-ready-channel-p source) nil)
  (setf (update-thread source)
        (bt:make-thread
         (lambda ()
           ;; Wait until initial-suggestions are ready.
           (bt:acquire-lock (initial-suggestions-lock source))
           (bt:release-lock (initial-suggestions-lock source))
           (let ((last-notification-time (get-internal-real-time))
                 (preprocessed-suggestions (mapcar #'copy-object
                                                   (initial-suggestions source))))
             (setf preprocessed-suggestions
                   (if (filter-preprocessor source)
                       (maybe-funcall (filter-preprocessor source)
                                      preprocessed-suggestions source input)
                       (list (make-instance 'suggestion
                                            :value input
                                            :properties (maybe-funcall (suggestion-property-function source)
                                                                       input)
                                            :match-data ""))))
             ;; TODO: Should we really reset the suggestions here?
             (setf (slot-value source 'suggestions) '())
             (if (or (str:empty? input)
                     (not (functionp (filter source))))
                 (setf (slot-value source 'suggestions) preprocessed-suggestions)
                 (dolist (suggestion preprocessed-suggestions)
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
           (bt:with-lock-held ((wrote-to-ready-channel-mutex source))
             (unless (wrote-to-ready-channel-p source)
               (calispel:! new-ready-channel source)
               (setf (wrote-to-ready-channel-p source) t)))))))
