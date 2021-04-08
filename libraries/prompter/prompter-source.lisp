;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

;; TODO: Use methods instead of slots?  Probably no, because we must be able to
;; handle anonymous sources / prompters.
;; TODO: Memoize suggestion computation?
;; TODO: User classes?  Probably useful mostly for `source' since
;; they may be defined globally.  Conversely, `prompter' is mostly used
;; locally.

(deftype must-match-choices ()
  `(member :always :ignore :confirm))

(deftype function-symbol ()
  `(and symbol (satisfies fboundp)))

(defun object-public-slots (object-specifier)
  "Return the list of exported slots."
  (delete-if
   (complement #'exported-p)
   (mopu:slot-names object-specifier)))

(defun default-object-attributes (object)
  (list :default (princ-to-string object)))

(export-always 'object-attributes)
(defmethod object-attributes ((object t))
  "Return a plist of (ATTRIBUTE-KEY ATTRIBUTE-VALUE) for OBJECT.
Attributes are meant to describe the OBJECT structurally.
Attribute-values can be of arbitrary type.

For structure and class instances, the plist is made of the exported slots: the
keys are the slot symbols and the values the slot values passed to
`princ-to-string'.

It's used in `make-suggestion' which can be used as a `suggestion-maker' for `source's.

It's useful to separate concerns and compose between different object attributes
and different sources (for instance, the same `object-attributes' method can be
inherited or used across different sources)."
  (cond
    ((or (typep object 'standard-object)
         (typep object 'structure-object))
     (or
      (alex:mappend (lambda (slot)
                      (list (intern (string slot) "KEYWORD")
                            (princ-to-string (slot-value object slot))))
                    (object-public-slots object))
      (princ-to-string object)))
    (t (default-object-attributes object))))

(define-class suggestion ()
  ((value nil ; TODO: Rename `data' as with the GHT?  Maybe confusing since we have `match-data'.
          :type t)
   (attributes '()
               :documentation "A plist of attributes to structure the filtering.
The key is the attribute name and the value is a string.")
   (match-data nil
               :type t
               :writer t
               :documentation "Arbitrary data that can be used by the `filter'
function and its preprocessors.")
   (score 0.0
          :documentation "A score the can be set by the `filter' function and
used by the `sort-predicate'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Suggestions are processed and listed in `source'.
It wraps arbitrary object stored in the `value' slot.
The other slots are optional.

Suggestions are made with the `suggestion-maker' slot from `source'."))

(defun object-attributes-p (object)
  (and (listp object)
       (evenp (length object))))

(defun format-attributes (attributes &optional downcasedp)
  (funcall (if downcasedp #'string-downcase #'identity)
           (str:join " " (mapcar #'princ-to-string (sera:plist-values attributes)))))

(defmethod initialize-instance :after ((suggestion suggestion) &key source input)
  "Set SUGGESTION `match-data' if empty and if SOURCE and INPUT initargs are provided.
`match-data' is set by concatenating all the active attributes into a
space-separated string.
The `match-data' is downcased if INPUT is lower-case."
  (unless (object-attributes-p (attributes suggestion))
    (warn "Attributes of ~s should be a plist instead of ~s" (value suggestion) (attributes suggestion))
    (setf (attributes suggestion) (default-object-attribute (value suggestion))))
  (when (uiop:emptyp (match-data suggestion))
    (setf (match-data suggestion)
          (format-attributes
           (if source
               (active-attributes suggestion :source source)
               (attributes suggestion))
           (if input
               (str:downcasep input)
               :downcasep)))))

(defmethod match-data ((suggestion suggestion))
  (alex:if-let ((value (slot-value suggestion 'match-data)))
    value
    (setf (match-data suggestion)
          (format-attributes (attributes suggestion) :downcasedp))))

(export-always 'make-suggestion)
(defmethod make-suggestion ((value t) source &optional input)
  "Return a `suggestion' wrapping around VALUE.
Attributes are set with `object-attributes'."
  (make-instance 'suggestion
                 :value value
                 :attributes (object-attributes value)
                 :source source
                 :input input))

(define-class source ()
  ((name (error "Source must have a name")
         :documentation
         "Name which can be used to differentiate sources from one
another.")

   (constructor nil
                :type (or null list function)
                :documentation
                "Function or list to set `initial-suggestions'.
If a function, it's called asynchronously with the source as argument.
The returned value is assigned to `initial-suggestions'.

If a list, it's assigned synchronously to `initial-suggestions'.  The list is
guaranteed to never be modified.")

   (destructor nil
               :type (or null function)
               :documentation
               "Function called with the source as parameter to clean it up.
It's called when `destroy' is called over `prompter'.")

   (initial-suggestions '()
                        :reader initial-suggestions
                        :documentation
                        "Suggestions used on initialization, before any
user input is processed.
On initialization this list is transformed to a list of `suggestion's
where attributes are set from `suggestion-attribute-function'.
This list is never modified after initialization.")

   (initial-suggestions-lock (bt:make-lock)
                             :type bt:lock
                             :export nil
                             :initarg nil
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

   (marks '()
          :documentation
          "The list of suggestion values which have been marked by the user.
Marking is only allowed when `multi-selection-p' is non-nil.
When suggestions are marked, the subsequent action is run over all marked suggestions.

We store the values instead of the suggestion because suggestions objects are
reinstantiated between each input processing.")

   (active-attributes '()
                      :export t
                      :accessor nil
                      :documentation "Suggestion attributes to display and
process when filtering.  A suggestion `object-attributes' method should return a
plist of attribute names and string values.  An empty list means all attributes
are displayed.")

   (suggestion-maker #'make-suggestion
                     :documentation "Function that wraps an arbitrary
object into a source `suggestion'.
This is useful to set the suggestion slots such as `attributes' and `match-data'
depending on the source and the input.

Called on
- arbitrary object
- source
- (optional) current input.")

   (filter #'fuzzy-match
           :type (or null function function-symbol)
           :documentation
           "Takes `input' and a `suggestion' and return a new suggestion, or
nil if the suggestion is discarded.")

   (filter-preprocessor #'delete-inexact-matches
                        :type (or null function)
                        :documentation
                        "Function called when
input is modified, before filtering the suggestions.
It is passed the following arguments:
- a copy of `initial-suggestions';
- the source;
- the input.")

   (filter-postprocessor nil
                         :type (or null function)
                         :documentation
                         "Function called when input is modified, after
filtering the suggestions with `filter'.
It is passed the following arguments:
- the filtered suggestions;
- the source;
- the input.")

   (sort-predicate #'score>
                   :type (or null function)
                   :documentation "A predicate used to sort the suggestions once
filtered.  The predicate works the same as the `sort' predicate.")

   (actions '(identity)
            :type list
            :accessor nil
            :export nil
            :documentation "List of funcallables that can be run on suggestions
of this source.
This is the low-level implementation, see the `actions' function for the public
interface.")

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
                  :initarg nil
                  :documentation "Notify listener that source is ready.
The source object is sent to the channel.
If update calculation is aborted, nil is sent instead.")

   (wrote-to-ready-channel-p nil
                             :type boolean
                             :export nil
                             :initarg nil
                             :documentation "Whether the source was sent
to the `ready-channel'.")

   (wrote-to-ready-channel-lock (bt:make-lock)
                                :type bt:lock
                                :export nil
                                :initarg nil
                                :documentation "Protect
`wrote-to-ready-channel-p' access.")

   (update-thread nil
                  :type t
                  :export nil
                  :initarg nil
                  :documentation "Thread where the `filter-preprocessor', `filter' and
`filter-postprocessor' are run.  We store it in a slot so that we can terminate it.")

   (suggestion-limit 0       ; TODO: Implement.  Move to Nyxt's prompt-buffer?
                     :documentation
                     "Don't display more suggestions than this.
If 0, there is no limit.")

   (multi-selection-p nil
                      :type boolean
                      :documentation
                      "Whether multiple candidates can be marked.")

   (resumer nil
            :type (or null function)
            :documentation
            "Function meant to be called with the source as argument when the
prompter is resumed.
See `resume-sources'.")

   (follow-p nil
             :type boolean
             :documentation
             "Whether `persistent-action' is automatically executed.
Also see `follow-delay'.")

   (follow-delay 0.0
                 :documentation
                 "Execute `persistent-action' after this delay when `follow-p' is
non-nil.")

   (must-match-p :always ; TODO: Remove and use dedicated source instead?  Then remove `return-input'.
                 :type (or must-match-choices boolean) ; TODO: Should not allow T, should we?  Anyways, we can probably remove this slot.
                 :documentation
                 "Control what to do when input does not match anything.
- `:always': Reject input.
- `:confirm': Prompt before accepting the input.
- `:ignore': Exit and do nothing.")

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
call."))

(define-class yes-no-source (source)
  ((name "Confirm")
   (constructor '("yes" "no")))
  (:export-class-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompt source for yes-no questions."))

(defun make-input-suggestion (suggestions source input)
  (declare (ignore suggestions))
  (list (funcall (suggestion-maker source) input source)))

(define-class raw-source (source)
  ((name "Input")
   (filter-preprocessor 'make-input-suggestion)
   (filter nil)
   (multi-selection-p nil))
  (:export-class-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompt source for raw user input."))

(defun make-word-suggestions (suggestions source input)
  (declare (ignore suggestions))
  (mapcar (lambda (word)
            (funcall (suggestion-maker source) word source))
          (sera:words input)))

(define-class word-source (source)
  ((name "Input words")
   (filter-preprocessor 'make-word-suggestions)
   (filter nil)
   (multi-selection-p t))
  (:export-class-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompt source for user input words."))

(defmethod ensure-suggestions-list ((source source) elements
                                    &key input &allow-other-keys)
  (mapcar (lambda (suggestion-value)
            (if (suggestion-p suggestion-value)
                suggestion-value
                (funcall (suggestion-maker source)
                         suggestion-value
                         source
                         input)))
          (uiop:ensure-list elements)))

(defmethod initialize-instance :after ((source source) &key)
  "See the `constructor' documentation of `source'."
  (let ((wait-channel (make-channel)))
    (bt:make-thread
     (lambda ()
       (bt:acquire-lock (initial-suggestions-lock source))
       ;; `initial-suggestions' initialization must be done before first input can be processed.
       (cond ((listp (constructor source))
              (setf (slot-value source 'initial-suggestions)
                    (constructor source)))
             ((constructor source)
              ;; Run constructor asynchronously.
              (calispel:! wait-channel t)
              (setf (slot-value source 'initial-suggestions)
                    (funcall (constructor source) source))))
       (setf (slot-value source 'initial-suggestions)
             (ensure-suggestions-list source (initial-suggestions source)))
       ;; TODO: Setting `suggestions' is not needed?
       (setf (slot-value source 'suggestions) (initial-suggestions source))
       (bt:release-lock (initial-suggestions-lock source))
       (when (listp (constructor source))
         ;; Initial suggestions are set synchronously in this case.
         (calispel:! wait-channel t))))
    ;; Wait until above thread has acquired the `initial-suggestions-lock'.
    (calispel:? wait-channel))
  (unless (filter source)
    ;; If we have no filter, then we have no suggestions beside
    ;; immediate input, so we must allow them as valid suggestion.
    (setf (must-match-p source) nil))
  source)

;; TODO: Attribute keys should be strings.  Should attributes be objects?
(defmethod source-attributes ((source source)) ; We don't name this `attributes' so that it's unexported.
  (sera:plist-keys
   (alex:if-let ((sugg (first (suggestions source)))) ; TODO: Instead, ensure that suggestions always has an element?
     (attributes sugg)
     (default-object-attributes ""))))

(export-always 'attributes-non-default)
(defmethod attributes-non-default ((source source))
  "Return SOURCE attributes except the default one."
  (rest (source-attributes source)))

(export-always 'attributes-default)
(defmethod attributes-default ((source source))
  "Return SOURCE default attribute."
  (first (source-attributes source)))

(defmethod attributes-default ((suggestion suggestion))
  "Return SUGGESTION default attribute value."
  (second (attributes suggestion)))

(defmethod attributes-non-default ((suggestion suggestion))
  "Return SUGGESTION non-default attributes as a plist."
  (cddr (attributes suggestion)))

(defmethod active-attributes ((source source) &key &allow-other-keys)
  "Return active attributes.
If the `active-attributes' slot is NIL, return all attributes."
  (or (slot-value source 'active-attributes)
      (source-attributes source)))

(defmethod (setf active-attributes) (value (source source))
  "Set active attributes to the intersection of VALUE and SOURCE attributes."
  (flet ((remove-from-seq (seq &rest items)
           (reduce (lambda (seq item) (remove item seq))
                   (set-difference seq items)
                   :initial-value seq)))
    (setf (slot-value source 'active-attributes)
          (cons (attributes-default source)
                (apply #'remove-from-seq (attributes-non-default source) value)))))

(defmethod active-attributes ((suggestion suggestion)
                              &key (source (error "Source required"))
                              &allow-other-keys)
  "Return the active attributes of SUGGESTION.
Active attributes are queried from SOURCE."
  (apply #'alex:remove-from-plist
         (attributes suggestion)
         (set-difference (sera:plist-keys (attributes suggestion))
                         (active-attributes source))))

(export-always 'marked-p)
(defun marked-p (source value)
  (find value (prompter:marks source) :test #'equalp))

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
  "Update SOURCE to narrow down the list of `suggestions' according to INPUT.
If a previous suggestion computation was not finished, it is forcefully terminated.

- First the `filter-preprocessor' is run over a copy of `initial-suggestions'.
- The resulting suggestions are passed one by one to `filter'.
  When filter returns non-nil, the result is added to `suggestions' and
  `update-notifier' is notified, if `notification-delay' has been exceeded or if
  the last suggestion has been processed.
- Last the `filter-postprocessor' is run the SOURCE and the INPUT.
  Its return value is assigned to the list of suggestions.
- Finally, `ready-notifier' is fired up.

The reason we filter in 3 stages is to allow both for asynchronous and
synchronous filtering.  The benefit of asynchronous filtering is that it sends
feedback to the user while the list of suggestions is being computed."
  (when (and (update-thread source)
             ;; TODO: This is prone to a race condition.
             (bt:thread-alive-p (update-thread source)))
    (bt:with-lock-held ((wrote-to-ready-channel-lock source))
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
           (flet ((wait-for-initial-suggestions ()
                    (bt:acquire-lock (initial-suggestions-lock source))
                    (bt:release-lock (initial-suggestions-lock source)))
                  (preprocess (initial-suggestions-copy)
                    (if (filter-preprocessor source)
                        (ensure-suggestions-list
                         source
                         (funcall (filter-preprocessor source)
                                  initial-suggestions-copy source input))
                        initial-suggestions-copy))
                  (process! (preprocessed-suggestions)
                    (let ((last-notification-time (get-internal-real-time)))

                      (setf (slot-value source 'suggestions) '())
                      (if (or (str:empty? input)
                              (not (filter source)))
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
                                (when (or (> duration (notification-delay source))
                                          (= (length (slot-value source 'suggestions))
                                             (length preprocessed-suggestions)))
                                  (calispel:! (update-notifier source) t)
                                  (setf last-notification-time now))))))))
                  (postprocess! ()
                    (when (filter-postprocessor source)
                      (setf (slot-value source 'suggestions)
                            (ensure-suggestions-list
                             source
                             (maybe-funcall (filter-postprocessor source)
                                            (slot-value source 'suggestions)
                                            source
                                            input)
                             :input input)))))
             (wait-for-initial-suggestions)
             (process!
              (preprocess
               ;; We copy the list of initial-suggestions so that the
               ;; preprocessor cannot modify them.
               (mapcar (lambda (suggestion)
                         (funcall (suggestion-maker source)
                          (value suggestion)
                          source
                          input))
                       (initial-suggestions source))))
             (postprocess!)
             (bt:with-lock-held ((wrote-to-ready-channel-lock source))
               (unless (wrote-to-ready-channel-p source)
                 (calispel:! new-ready-channel source)
                 (setf (wrote-to-ready-channel-p source) t))))))))
