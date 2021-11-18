;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

;; TODO: Use methods instead of slots?  Probably no, because we must be able to
;; handle anonymous sources / prompters.
;; TODO: Memoize suggestion computation?
;; TODO: User classes?  Probably useful mostly for `source' since
;; they may be defined globally.  Conversely, `prompter' is mostly used
;; locally.

;; TODO: Performance: plists are faster, especially when it comes to modifying
;; existing attributes.
;; TODO: Performance: Consider computing the attribute values only when active.
;; Lazy evaluation would make it easy.

(deftype function-symbol ()
  `(and symbol (satisfies fboundp)))

(defun object-public-slots (object-specifier)
  "Return the list of exported slots."
  (delete-if
   (complement #'exported-p)
   (mopu:slot-names object-specifier)))

(defun default-object-attributes (object)
  `(("Default" ,(princ-to-string object))))

(export-always 'object-attributes)
(defmethod object-attributes ((object t))
  "Return an alist of non-dotted pairs (ATTRIBUTE-KEY ATTRIBUTE-VALUE) for OBJECT.
Attributes are meant to describe the OBJECT structurally.
Both attribute-keys and attribute-values are strings.

For structure and class instances, the alist is made of the exported slots: the
keys are the sentence-cased slot names and the values the slot values passed to
`princ-to-string'.

It's used in `make-suggestion' which can be used as a `suggestion-maker' for `source's.

It's useful to separate concerns and compose between different object attributes
and different sources (for instance, the same `object-attributes' method can be
inherited or used across different sources)."
  (cond
    ((hash-table-p object)
     (let ((result))
       (maphash (lambda (key value)
                  (push (list (princ-to-string key)
                              (princ-to-string value))
                        result))
                object)
       (sort result #'string< :key #'first)))
    ((or (typep object 'standard-object)
         (typep object 'structure-object))
     (or
      (mapcar (lambda (slot)
                (list (string-capitalize (string slot))
                      (princ-to-string (slot-value object slot))))
              (object-public-slots object))
      (default-object-attributes object)))
    ((plist-p object)
     (let ((result '()))
       (alex:doplist (key value object result)
                     (push (list (string-capitalize key) (princ-to-string value))
                           result))
       (nreverse result)))
    ((undotted-alist-p object)
     (mapcar (lambda (pair)
               (list
                (princ-to-string (first pair))
                (princ-to-string (second pair))))
             object))
    ((alist-p object)
     (mapcar (lambda (pair)
               (list
                (princ-to-string (first pair))
                (princ-to-string (rest pair))))
             object))
    (t (default-object-attributes object))))

(define-class suggestion ()
  ((value nil ; TODO: Rename `data' as with the GHT?  Maybe confusing since we have `match-data'.
          :type t)
   (attributes '()
               :documentation "A non-dotted alist of attributes to structure the filtering.
Both the key and the value are strings.")
   (match-data nil
               :type t
               :documentation "Arbitrary data that can be used by the `filter'
function and its preprocessors.  It's the responsibility of the filter to ensure
the match-data is ready for its own use.")
   (score 0.0
          :documentation "A score the can be set by the `filter' function and
used by the `sort-predicate'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Suggestions are processed and listed in `source'.
It wraps arbitrary object stored in the `value' slot.
The other slots are optional.

Suggestions are made with the `suggestion-maker' slot from `source'."))

(defun pair-p (object)
  (and (listp object)
       (or (not (listp (rest object)))
           (null (rest (rest object))))))

(defun undotted-pair-p (object)
  (and (listp object)
       (listp (rest object))
       (null (rest (rest object)))))

(defun alist-p (object)
  "Return non-nil if OBJECT is an alist, dotted or undotted."
  (and (listp object)
       (every #'pair-p object)))

(defun undotted-alist-p (object &optional value-type)
  "If VALUE-TYPE is non-nil, check if all values are of the specified type."
  (and (listp object)
       (every #'undotted-pair-p object)
       (or (not value-type)
           (every (lambda (e) (typep (first e) value-type))
                  (mapcar #'rest object)))))

(defun plist-p (object)
  "Return non-nil if OBJECT is a plist."
  (and (listp object)
       (alex:proper-list-p object)
       (evenp (length object))
       (loop :for x :in object :by #'cddr
             :always (keywordp x))))

(defun object-attributes-p (object)
  (undotted-alist-p object 'string))

(defmethod attribute-key ((attribute t))
  (first attribute))
(defmethod attribute-value ((attribute t))
  (second attribute))
(defmethod attributes-keys ((attributes t))
  (mapcar #'attribute-key attributes))
(defmethod attributes-values ((attributes t))
  (mapcar #'attribute-value attributes))

(defun format-attributes (attributes)
  "Performance bottleneck: This function is called as many times as they are
suggestions."
  (sera:string-join (attributes-values attributes) " "))

(defmethod initialize-instance :after ((suggestion suggestion) &key)
  "Check validity."
  (unless (object-attributes-p (attributes suggestion))
    (warn "Attributes of ~s should be a non-dotted alist instead of ~s" (value suggestion) (attributes suggestion))
    (setf (attributes suggestion) (default-object-attributes (value suggestion)))))

(defun ensure-non-base-string (s)
  "Convert S to (simple-array character) type."
  (if (typep s 'base-string)
      ;; REVIEW: Maybe simpler to coerce to 'string?
      (coerce s `(simple-array character (,(length s))))
      s))

(defun ensure-match-data-string (suggestion source)
  "Return SUGGESTION's `match-data' as a string.
If unset, set it to the return value of `format-attributes'."
  (flet ((maybe-downcase (s)
           (if (current-input-downcase-p source)
               (string-downcase s)
               s)))
    (setf (match-data suggestion)
          (if (and (match-data suggestion)
                   (typep (match-data suggestion) 'string)
                   ;; mk-string-metrics requires the (simple-array character)
                   ;; type, but 'string should be enough.  See
                   ;; `mk-string-metrics:damerau-levenshtein'.
                   (not (typep (match-data suggestion) 'base-string)))
              (if (not (eq (last-input-downcase-p source)
                           (current-input-downcase-p source)))
                  (maybe-downcase (match-data suggestion))
                  (match-data suggestion))
              (let ((result (ensure-non-base-string
                             (format-attributes (attributes suggestion)))))
                (maybe-downcase result)))))
  (match-data suggestion))

(export-always 'make-suggestion)
(defmethod make-suggestion ((value t) &optional source input)
  "Return a `suggestion' wrapping around VALUE.
Attributes are set with `object-attributes'."
  (declare (ignore source input))
  (make-instance 'suggestion
                 :value value
                 :attributes (object-attributes value)))

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
with `suggestion-maker'.
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

   (active-attributes-keys '()
                           :export t
                           :accessor nil
                           :documentation "Keys of the suggestion attributes to
display and process when filtering.  An empty list means all attributes are
displayed.")

   (hide-attribute-header-p :never
                            :type (member :always :never :single)
                            :documentation "Let know the caller whether the
attribute names are meant to be displayed or not.
- When `:always', the column attribute header should be hidden.
- When `:never', the column attribute header should be shown.
- When `:single', it's hidden if there is only one active attribute.")
   (hide-suggestion-count-p nil
                            :type boolean
                            :documentation "Let know the caller whether the
suggestion count is meant to be displayed or not.")

   (suggestion-maker #'make-suggestion
                     :type (function (t &optional source string) suggestion)
                     :documentation "Function that wraps an arbitrary
object into a source `suggestion'.
This is useful to set the suggestion slots such as `attributes' and `match-data'
depending on the source and the input.

Called on
- arbitrary object
- (optional) source
- (optional) current input.")

   (filter #'fuzzy-match
           :type (or null
                     (function (suggestion source string) (or null suggestion))
                     function-symbol)
           :documentation
           "Takes a `suggestion', the `source' and the `input' and return a new
suggestion, or nil if the suggestion is discarded.")

   (filter-preprocessor #'delete-inexact-matches
                        :type (or null (function (list source string) list) function-symbol)
                        :documentation
                        "Function called when
input is modified, before filtering the suggestions.
It is passed the following arguments:
- a copy of `initial-suggestions';
- the source;
- the input.")

   (filter-postprocessor nil
                         :type (or null (function (list source string) list) function-symbol)
                         :documentation
                         "Function called when input is modified, after
filtering the suggestions with `filter'.
It is passed the following arguments:
- the filtered suggestions;
- the source;
- the input.")

   (current-input-downcase-p nil
                             :type boolean
                             :export nil
                             :documentation "Whether input is downcased.
This is useful for filters to avoid recomputing it every time.")
   (last-input-downcase-p nil
                          :type boolean
                          :export nil
                          :documentation "Whether previous input was
downcased.  This is useful to know if there is a case difference since last time
and to know if we have to recompute the match-data for instance.")

   (sort-predicate #'score>
                   :type (or null
                             (function (suggestion suggestion) boolean))
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

   (update-notifier (make-channel)
                    :type calispel:channel
                    :documentation "A channel which is written to when `filter'
commits a change to `suggestions'.  A notification is only sent if at least
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

   (update-thread nil
                  :type t
                  :export nil
                  :initarg nil
                  :documentation "Thread where the `filter-preprocessor', `filter' and
`filter-postprocessor' are run.  We store it in a slot so that we can terminate it.")

   (multi-selection-p nil
                      :type boolean
                      :documentation
                      "Whether multiple candidates can be marked.")

   (resumer nil
            :type (or null (function (source)))
            :documentation
            "Function meant to be called with the source as argument when the
prompter is resumed.
See `resume-sources'.")

   (follow-p nil
             :type boolean
             :documentation
             "Whether the first `follow-mode-functions' is automatically
executed.  Also see `follow-delay'.")

   (follow-delay 0.0
                 :documentation
                 "Execute the first `follow-mode-functions' after this delay
when `follow-p' is non-nil.")

   (follow-mode-functions '()
                          :type (or null
                                    (or function function-symbol)
                                    (cons (or function function-symbol) *))
                          :documentation
                          "The first function of this list is called
automatically on the selection when it's changed.  It does not interrupt or
return the prompter.
For convenience, it may be initialized with a single function, in which case it
will be automatically turned into a list."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
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
   (constructor '("yes" "no"))
   (hide-attribute-header-p :always))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompt source for yes-no questions."))

(defun make-input-suggestion (suggestions source input)
  (declare (ignore suggestions))
  (list (funcall (suggestion-maker source) input
                 source input)))

(define-class raw-source (source)
  ((name "Input")
   (filter-preprocessor 'make-input-suggestion)
   (filter nil)
   (hide-attribute-header-p :always)
   (hide-suggestion-count-p t)
   (multi-selection-p nil))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompt source for raw user input.
Its only suggestion is the user input, thus it has no constructor.
If you are looking for a source that just returns its plain suggestions, use `source'."))

(defmethod initialize-instance :after ((raw-source raw-source) &key)
  "Report error when RAW-SOURCE is provided bad initial arguments."
  (when (constructor raw-source)
    (error "Raw source should have no constructor: ~a" (constructor raw-source))))

(defun make-word-suggestions (suggestions source input)
  (declare (ignore suggestions))
  (mapcar (lambda (word)
            (funcall (suggestion-maker source) word
                     source input))
          (sera:words input)))

(define-class word-source (source)
  ((name "Input words")
   (filter-preprocessor 'make-word-suggestions)
   (filter nil)
   (hide-attribute-header-p :always)
   (multi-selection-p t))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompt source for user input words."))

(export-always 'ensure-suggestions-list)
(defmethod ensure-suggestions-list ((source source) elements)
  (lparallel:pmapcar
   (lambda (suggestion-value)
     (if (suggestion-p suggestion-value)
         suggestion-value
         (funcall (suggestion-maker source)
                  suggestion-value
                  source)))
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
         (calispel:! wait-channel t)))
     :name "Prompter source init thread")
    ;; Wait until above thread has acquired the `initial-suggestions-lock'.
    (calispel:? wait-channel))
  (setf (follow-mode-functions source)
        (uiop:ensure-list (follow-mode-functions source)))
  source)

(export-always 'attributes-keys-non-default)
(defmethod attributes-keys-non-default ((source source))
  "Return SOURCE attributes except the default one."
  (rest (attributes-keys source)))

(export-always 'attributes-keys-default)
(defmethod attributes-keys-default ((source source))
  "Return SOURCE default attribute as a non-dotted pair."
  (first (attributes-keys source)))

(export-always 'attributes-default)
(defmethod attributes-default ((suggestion suggestion))
  "Return SUGGESTION default attribute value."
  (second (first (attributes suggestion))))

(defmethod attributes-default ((object t))
  "Return OBJECT default attribute value.
Since the OBJECT is taken outside of a source context, attributes are derived
from `object-attributes'."
  (second (first (object-attributes object))))

(export-always 'attributes-non-default)
(defmethod attributes-non-default ((suggestion suggestion))
  "Return SUGGESTION non-default attributes."
  (rest (attributes suggestion)))

(defmethod attributes-keys ((source source))
  (attributes-keys
   (alex:if-let ((sugg (first (suggestions source)))) ; TODO: Instead, ensure that SUGGESTIONS always has an element?
     (attributes sugg)
     (default-object-attributes ""))))

(defmethod active-attributes-keys ((source source) &key &allow-other-keys)
  "Return active attributes keys.
If the `active-attributes' slot is NIL, return all attributes keys."
  (or (slot-value source 'active-attributes-keys)
      (attributes-keys source)))

(defmethod (setf active-attributes-keys) (value (source source))
  "Set active attributes to the intersection of VALUE and SOURCE attributes."
  (flet ((remove-from-seq (seq &rest items)
           (reduce (lambda (seq item) (remove item seq :test #'string=))
                   (set-difference seq items :test #'string=)
                   :initial-value seq)))
    (setf (slot-value source 'active-attributes-keys)
          (cons (attributes-keys-default source)
                (apply #'remove-from-seq (attributes-keys-non-default source) value)))))

(export-always 'active-attributes)
(defmethod active-attributes ((suggestion suggestion)
                              &key (source (error "Source required"))
                              &allow-other-keys)
  "Return the active attributes of SUGGESTION.
Active attributes are queried from SOURCE."
  (let ((inactive-keys (set-difference (attributes-keys (attributes suggestion))
                                       (active-attributes-keys source)
                                       :test #'string=)))
    (remove-if
     (lambda (attr)
       (find (attribute-key attr) inactive-keys :test #'string=))
     (attributes suggestion))))

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

(defun copy-object (object &rest slot-overrides)
  (let ((class-sym (class-name (class-of object))))
    (apply #'make-instance class-sym
           (append
            slot-overrides
            (alexandria:mappend
             (lambda (slot)
               (list (intern (symbol-name slot) "KEYWORD")
                     (slot-value object slot)))
             (mopu:slot-names class-sym))))))

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
             ;; This is prone to a race condition, but worst case we destroy an
             ;; already terminated thread.
             (bt:thread-alive-p (update-thread source)))
    ;; Note that we may be writing multiple times to this channel, but that's
    ;; OK, only the first value is read, so worst case the caller sees that the
    ;; source is terminated even though it just finished updating.
    (calispel:! (ready-channel source) nil)
    ;; Ignore errors in case thread is already terminated.
    ;; REVIEW: Is there a cleaner way to do this?
    (ignore-errors (bt:destroy-thread (update-thread source))))
  (setf (ready-channel source) new-ready-channel)
  (setf input (ensure-non-base-string input))
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
                                             (funcall (filter source) suggestion source input)))
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
                                            input))))))
             (wait-for-initial-suggestions)
             (setf (last-input-downcase-p source) (current-input-downcase-p source))
             (setf (current-input-downcase-p source) (str:downcasep input))
             (process!
              (preprocess
               ;; We copy the list of initial-suggestions so that the
               ;; preprocessor cannot modify them.
               (mapcar #'copy-object (initial-suggestions source))))
             (postprocess!)
             ;; Signal this source is done:
             (calispel:! new-ready-channel source)))
         :name "Prompter update thread")))
