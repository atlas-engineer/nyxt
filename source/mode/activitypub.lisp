;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/activitypub-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:-> #:export-always)
  (:documentation "Mode for ActivityPub browsing."))
(in-package :nyxt/activitypub-mode)
(use-nyxt-package-nicknames)

;; TODO: Compact URLs
;; TODO: Parsing JSON schemes automatically.
;; TODO: Parse @context.

(define-mode activitypub-mode ()
  ""
  ((rememberable-p nil)
   (model nil :type (maybe base))
   (style (theme:themed-css (theme *browser*)
            (.card
             :border-width "1px"
             :border-color theme:primary
             :border-style "solid"
             :padding "1em")))
   (auth-token
    nil
    :type (maybe string)
    :documentation "The authorization token for Nyxt to handle ActivityPub auth.")))

(export-always 'base)
(define-class base ()
  ((id nil :type (or string null))
   (object-type "Object" :type string)
   (original-object nil :type (maybe hash-table)
                    :documentation "The de-serialized JSON object this one was created from.
Possibly contains additional Lisp-inaccessible properties."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "The base class for all the ActivityStreams types."))

(export-always 'fill-object)
(defgeneric fill-object (object json)
  (:method ((object t) json)
    (declare (ignore json))
    object)
  (:method ((object base) json)
    (when (hash-table-p json)
      (setf (id object) (gethash "id" json)
            (object-type object) (gethash "type" json)
            (original-object object) json))
    (call-next-method))
  (:documentation "Fill the OBJECT based on the information from JSON.

Should always CALL-NEXT-METHOD, so that all the superclasses are filled too."))

(defvar *url->object* (make-hash-table :test 'equal)
  "A memoization table from URL string to the fetched objects.")

(export-always 'fetch-object)
(defgeneric fetch-object (object)
  (:method ((object string))
    (when (valid-url-p object)
      (log:debug "Fetching ~a." object)
      (flet ((get-object ()
               (ignore-errors
                (parse-object
                 (decode-json
                  (dex:get object :headers
                           `(("Accept" . "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
                             ,@(alex:when-let* ((mode (current-mode 'activitypub))
                                                (auth (auth-token mode)))
                                 `(("Authorization" . ,(str:concat "Bearer " auth)))))))))))
        (or (alex:ensure-gethash object *url->object* (get-object))
            (get-object)))))
  (:method ((object quri:uri))
    (fetch-object (quri:render-uri object)))
  (:documentation "Fetch the object from the provided URL."))

(defvar *classes* (make-hash-table :test 'equalp)
  "A map from ActivityStreams/ActivityPub type name to the Lisp-side class symbol.")

(sera:export-always 'parse-object)
(defgeneric parse-object (object)
  (:method ((object t))
    object)
  (:documentation "Parse the object from the provided JSON data.
Possibly recurse to the nested sub-objects."))

(defmethod parse-object ((object hash-table))
  (or (sera:and-let* ((type (gethash "type" object))
                      (class (gethash type *classes*)))
        (fill-object (make-instance class) object))
      object))

(defmethod parse-object ((object sequence))
  (lpara:pmap (serapeum:class-name-of object) #'parse-object object))

(defmethod parse-object ((object string))
  (or (sera:and-let* ((valid (valid-url-p object))
                      (fetched (fetch-object object)))
        (fill-object (parse-object fetched) fetched))
      object))

(defmacro define-json-type (name type (&rest superclasses) &body names-and-slots)
  "Define a JSON-serializable ActivityPub type with a Lisp class mirroring it.

NAME is an unquoted symbol naming the Lisp-side class.
TYPE is a string with a ActivityPub object type.
SUPERCLASSES are Lisp superclasses to fill slots from in addition to the direct ones.

NAMES-AND-SLOTS is a list of (JSON-NAME &optional NAME PROCESSOR) forms or just
JSON-NAMEs as strings, where
- JSON-NAME is the string matching the JSON name of the property to fill the
  slot from.
- NAME is the (unquoted) symbol for the Lisp-side slot storing the information.
- PROCESSOR is a form evaluating to a function object. The function designated
  by this object should take the raw string value of a JSON-NAMEd property and
  produce a Lisp value matching it."
  (let ((normalized-slots
          (mapcar (lambda (slot)
                    (destructuring-bind
                        (json-name &optional lisp-name processor)
                        (uiop:ensure-list slot)
                      (list json-name (or lisp-name (cffi:translate-camelcase-name json-name)) processor)))
                  names-and-slots)))
    `(progn
       (setf (gethash ,type *classes*) (quote ,name))
       (define-class ,name (,@superclasses)
         (,@(mapcar (lambda (slot) `(,(second slot) :accessor nil)) normalized-slots))
         (:export-class-name-p t)
         (:export-accessor-names-p t)
         (:accessor-name-transformer (class*:make-name-transformer name)))
       ,@(loop for slot in normalized-slots
               for lisp-name = (second slot)
               collect `(defmethod ,lisp-name ((object ,name))
                          (cond
                            ((valid-url-p (slot-value object (quote ,lisp-name)))
                             (fetch-object (slot-value object (quote ,lisp-name))))
                            (t (parse-object (slot-value object (quote ,lisp-name)))))))
       (defmethod fill-object ((object ,name ) processed-json)
         (when (hash-table-p processed-json)
           ,@(loop for (json-name lisp-name processor) in normalized-slots
                   collect `(when (and (gethash ,json-name processed-json)
                                       (not (eq :null (gethash ,json-name processed-json))))
                              ;; FIXME: Is there a better way to translate from CaMelCAsE?
                              (setf (slot-value object (quote ,lisp-name))
                                    ,(if processor
                                         `(funcall ,processor (gethash ,json-name processed-json))
                                         `(gethash ,json-name processed-json))))))
         (call-next-method)))))


(define-json-type object "Object" (base)
  "name"
  "nameMap"
  "attachment" ; nested
  "attributedTo"
  "audience" ; nested
  "content"
  "contentMap"
  "source"
  "context"
  ("startTime" start-time #'local-time:parse-timestring)
  ("endTime" end-time #'local-time:parse-timestring)
  ("published" published #'local-time:parse-timestring)
  ("updated" updated #'local-time:parse-timestring)
  "duration" ; time period
  "generator" ; nested
  "icon" ; nested
  "image" ; nested
  "location" ; nested
  "preview" ; nested
  "replies" ; nested
  "summary"
  "summaryMap"
  "tag" ; nested
  "inReplyTo" ; nested
  "url" ; nested
  "to" ; nested
  "bto" ; nested
  "cc" ; nested
  "bcc" ; nested
  "mediaType"
  "likes" ; nested
  "shares" ; nested
  "width" ;; Mastodon adds this
  "height"  ;; Mastodon adds this
  )

(define-json-type link "Link" (base)
  ("href" href #'quri:uri)
  "rel"
  "mediaType"
  "name"
  "hreflang"
  "height"
  "width"
  "preview" ; nested
  )

(define-json-type base-activity "" (object)
  "actor" ; nested
  "target" ; nested
  "result" ; nested
  "origin" ; nested
  "instrument" ; nested
  )

(define-json-type activity "Activity" (base-activity)
  "object" ; nested
  )

(define-json-type intransitive-activity "IntransitiveActivity" (base-activity))

(define-json-type actor "" (object)
  "inbox" ; nested
  "outbox" ; nested
  "following" ; nested
  "followers" ; nested
  "liked" ; nested
  "streams" ; nested
  "preferredUsername"
  "endpoints" ; nested
  "featured" ; Mastodon-specific
  )

(define-json-type base-collection "" (object)
  "totalItems"
  ("first" first-item) ; object
  ("last" last-item) ; object
  ("current" current-item) ; object
  )

(define-json-type collection "Collection" (base-collection)
  "items" ; nested
  )

(define-json-type ordered-collection "OrderedCollection" (collection)
  "orderedItems" ; nested
  )

(define-json-type collection-page "CollectionPage" (collection)
  "partOf" ; nested
  "next" ; nested
  "prev" ; nested
  )

(define-json-type ordered-collection-page "OrderedCollectionPage" (collection-page ordered-collection)
  "startIndex")

;;; Activity Vocabulary Actor Types (https://www.w3.org/TR/activitystreams-vocabulary/#actor-types)

(define-json-type application "Application" (actor))
(define-json-type group "Group" (actor))
(define-json-type organization "Organization" (actor))
(define-json-type person "Person" (actor))
(define-json-type service "Service" (actor))

;;; Activity Vocabulary Activity Types (https://www.w3.org/TR/activitystreams-vocabulary/#activity-types)

(define-json-type accept-activity "Accept" (activity))
(define-json-type tentative-accept-activity "TentativeAccept" (accept-activity))
(define-json-type add-activity "Add" (activity))
(define-json-type arrive-activity "Arrive" (intransitive-activity))
(define-json-type create-activity "Create" (activity))
(define-json-type delete-activity "Delete" (activity))
(define-json-type follow-activity "Follow" (activity))
(define-json-type ignore-activity "Ignore" (activity))
(define-json-type block-activity "Block" (ignore-activity))
(define-json-type join-activity "Join" (activity))
(define-json-type leave-activity "Leave" (activity))
(define-json-type like-activity "Like" (activity))
(define-json-type offer-activity "Offer" (activity))
(define-json-type invite-activity "Invite" (offer-activity))
(define-json-type reject-activity "Reject" (activity))
(define-json-type tentative-reject-activity "TentativeReject" (reject-activity))
(define-json-type remove-activity "Remove" (activity))
(define-json-type undo-activity "Undo" (activity))
(define-json-type update-activity "Update" (activity))
(define-json-type view-activity "View" (activity))
(define-json-type listen-activity "Listen" (activity))
(define-json-type read-activity "Read" (activity))
(define-json-type move-activity "Move" (activity))
(define-json-type travel-activity "Travel" (intransitive-activity))
(define-json-type announce-activity "Announce" (activity))
(define-json-type flag-activity "Flag" (activity))
(define-json-type dislike-activity "Dislike" (activity))
(define-json-type question-activity "Question" (intransitive-activity)
  "oneOf" ; nested
  "anyOf" ; nested
  ("closed" closed #'local-time:parse-timestring))

;;; Activity Vocabulary Activity Types (https://www.w3.org/TR/activitystreams-vocabulary/#object-types)

(define-json-type relationship "Relationship" (object)
  "subject" ; nested
  "object" ; nested
  "relationship" ; nested
  )
(define-json-type article "Article" (object))
(define-json-type document "Document" (object))
(define-json-type audio "Audio" (document))
(define-json-type image "Image" (document))
(define-json-type video "Video" (document))
(define-json-type note "Note" (object))
(define-json-type page "Page" (document))
(define-json-type event "Event" (object))
(define-json-type place "Place" (object)
  ("accuracy" accuracy #'serapeum:parse-float)
  ("altitude" altitude #'serapeum:parse-float)
  ("latitude" latitude #'serapeum:parse-float)
  ("longitude" longitude #'serapeum:parse-float)
  ("radius" radius #'serapeum:parse-float)
  "units")
(define-json-type mention "Mention" (link))
(define-json-type profile "Profile" (object)
  "describes" ; nested
  )
(define-json-type tombstone "Tombstone" (object)
  "formerType" ; nested
  ("deleted" deleted #'local-time:parse-timestring))

(defun http->ap (url)
  (or (alexandria:when-let* ((url url)
                             (url (quri:uri url)))
        (str:concat "ap:" (nyxt::schemeless-url url)))
      ""))


;; FIXME: This should not exists! Strong typing should be strong!
(defmethod name* ((object t)) "")

(defgeneric json-true-p (object)
  (:method ((object t)) object)
  (:method ((object string)) (not (uiop:emptyp object)))
  (:method ((object symbol)) (not (member object '(:null nil)))))

(defmethod name* ((object actor))
  (if (json-true-p (name object))
      (name object)
      (preferred-username object)))

(defmethod name* ((object object))
  (if (json-true-p (name object))
      (name object)
      (id object)))

(defmethod name* ((object link))
  (cond
    ((json-true-p (name object)) (name object))
    ((json-true-p (slot-value object 'href)) (quri:render-uri (slot-value object 'href)))
    (t (slot-value object 'id))))

(defgeneric object->html (object format)
  (:method ((object base) (format (eql :link)))
    (spinneret:with-html-string
      (:a :class "button"
          :href (http->ap (id object))
          (name* object))))
  (:method ((object link) (format (eql :link)))
    (spinneret:with-html-string
      (:a :class "button"
          :href (http->ap (slot-value object 'href))
          (name* object))))
  (:method ((objects list) format)
    (spinneret:with-html-string
      (:raw
       (apply #'str:concat
              (lpara:pmapcar
               (lambda (object)
                 (spinneret:with-html-string (:raw (object->html object format))))
               objects)))))
  (:method ((object t) format)
    (spinneret:with-html-string
      (:a :class "button"
          :href object
          (:raw (value->html object (eq :link format))))))
  (:documentation "Produce a reasonable HTML representation of an OBJECT according to FORMAT.
FORMAT can be one of
- :LINK for condensed link to OBJECT.
- :CARD for compact yet informative representation of OBJECT.
- :PAGE for full-page OBJECT rendering (is usually linked to by :LINK format)."))

(defmethod object->html ((object document) (format (eql :card)))
  (if (str:starts-with? "image" (media-type object))
      (spinneret:with-html-string
        (:img :src (slot-value object 'url)
              :alt (when (and (name object)
                              (not (eq :null (name object))))
                     (name object))))
      (object->html object :link)))

(defmethod object->html ((object note) (format (eql :card)))
  (spinneret:with-html-string
    (:p (:raw (content object)))
    (when (attachment object)
      (:raw (object->html (attachment object) :card)))))

(defmethod object->html ((object page) (format (eql :card)))
  (spinneret:with-html-string
    (when (slot-value object 'name)
      (:h2 (slot-value object 'name)))
    (when (content object)
      (:p (:raw (content object))))
    (when (attachment object)
      (:p (:raw (object->html (attachment object) :link))))))

(defmethod published* ((object object))
  (alex:if-let ((time (some (lambda (x) (and x (not (eq :null x)) x))
                            (list (published object) (updated object) (start-time object)))))
    (local-time:format-timestring nil time :format local-time:+asctime-format+)
    "sometime"))

(defmethod object->html ((object create-activity) (format (eql :card)))
  (spinneret:with-html-string
    (:div
     :class "card"
     (:i (:a :href (id (actor object)) (name* (actor object)))
         " created/posted on " (published* object))
     (:raw (object->html (object object) :card)))))

(defmethod object->html ((object announce-activity) (format (eql :card)))
  (spinneret:with-html-string
    (:div
     :class "card"
     (:i (:a :href (id (actor object)) (name* (actor object)))
         (let ((author (or (origin object)
                           (and (object object)
                                (or (attributed-to (object object))
                                    (generator (object object)))))))
           (when author
             (:i " (originally by " (:a :href (http->ap (or (id author) (url author))) (name* author)) ")")))
         " announced on " (published* object))
     (:raw (object->html (object object) :card)))))

(defmethod object->html ((object string) (format (eql :link)))
  (if (valid-url-p object)
      (spinneret:with-html-string
        (:a :class "button"
            :href object
            (:b "[unsupported] ") (nyxt::schemeless-url (quri:uri object))))
      (call-next-method)))

;; FIXME: What is a "card view" for a collection? A card with condensed
;; link-like content or a container for cards? It used to be the former, now
;; it's the latter.
(defmethod object->html ((object collection) (format (eql :card)))
  (declare (ignorable format))
  (let* ((items (items object))
         (ordered-items (ordered-items object))
         (items (or items ordered-items))
         (first-item (first-item object))
         (id (nyxt::get-unique-identifier *browser*)))
    (spinneret:with-html-string
      (:div
       :id id
       (cond
         (items
          (dolist (item items)
            (:raw (object->html item :card))))
         ((collection-page-p first-item)
          (dolist (item (or (items first-item) (ordered-items first-item)))
            (:raw (object->html item :card))))
         ((base-p first-item)
          (loop for item = first-item then (next item)
                collect (:raw (object->html item :card)))))
       (when (collection-page-p first-item)
         (unless (equal first-item (last-item object))
           (:a :class "button" :href (http->ap (id object)) "More...")))))))

(defmethod object->html ((object actor) (format (eql :card)))
  (spinneret:with-html-string
    (:div
     :class "card"
     (:h2 (:a :href (http->ap (id object))
              (format nil "~a (@~a)" (name* object) (preferred-username object))))
     (:raw (summary object)))))

(defmethod object->html ((object actor) (format (eql :page)))
  (spinneret:with-html-string
    (:header
     (:h1 (format nil "~a (@~a)" (name* object) (preferred-username object))))
    (:raw (summary object))
    (:br)
    (when (and (following object)
               (not (zerop (total-items (following object)))))
      (:raw (object->html (following object) :link)))
    (when (and (followers object)
               (not (zerop (total-items (followers object)))))
      (:raw (object->html (followers object) :link)))
    (when (and (featured object)
               (not (zerop (total-items (featured object)))))
      (:raw (object->html (featured object) :link)))
    (:raw (object->html (outbox object) :card))))

(define-internal-scheme "ap"
    (lambda (url buffer)
      (enable-modes '(activitypub-mode) buffer)
      (let ((mode (current-mode 'activitypub)))
        (values
         (alex:if-let ((object (setf (model mode)
                                     (ignore-errors
                                      (fetch-object (str:concat "https://" (subseq url 3)))))))
           (spinneret:with-html-string
             (:head
              (:style (style buffer))
              (:style (style mode)))
             (:body
              (:raw (object->html object :page))))
           (error-help "Not an ActivityPub-enabled page"
                       "This link does not provide ActivityPub representation for content.
Try navigating to a different page."))
         "text/html;charset=utf8")))
  :local-p t)
