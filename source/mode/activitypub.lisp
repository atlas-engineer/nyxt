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
  "A mode for ActivityPub-enabled pages browsing.

The `model' is usually the object that the current page was generated from. It's
mostly there for introspection purposes.

There is a number of helper functions to extract semantic data from ActivityPub
objects: `name*', `url*', `author*', `published*'.

Important methods to be avare of:
- `object->html' method allows you to override the way ActivityPub entities are
  rendered.
- `parse-object' ensures that the data one gets from the page is a proper
  `object', `link', `activity' etc.
- `fetch-object' fetches the object from the HTTP(S) URL and parses it to
  primitive Lisp form."
  ((rememberable-p nil)
   (model nil :type (maybe base))
   (style (theme:themed-css (theme *browser*)
            (.card
             :border-width "1px"
             :border-color theme:primary
             :border-style "solid"
             :padding "1em"
             :margin "1em")))
   (auth-token
    nil
    :type (maybe string)
    :allocation :class
    :documentation "The authorization token for Nyxt to handle ActivityPub auth.")
   (nickname
    nil
    :type (maybe string)
    :documentation "The nickname of the account currently logged in.")
   (instance
    nil
    :type (maybe quri:uri)
    :documentation "The URL of the home instance for the current account.")))

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

;; TODO: Wipe/clean it periodically to allow feed updates?
(defvar *url->object* (make-hash-table :test 'equal)
  "A memoization table from URL string to the fetched objects.")

(export-always 'fetch-object)
(defgeneric fetch-object (object)
  (:method ((object list))
    (lpara:pmapcar #'fetch-object object))
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
  (:method ((object list))
    (lpara:pmapcar #'parse-object object))
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
                        (json-name
                         ;; FIXME: Is there a better way to translate from CaMelCAsE?
                         &key (lisp-name (cffi:translate-camelcase-name json-name))
                           processor (literal-p (sera:true processor)))
                        (uiop:ensure-list slot)
                      (list json-name lisp-name processor literal-p)))
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
               for literal-p = (fourth slot)
               collect `(defmethod ,lisp-name ((object ,name))
                          ,(if literal-p
                               `(slot-value object (quote ,lisp-name))
                               `(when (slot-value object (quote ,lisp-name))
                                  (parse-object (slot-value object (quote ,lisp-name)))))))
       (defmethod fill-object ((object ,name) processed-json)
         (when (hash-table-p processed-json)
           ,@(loop for (json-name lisp-name processor) in normalized-slots
                   collect `(when (json-true-p (gethash ,json-name processed-json))
                              (setf (slot-value object (quote ,lisp-name))
                                    ,(if processor
                                         `(funcall ,processor (gethash ,json-name processed-json))
                                         `(gethash ,json-name processed-json))))))
         (call-next-method)))))


(define-json-type object "Object" (base)
  ("name" :literal-p t)
  ("nameMap" :literal-p t)
  "attachment" ; nested
  "attributedTo" ; nested
  "audience" ; nested
  ("content" :literal-p t)
  ("contentMap" :literal-p t)
  ("source" :literal-p t)
  ("context" :literal-p t)
  ("startTime" :processor #'local-time:parse-timestring)
  ("endTime" :processor #'local-time:parse-timestring)
  ("published" :processor #'local-time:parse-timestring)
  ("updated" :processor #'local-time:parse-timestring)
  ("duration" :literal-p t) ; time period
  "generator" ; nested
  "icon" ; nested
  "image" ; nested
  "location" ; nested
  "preview" ; nested
  "replies" ; nested
  ("summary" :literal-p t)
  ("summaryMap" :literal-p t)
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
  ("width" :literal-p t) ;; Mastodon adds this
  ("height" :literal-p t)  ;; Mastodon adds this
  )

(define-json-type link "Link" (base)
  ("href" :processor #'quri:uri)
  ("rel" :literal-p t)
  ("mediaType" :literal-p t)
  ("name" :literal-p t)
  ("hreflang" :literal-p t)
  ("height" :literal-p t)
  ("width" :literal-p t)
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
  ("preferredUsername" :literal-p t)
  "endpoints" ; nested
  "featured" ; Mastodon-specific
  )

(define-json-type base-collection "" (object)
  ("totalItems" :literal-p t)
  ("first" :lisp-name first-item) ; object
  ("last" :lisp-name last-item) ; object
  ("current" :lisp-name current-item) ; object
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
  ("startIndex" :literal-p t))

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
  ("closed" :processor #'local-time:parse-timestring))

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
  ("accuracy" :processor #'serapeum:parse-float)
  ("altitude" :processor #'serapeum:parse-float)
  ("latitude" :processor #'serapeum:parse-float)
  ("longitude" :processor #'serapeum:parse-float)
  ("radius" :processor #'serapeum:parse-float)
  ("units" :literal-p t))
(define-json-type mention "Mention" (link))
(define-json-type profile "Profile" (object)
  "describes" ; nested
  )
(define-json-type tombstone "Tombstone" (object)
  "formerType" ; nested
  ("deleted" :processor #'local-time:parse-timestring))

(defun http->ap (url)
  (or (alex:when-let* ((url url)
                       (valid (valid-url-p url))
                       (url (quri:uri url)))
        (str:concat "ap:" (nyxt::schemeless-url url)))
      url))

(defgeneric json-true-p (object)
  (:method ((object t)) object)
  (:method ((object string)) (not (uiop:emptyp object)))
  (:method ((object symbol)) (not (member object '(:null nil)))))

(defgeneric name* (object)
  ;; FIXME: This should not exists! Strong typing should be strong!
  (:mehtod ((object t)) "")
  (:method :around ((object t))
    (first (uiop:ensure-list (call-next-method))))
  (:method ((object sequence))
    (lpara:pmap (serapeum:class-name-of object) #'name* object))
  (:documentation "Return a human-readable name for the object.
Try to guess it from all the data available."))

(defmethod name* ((object actor))
  (cond
    ((json-true-p (name object)) (name object))
    ((json-true-p (preferred-username object)) (preferred-username object))
    (t (slot-value object 'id))))

(defmethod name* ((object object))
  (if (json-true-p (name object))
      (name object)
      (id object)))

(defmethod name* ((object link))
  (cond
    ((json-true-p (name object)) (name object))
    ((json-true-p (href object)) (quri:render-uri (href object)))
    (t (slot-value object 'id))))

(defgeneric author* (object)
  (:method :around ((object t))
    (first (uiop:ensure-list (call-next-method))))
  (:method ((object sequence))
    (lpara:pmap (serapeum:class-name-of object) #'author* object))
  (:documentation "Return the supposed original author of the OBJECT."))

(defmethod author* ((object object))
  (or (attributed-to object)
      (generator object)))

(defmethod author* ((object activity))
  (or (origin object)
      (and (object object)
           (author* (object object)))))

(defgeneric url* (object)
  (:method ((object string)) object)
  (:method :around ((object t))
    (let* ((urls (uiop:ensure-list (call-next-method)))
           (suitable-url (or (find #'quri:uri-https-p urls)
                             (find #'quri:uri-http-p urls)
                             (first urls))))
      (when (json-true-p suitable-url)
        (quri:render-uri (quri:uri suitable-url)))))
  (:method ((object sequence))
    (lpara:pmap (serapeum:class-name-of object) #'url* object))
  (:method ((object hash-table))
    (url* (or (gethash "href" object)
              (gethash "url" object))))
  (:document "Get the URL to the OBJECT that it can be referred to by."))

(defmethod url* ((object link))
  (slot-value object 'href))

(defmethod url* ((object object))
  (if (json-true-p (slot-value object 'url))
      (url* (slot-value object 'url))
      (slot-value object 'id)))

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

(defun render-image-card (object)
  (spinneret:with-html-string
    (:img :src (url* object)
          :alt (name* object))))

(defun render-video-card (object)
  (spinneret:with-html-string
    (:video :src (url* object) :controls t)))

(defmethod object->html ((object image) (format (eql :card)))
  (render-image-card object))

(defmethod object->html ((object video) (format (eql :card)))
  (render-video-card object))

(defmethod object->html ((object document) (format (eql :card)))
  (cond
    ((str:starts-with? "image/" (media-type object))
     (render-image-card object))
    ((str:starts-with? "video/" (media-type object))
     (render-video-card object))
    (t (object->html object :link))))

(defmethod object->html ((object note) (format (eql :card)))
  (spinneret:with-html-string
    (:p (:raw (content object)))
    (when (attachment object)
      (:raw (object->html (attachment object) :card)))))

(defmethod object->html ((object page) (format (eql :card)))
  (spinneret:with-html-string
    (when (name* object)
      (:h2 (:a :href (http->ap (slot-value object 'id))
               (name* object))))
    (when (attachment object)
      (:p (:raw (object->html (attachment object) :link))))))

(defmethod object->html ((object page) (format (eql :page)))
  (spinneret:with-html-string
    (when (name* object)
      (:h1 (name* object)))
    (:i "by " (when (author* object)
                (:a :href (slot-value (author* object) 'id) (name* (author* object)))))
    (when (content object)
      (:p (:raw (content object))))))

(defmethod published* ((object object))
  (alex:if-let ((time (some (lambda (x) (and x (not (eq :null x)) x))
                            (list (published object) (updated object) (start-time object)))))
    (local-time:format-timestring nil time :format local-time:+asctime-format+)
    "sometime"))

(defmethod object->html ((object activity) (format (eql :card)))
  (spinneret:with-html-string
    (:div
     :class "card"
     (:i (:a :href (http->ap (url* (actor object))) (name* (actor object)))
         (format nil " ~(~a~)-ed" (object-type object)))
     (:div (:raw (object->html (object object) :card))))))

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

(defmethod object->html ((object collection) (format (eql :page)))
  (declare (ignorable format))
  (object->html object :card))

(defmethod object->html ((object actor) (format (eql :card)))
  (declare (ignorable format))
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
    (when (summary object)
      (:raw (summary object)))
    (:br)
    (when (and (following object)
               (or (null (total-items (following object)))
                   (not (zerop (total-items (following object))))))
      (:raw (object->html (following object) :link)))
    (when (and (followers object)
               (or (null (total-items (followers object)))
                   (not (zerop (total-items (followers object))))))
      (:raw (object->html (followers object) :link)))
    (when (and (featured object)
               (or (null (total-items (followers object)))
                   (not (zerop (total-items (featured object))))))
      (:raw (object->html (featured object) :link)))
    (when (and (outbox object)
               (or (null (total-items (followers object)))
                   (not (zerop (total-items (outbox object))))))
      (:raw (object->html (outbox object) :card)))))

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
           nil)
         "text/html;charset=utf8")))
  :local-p t)
