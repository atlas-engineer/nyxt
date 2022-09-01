;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/activitypub-mode
    (:shadow #:profile)
    (:documentation "Mode for ActivityPub browsing."))
(in-package :nyxt/activitypub-mode)
(use-nyxt-package-nicknames)

;; TODO: Compact URLs
;; TODO: Parse JSON schemes automatically.
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
- `unparse-object' serializes the object to the ActivityPub-friendly JSON.
- `fetch-object' fetches the object from the HTTP(S) URL and parses it to
  primitive Lisp form.
- `send-object' sends the objects to the account outbox (given you are logged
  in)."
  ((rememberable-p nil)
   (model nil :type (maybe base))
   (style (theme:themed-css (theme *browser*)
            (.card
             :border-width "1px"
             :border-color theme:primary
             :border-style "solid"
             :padding "1em"
             :margin-top "1em"
             :margin-bottom "1em")
            ("img, video"
             :max-height "80vh"
             :max-width "80vw")))
   (auth-token
    nil
    :type (maybe string)
    :allocation :class
    :documentation "The authorization token for Nyxt to handle ActivityPub auth.")
   (nickname
    nil
    :type (maybe string)
    :allocation :class
    :documentation "The nickname of the account currently logged in.")
   (instance
    nil
    :type (maybe quri:uri)
    :allocation :class
    :documentation "The URL of the home instance for the current account.")
   (instance-type
    :mastodon
    :type (maybe keyword)
    :allocation :class
    :documentation "The type of the instance -- Mastodon (implied by default), Plume, Lemmy, etc.
Rendering may be different for different types.")
   (profile
    nil
    :type (maybe actor)
    :allocation :class
    :documentation "The object for user profile as an `actor' object.")))

(define-command activitypub-login
  (&key
   (mode (find-submode 'nyxt/activitypub-mode:activitypub-mode))
   (nickname (prompt1
               :prompt "Username"
               :sources (list (make-instance 'prompter:raw-source))))
   (instance (prompt1
               :prompt "Instance you're registered at"
               :sources (list (make-instance 'prompter:raw-source))))
   (auth-token (prompt1
                :prompt (format nil "Your authorization token for ~a" instance)
                :invisible-input-p t
                :sources (list (make-instance 'prompter:raw-source)))))
  "Log into the ActivityPub on a chosen instance under a chosen nickname."
  (let ((instance-url (nyxt::ensure-url
                       ;; Do we have any helper functions for this?
                       (if (or (str:starts-with-p "https://" instance)
                               (str:starts-with-p "http://" instance))
                           instance
                           (str:concat "https://" instance)))))
    (setf (auth-token mode) auth-token
          (nickname mode) nickname
          (instance mode) instance-url
          (profile mode) (fetch-object (sera:lret ((url (quri:copy-uri instance-url)))
                                         (setf (quri:uri-path url) (str:concat "/users/" nickname)))))))

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
                             ,@(alex:when-let* ((mode (find-submode 'nyxt/activitypub-mode:activitypub-mode))
                                                (auth (auth-token mode)))
                                 `(("Authorization" . ,(str:concat "Bearer " auth)))))))))))
        (or (alex:ensure-gethash object *url->object* (get-object))
            (get-object)))))
  (:method ((object quri:uri))
    (fetch-object (quri:render-uri object)))
  (:documentation "Fetch the object from the provided URL."))

(export-always 'send-object)
(defgeneric send-object (object)
  (:method ((object list))
    (lpara:pmapcar #'send-object object))
  (:method ((object base))
    (alex:if-let ((mode (find-submode 'nyxt/activitypub-mode:activitypub-mode))
                  (prof (profile (find-submode 'nyxt/activitypub-mode:activitypub-mode))))
      (multiple-value-bind (content code headers)
          (dex:post (slot-value prof 'outbox)
                    :headers `(("Content-Type" . "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
                               ,@(alex:when-let* ((auth (auth-token mode)))
                                   `(("Authorization" . ,(str:concat "Bearer " auth)))))
                    :content (unparse-object object))
        (declare (ignore content))
        (cond
          ((and (= code 201) headers (hash-table-p headers))
           (or (gethash "Location" headers) t))
          ((= code 201) t)
          (t nil)))
      (echo-warning
       "You are not logged in. Run `activitypub-login' to be able to post and follow others.")))
  (:documentation "Send the object to the current account outbox."))

(defvar *classes* (make-hash-table :test 'equalp)
  "A map from ActivityStreams/ActivityPub type name to the Lisp-side class symbol.")

(defmethod initialize-instance :after ((object base) &key original-object)
  (let ((class-name (sera:class-name-of object)))
    (setf (object-type object)
          (or (when (hash-table-p original-object)
                (gethash "type" original-object))
              (block find-type
                (maphash (lambda (type class)
                           (when (eq class class-name)
                             (return-from find-type type)))
                         *classes*))
              "Object"))
    (when (hash-table-p original-object)
      (setf (id object) (gethash "id" original-object)))))

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
        (make-instance class :original-object object))
      object))

(defmethod parse-object ((object sequence))
  (lpara:pmap (serapeum:class-name-of object) #'parse-object object))

(defmethod parse-object ((object string))
  (or (sera:and-let* ((valid (valid-url-p object))
                      (fetched (fetch-object object)))
        (parse-object fetched))
      object))

(sera:export-always 'unparse-object)
(defgeneric unparse-object (object &optional hash)
  (:method :around (object &optional hash)
    (call-next-method object (or hash (make-hash-table :test 'equal))))
  (:method ((object t) &optional hash)
    hash)
  (:method ((object base) &optional (hash (make-hash-table :test 'equal)))
    (encode-json (or (original-object object)
                     (sera:lret ((hash hash))
                       (when (id object)
                         (setf (gethash "id" hash) (id object)))
                       (setf (gethash "type" hash) (object-type object))
                       (setf (gethash "@context" hash) "https://www.w3.org/ns/activitystreams")))
                 nil))
  (:documentation "Produce the JSON for the OBJECT.
Should always CALL-NEXT-METHOD with the half-filled HASH so that superclass methods fill it too."))

(defmacro define-json-type (name type (&rest superclasses) &body names-and-slots)
  "Define a JSON-serializable ActivityPub type with a Lisp class mirroring it.

NAME is an unquoted symbol naming the Lisp-side class.
TYPE is a string with a ActivityPub object type.
SUPERCLASSES are Lisp superclasses to fill slots from in addition to the direct ones.

NAMES-AND-SLOTS is (JSON-NAME &key LISP-NAME PROCESSOR DEPROCESSOR LITERAL-P)
forms list or just JSON-NAMEs as strings, where

- JSON-NAME is the string matching the JSON name of the property to fill the
  slot from.
- LISP-NAME is the (unquoted) symbol for the Lisp-side slot storing the information.
- PROCESSOR is a form evaluating to a function object. The function designated
  by this object should take the raw string value of a JSON-NAMEd property and
  produce a Lisp value matching it.
- DEPROCESSOR is a form evaluating to a function object. The resulting function
  takes a non-nil object and encodes it to a valid Lisp value serializable to
  JSON. DEPROCESSOR should ideally perfectly reverse the effect of the
  PROCESSOR.
- LITERAL-P denotes whether the object is in its final form when accessed. If
  LITERAL-P is false (it is by default, unless PROCESSOR is set), slot accessor
  might try to fetch the object from the remote URL and/or parse the slot
  value."
  (let ((normalized-slots
          (mapcar (lambda (slot)
                    (destructuring-bind
                        (json-name
                         ;; FIXME: Is there a better way to translate from CaMelCAsE?
                         &key (lisp-name (cffi:translate-camelcase-name json-name))
                           processor deprocessor (literal-p (sera:true processor)))
                        (uiop:ensure-list slot)
                      (list json-name lisp-name processor deprocessor literal-p)))
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
       (defmethod unparse-object ((object ,name) &optional hash)
         ,@(loop for (json-name lisp-name . rest) in normalized-slots
                 for deprocessor = (second rest)
                 collect `(when (json-true-p (slot-value object (quote ,lisp-name)))
                            (setf (gethash ,json-name hash)
                                  ,(if deprocessor
                                       `(funcall ,deprocessor (slot-value object (quote ,lisp-name)))
                                       `(slot-value object (quote ,lisp-name))))))
         (call-next-method object hash))
       (defmethod initialize-instance :after ((object ,name) &key original-object)
         (when (hash-table-p original-object)
           ,@(loop for (json-name lisp-name processor) in normalized-slots
                   collect `(when (json-true-p (gethash ,json-name original-object))
                              (setf (slot-value object (quote ,lisp-name))
                                    ,(if processor
                                         `(funcall ,processor (gethash ,json-name original-object))
                                         `(gethash ,json-name original-object))))))))))

(defun unparse-timestring (timestamp)
  (when timestamp
    (local-time:format-timestring nil timestamp :format local-time:+rfc3339-format+)))

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
  ("startTime" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring)
  ("endTime" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring)
  ("published" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring)
  ("updated" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring)
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
  ("href" :processor #'quri:uri :deprocessor #'quri:render-uri)
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
  ("accuracy" :literal-p t)
  ("altitude" :literal-p t)
  ("latitude" :literal-p t)
  ("longitude" :literal-p t)
  ("radius" :literal-p t)
  ("units" :literal-p t))
(define-json-type mention "Mention" (link))
(define-json-type profile "Profile" (object)
  "describes" ; nested
  )
(define-json-type tombstone "Tombstone" (object)
  "formerType" ; nested
  ("deleted" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring))

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

(defmacro jwhen (condition &body body)
  `(when (json-true-p ,condition)
     ,@body))

(defmacro jor (&rest args)
  `(or ,@(loop for arg in args
               collecting `(jwhen ,arg ,arg))))

(defgeneric name* (object)
  ;; FIXME: This should not exists! Strong typing should be strong!
  (:method ((object t)) "")
  (:method :around ((object t))
    (first (uiop:ensure-list (call-next-method))))
  (:method ((object sequence))
    (lpara:pmap (serapeum:class-name-of object) #'name* object))
  (:documentation "Return a human-readable name for the object.
Try to guess it from all the data available."))

(defmethod name* ((object actor))
  (jor (name object) (preferred-username object) (id object)))

(defmethod name* ((object object))
  (jor (name object) (id object)))

(defmethod name* ((object link))
  (jor (name object) (id object) (render-url (href object))))

(defgeneric author* (object)
  (:method :around ((object t))
    (first (uiop:ensure-list (call-next-method))))
  (:method ((object sequence))
    (lpara:pmap (serapeum:class-name-of object) #'author* object))
  (:documentation "Return the supposed original author of the OBJECT."))

(defmethod author* ((object object))
  (jor (attributed-to object)
           (generator object)))

(defmethod author* ((object activity))
  (jor (origin object)
           (and (object object)
                (author* (object object)))))

(defgeneric url* (object)
  (:method ((object string)) object)
  (:method :around ((object t))
    (let* ((urls (uiop:ensure-list (call-next-method)))
           (suitable-url (or (find #'quri:uri-https-p urls)
                             (find #'quri:uri-http-p urls)
                             (first urls))))
      (jwhen suitable-url
        (quri:render-uri (quri:uri suitable-url)))))
  (:method ((object sequence))
    (lpara:pmap (serapeum:class-name-of object) #'url* object))
  (:method ((object hash-table))
    (url* (or (gethash "href" object)
              (gethash "url" object))))
  (:documentation "Get the URL to the OBJECT that it can be referred to by."))

(defmethod url* ((object link))
  (href object))

(defmethod url* ((object object))
  (cond
    ((stringp (slot-value object 'url)) (slot-value object 'url))
    ((json-true-p (url object))
     (if (link-p (first (uiop:ensure-list (url object))))
         (url* (url object))
         (slot-value object 'url)))
    (t (id object))))

(defmethod published* ((object object))
  (alex:if-let ((time (jor (published object) (updated object) (start-time object))))
    (local-time:format-timestring nil time :format local-time:+asctime-format+)
    "sometime"))

;;; Object rendering

(defgeneric object->html (object format)
  (:method ((object base) (format (eql :link)))
    (spinneret:with-html-string
      (:a :class "button"
          :href (http->ap (id object))
          (name* object))))
  (:method ((object base) (format (eql :page)))
    (spinneret:with-html-string
      (:h1 (name* object))
      (:raw (object->html object :card))))
  (:method ((object link) (format (eql :link)))
    (spinneret:with-html-string
      (:a :class "button"
          :href (http->ap (href object))
          (name* object))))
  (:method ((objects list) format)
    (spinneret:with-html-string
      (:raw
       (apply #'str:concat
              (lpara:pmapcar
               (lambda (object)
                 (spinneret:with-html-string (:raw (object->html object format))))
               objects)))))
  (:method ((object string) (format (eql :link)))
    (if (valid-url-p object)
        (spinneret:with-html-string
          (:a :class "button"
              :href object
              (:b "[unsupported] ") (nyxt::schemeless-url (quri:uri object))))
        (call-next-method)))
  (:method ((object t) format)
    (spinneret:with-html-string
      (:raw (value->html object (eq :link format)))))
  (:documentation "Produce a reasonable HTML representation of an OBJECT according to FORMAT.
FORMAT can be one of
- :LINK for condensed link to OBJECT.
- :CARD for compact yet informative representation of OBJECT.
- :PAGE for full-page OBJECT rendering (is usually linked to by :LINK format)."))

(defun anchor (object)
  (spinneret:with-html-string
    (:a :href (http->ap (url* object)) (name* object))))

(defun render-image-card (object)
  (spinneret:with-html-string
    (:img :src (url* object)
          :alt (name* object))))

(defun render-video-card (object)
  (spinneret:with-html-string
    (:video :src (url* object) :controls t)))

(defun render-audio-card (object)
  (spinneret:with-html-string
    (:audio :src (url* object) :controls t :preload t)))

(defmacro with-card (&body body)
  `(spinneret:with-html-string
    (:div :class "card" ,@body)))

(defmethod object->html ((object image) (format (eql :card)))
  (render-image-card object))

(defmethod object->html ((object video) (format (eql :card)))
  (render-video-card object))

(defmethod object->html ((object audio) (format (eql :card)))
  (render-audio-card object))

(defmethod object->html ((object document) (format (eql :card)))
  (cond
    ((str:starts-with? "image/" (media-type object))
     (render-image-card object))
    ((str:starts-with? "video/" (media-type object))
     (render-video-card object))
    ((str:starts-with? "audio/" (media-type object))
     (render-audio-card object))
    (t (object->html object :link))))

(defmethod object->html ((object tombstone) (format (eql :card)))
  (with-card
    (:h2 (or (name* object) "Ooops..."))
    (:p (former-type object) " used to be there, but it no longer is.")))

(defmethod object->html ((object profile) (format (eql :card)))
  (object->html (describes object) :card))

(defmethod object->html ((object note) (format (eql :card)))
  (with-card
    (:h2 (:raw (anchor (author* object))))
    (:p (:raw (content object)))
    (jwhen (attachment object)
      (:raw (object->html (attachment object) :card)))))

(defmethod object->html ((object page) (format (eql :card)))
  (spinneret:with-html-string
    (:h2 (:raw (anchor object)))
    (jwhen (content object)
      (:pre (sera:ellipsize (string-trim sera:whitespace (plump:text (plump:parse (content object)))) 300)))
    (jwhen (url* object)
      (:raw (anchor object)))))

(defun render-html-page (object)
  (spinneret:with-html-string
    (jwhen (name* object)
      (:h1 (name* object)))
    (:i "by " (jwhen (author* object) (:raw (anchor (author* object)))))
    (jwhen (content object)
      (:p (:raw (content object))))))

(defmethod object->html ((object page) (format (eql :page)))
  (render-html-page object))

(defmethod object->html ((object article) (format (eql :page)))
  (render-html-page object))

;; Activity card rendering

(defmethod object->html ((object activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (actor object))) (format nil " ~(~a~)-ed" (object-type object)))
    (:div (:raw (object->html (object object) :card)))))

(defmethod object->html ((object create-activity) (format (eql :card)))
  (object->html (object object) :card))

(defmethod object->html ((object add-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (actor object)))
        " added " (jwhen (target object)
                    (:span "into " (:a :href (http->ap (url* (target object)))
                                       (name* (target object))))))
    (:div (:raw (object->html (object object) :card)))))

(defmethod object->html ((object delete-activity) (format (eql :card)))
  (with-card
    (:i (:a :href (http->ap (url* (actor object))) (name* (actor object)))
        " deleted " (jwhen (origin object)
                      (:span "from " (:raw (anchor (origin object))))))
    (:div (:raw (object->html (object object) :card)))))

(defmethod object->html ((object invite-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (actor object)))
        " invited " (:raw (anchor (object object)))
        (jwhen (target object)
          (:span " into " (:raw (anchor (target object))))))))

(defmethod object->html ((object offer-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (actor object)))
        " offered " (:raw (anchor (object object)))
        (jwhen (target object)
          (:span " to " (:raw (anchor (target object))))))))

(defmethod object->html ((object remove-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (actor object)))
        " removed " (:raw (anchor (object object)))
        (jwhen (origin object)
          (:span " from " (:raw (anchor (origin object))))))))

(defmethod object->html ((object travel-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (actor object)))
        " travelled "
        (jwhen (origin object)
          (:span " from " (:raw (anchor (origin object)))))
        (jwhen (target object)
          (:span " to " (:raw (anchor (target object))))))))

(defmethod object->html ((object question-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (actor object))) " asks: ")
    (:h2 (name* object))
    (cond
      ((json-true-p (closed object))
       (:p "closed"))
      ((jor (one-of object) (any-of object))
       (dolist (option (jor (one-of object) (any-of object)))
         (:raw (object->html option :card)))))))

;;; Everything else card rendering

;; FIXME: What is a "card view" for a collection? A card with condensed
;; link-like content or a container for cards? It used to be the former, now
;; it's the latter.
(defmethod object->html ((object collection) (format (eql :card)))
  (declare (ignorable format))
  (let* ((items (items object))
         (ordered-items (ordered-items object))
         (items (or items ordered-items))
         (first-item (first-item object))
         (id (nyxt:new-id)))
    (spinneret:with-html-string
      (:div
       :id id
       (cond
         ((json-true-p items)
          (dolist (item items)
            (:raw (object->html item :card))))
         ((collection-page-p first-item)
          (dolist (item (or (items first-item) (ordered-items first-item)))
            (:raw (object->html item :card))))
         ((base-p first-item)
          (loop for item = first-item then (next item)
                collect (:raw (object->html item :card)))))
       (when (and (collection-page-p first-item)
                  (not (equal first-item (last-item object))))
         (:a :class "button" :href (http->ap (url* (next first-item))) "More..."))))))

(defmethod object->html ((object collection-page) (format (eql :page)))
  (declare (ignorable format))
  (spinneret:with-html-string
    (:h1 "Page of " (:raw (anchor (part-of object))))
    (dolist (item (or (items object) (ordered-items object)))
      (:raw (object->html item :card)))
    (jwhen (prev object)
      (:a :class "button" :href (http->ap (url* (prev object))) "Previous page"))
    (jwhen (next object)
      (:a :class "button" :href (http->ap (url* (next object))) "Next page"))))

(defmethod object->html ((object actor) (format (eql :card)))
  (declare (ignorable format))
  (with-card
    (:h2 (:a :href (http->ap (id object))
             (format nil "~a (@~a)" (name* object) (preferred-username object))))
    (jwhen (summary object)
      (:raw (summary object)))))

(defmethod object->html ((object actor) (format (eql :page)))
  (spinneret:with-html-string
    (:h1 (format nil "~a (@~a)" (name* object) (preferred-username object)))
    (jwhen (summary object)
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

;;; The internal scheme definition

(define-internal-scheme "ap"
    (lambda (url buffer)
      (enable-modes '(activitypub-mode) buffer)
      (let ((mode (find-submode 'activitypub-mode buffer)))
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
