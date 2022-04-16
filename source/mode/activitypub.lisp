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
                (decode-json
                 (dex:get object :headers
                          `(("Accept" . "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
                            ,@(alex:when-let* ((mode (current-mode 'activitypub))
                                               (auth (auth-token mode)))
                                `(("Authorization" ,(str:concat "Bearer " auth))))))))))
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
         (,@(mapcar #'second normalized-slots))
         (:export-class-name-p t)
         (:export-accessor-names-p t)
         (:accessor-name-transformer (class*:make-name-transformer name)))
       ,@(loop for (json-name lisp-name processor) in normalized-slots
               collect `(defmethod ,lisp-name ((object ,name))
                          (or (sera:and-let* ((value (slot-value object (quote ,lisp-name)))
                                              (url-p (valid-url-p value))
                                              (fetched (fetch-object value)))
                                (parse-object fetched))
                              (slot-value object (quote ,lisp-name)))))
       (defmethod fill-object ((object ,name ) processed-json)
         (when (hash-table-p processed-json)
           ,@(loop for (json-name lisp-name processor) in normalized-slots
                   collect `(when (gethash ,json-name processed-json)
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
  )

(define-json-type base-collection "" (object)
  "totalItems"
  ("first" first-item) ; object
  ("last" last-item) ; object
  ("current" current-item) ; object
  )

;; (define-class base-collection (object)
;;   ((total-items 0 :type integer)
;;    (first nil :type (maybe link collection-page))
;;    (last nil :type (maybe link collection-page))
;;    (current nil :type (maybe link collection-page))))

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

(defmethod object->html ((object t))
  (value->html object))

(define-internal-scheme "ap"
    (lambda (url buffer)
      (enable-modes '(activitypub-mode) buffer)
      (alex:if-let ((object (ignore-errors
                             (parse-object
                              (fetch-object
                               (str:concat "https://" (subseq url 3)))))))
        (object->html object)
        (error-help "Not an ActivityPub-enabled page"
                    "This link does not provide ActivityPub representation for content.
Try navigating to a different page.")))
  :local-p t)
