;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/activitypub-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:->)
  (:documentation "Mode for ActivityPub browsing."))
(in-package :nyxt/activitypub-mode)
(use-nyxt-package-nicknames)

;; TODO: Compact URLs
;; TODO: Parsing JSON schemes automatically.
;; TODO: Parse @context.

;;; ActivityStreams Core Classes

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

(defvar *classes* (make-hash-table :test 'equalp)
  "A map from ActivityStreams/ActivityPub type name to the Lisp-side class symbol.")

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

(defgeneric parse-object (object)
  (:method ((object t))
    object)
  (:documentation "Parse the object from the provided JSON data.
Possibly recurse to the nested sub-objects."))

(defmethod parse-object ((object hash-table))
  (alex:if-let ((type (gethash "type" object)))
    (fill-object (make-instance (gethash type *classes*)) object)
    object))

(defmethod parse-object ((object sequence))
  (map (serapeum:class-name-of object) #'parse-object object))

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
    (setf (gethash type *classes*) name)
    `(progn
       (define-class ,name (,@superclasses)
         (,@(mapcar #'second normalized-slots))
         (:export-class-name-p t)
         (:export-accessor-names-p t)
         (:accessor-name-transformer (class*:make-name-transformer name)))
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

;; (define-class object (base)
;;   ((name "" :type string)
;;    (name-map nil :type (maybe hash-table))
;;    (attachment '() :type list-of-bases)
;;    (attributed-to '() :type list-of-bases)
;;    (audience '() :type list-of-bases)
;;    (content "" :type string)
;;    (content-map nil :type (maybe hash-table))
;;    (source "" :type string)
;;    (context "" :type string)
;;    (start-time nil :type (maybe local-time:timestamp))
;;    (end-time nil :type (maybe local-time:timestamp))
;;    (published nil :type (maybe local-time:timestamp))
;;    (updated nil :type (maybe local-time:timestamp))
;;    (duration 0 :type integer
;;                :documentation "Approximate duration of the object in seconds.")
;;    (generator "" :type string
;;                  :documentation "Entity that generated the object.")
;;    (icon)
;;    (image)
;;    (location '() :type list-of-bases)
;;    (preview '() :type list-of-bases)
;;    (replies '() :type list-of-bases)
;;    (summary "" :type string
;;                :documentation "HTML summary for the object.")
;;    (summary-map nil :type (maybe hash-table))
;;    (tag '() :type list)
;;    (in-reply-to '() :type list-of-bases)
;;    (url '() :type list-of-bases)
;;    (to '() :type list-of-bases)
;;    (bto '() :type list-of-bases)
;;    (cc '() :type list-of-bases)
;;    (bcc '() :type list-of-bases)
;;    (media-type "text/html" :type string
;;                            :documentation "MIME type of the `content'.")
;;    ;; ActivityPub-specific:
;;    (likes nil :type (maybe collection))
;;    (shares nil :type (maybe collection)))
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (class*:make-name-transformer name))
;;   (:documentation "The Object is the primary base type for the Activity Streams."))

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

;; (define-class link (base)
;;   ((href nil :type (maybe quri:uri))
;;    (rel nil :type list-of-strings)
;;    (media-type "text/html" :type string)
;;    (name "" :type string)
;;    (hreflang "" :type string)
;;    (height 0 :type integer)
;;    (width 0 :type integer)
;;    (preview '() :type list))
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (class*:make-name-transformer name))
;;   (:documentation "A Link describes a qualified, indirect reference to another resource."))

(define-json-type base-activity "" (object)
  "actor" ; nested
  "target" ; nested
  "result" ; nested
  "origin" ; nested
  "instrument" ; nested
  )

;; (define-class base-activity (object)
;;   ((actor nil :type (or object link list))
;;    (target nil :type (or object link list))
;;    (result nil :type (maybe object link))
;;    (origin nil :type (maybe object link))
;;    (instrument nil :type (maybe object link))))

(define-json-type activity "Activity" (base-activity)
  "object" ; nested
  )

(define-json-type intransitive-activity "IntransitiveActivity" (base-activity))

;; (define-class intransitive-activity (base-activity)
;;   ()
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (class*:make-name-transformer name))
;;   (:documentation "IntransitiveActivity objects represent intransitive actions."))

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

;; (define-class actor (object)
;;   (;; ActivityPub-specific. No harm in adding it, though...
;;    (inbox nil :type (maybe ordered-collection))
;;    (outbox nil :type (maybe ordered-collection))
;;    (followind nil :type (maybe collection))
;;    (followers nil :type (maybe collection))
;;    (liked nil :type (maybe collection))
;;    (steams)
;;    (preferred-username "" :type string)
;;    (endpoints))
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (class*:make-name-transformer name))
;;   (:documentation "Actor objects are capable of carrying out an Activity."))

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

;; (define-class collection (base-collection)
;;   ((items '() :type list-of-bases))
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (class*:make-name-transformer name))
;;   (:documentation "Collection objects are a specialization of the base Object that serve as a container for other Objects or Links."))

(define-json-type ordered-collection "OrderedCollection" (collection)
  "ordererdItems" ; nested
  )

;; (define-class ordered-collection (collection)
;;   ((ordered-items '() :type list-of-bases))
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (class*:make-name-transformer name))
;;   (:documentation "The OrderedCollection is a Collection whose items are always ordered."))

(define-json-type collection-page "CollectionPage" (collection)
  "partOf" ; nested
  "next" ; nested
  "prev" ; nested
  )

;; (define-class collection-page (collection)
;;   ((part-of nil :type (maybe collection))
;;    (next nil :type (maybe collection-page))
;;    (prev nil :type (maybe collection-page)))
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (class*:make-name-transformer name))
;;   (:documentation "A subset or \"page\" inside a big Collection."))

(define-json-type ordered-collection-page "OrderedCollectionPage" (collection-page)
  "startIndex")

;; (define-class ordered-collection-page (collection-page)
;;   ((start-index 0 :type (maybe integer)))
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (class*:make-name-transformer name))
;;   (:documentation "The OrderedCollectionPage identifies a page whose items are strictly ordered."))

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

;;; ActivityPub extended classes.
