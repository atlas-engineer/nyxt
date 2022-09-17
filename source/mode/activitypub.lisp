;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/activitypub-mode
    (:shadow #:profile)
  (:local-nicknames
   (:ap :nactivitypub)
   (:j :njson))
  (:documentation "Mode for ActivityPub browsing."))
(in-package :nyxt/activitypub-mode)
(use-nyxt-package-nicknames)

(defmethod j:decode ((from array))
  (j:decode (swank/backend:utf8-to-string from)))

;; TODO: Compact URLs

(define-mode activitypub-mode ()
  "A mode for ActivityPub-enabled pages browsing.

The `model' is usually the object that the current page was generated from. It's
mostly there for introspection purposes.

`object->html' method allows you to override the way ActivityPub entities are rendered."
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
   (account
    nil
    :type (maybe actor)
    :allocation :class
    :documentation "The object for user account as an `actor' object.")))

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
          (account mode) (ap:fetch-object (sera:lret ((url (quri:copy-uri instance-url)))
                                            (setf (quri:uri-path url) (str:concat "/users/" nickname)))
                                          :auth-token (auth-token mode)))))

(defun http->ap (url)
  (or (alex:when-let* ((url url)
                       (valid (valid-url-p url))
                       (url (quri:uri url)))
        (str:concat "ap:" (nyxt::schemeless-url url)))
      url))

;;; Object rendering

(defgeneric object->html (object format)
  (:method ((object ap:base) (format (eql :link)))
    (spinneret:with-html-string
      (:a :class "button"
          :href (http->ap (ap:id object))
          (ap:name* object))))
  (:method ((object ap:base) (format (eql :page)))
    (spinneret:with-html-string
      (:h1 (ap:name* object))
      (:raw (object->html object :card))))
  (:method ((object ap:link) (format (eql :link)))
    (spinneret:with-html-string
      (:a :class "button"
          :href (http->ap (ap:href object))
          (ap:name* object))))
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
    (:a :href (http->ap (ap:url* object)) (ap:name* object))))

(defun render-image-card (object)
  (spinneret:with-html-string
    (:img :src (ap:url* object)
          :alt (ap:name* object))))

(defun render-video-card (object)
  (spinneret:with-html-string
    (:video :src (ap:url* object) :controls t)))

(defun render-audio-card (object)
  (spinneret:with-html-string
    (:audio :src (ap:url* object) :controls t :preload t)))

(defmacro with-card (&body body)
  `(spinneret:with-html-string
    (:div :class "card" ,@body)))

(defmethod object->html ((object ap:image) (format (eql :card)))
  (render-image-card object))

(defmethod object->html ((object ap:video) (format (eql :card)))
  (render-video-card object))

(defmethod object->html ((object ap:audio) (format (eql :card)))
  (render-audio-card object))

(defmethod object->html ((object ap:document) (format (eql :card)))
  (cond
    ((str:starts-with? "image/" (ap:media-type object))
     (render-image-card object))
    ((str:starts-with? "video/" (ap:media-type object))
     (render-video-card object))
    ((str:starts-with? "audio/" (ap:media-type object))
     (render-audio-card object))
    (t (object->html object :link))))

(defmethod object->html ((object ap:tombstone) (format (eql :card)))
  (with-card
    (:h2 (or (ap:name* object) "Ooops..."))
    (:p (ap:former-type object) " used to be there, but it no longer is.")))

(defmethod object->html ((object ap:profile) (format (eql :card)))
  (object->html (ap:describes object) :card))

(defmethod object->html ((object ap:note) (format (eql :card)))
  (with-card
    (:h2 (:raw (anchor (ap:author* object))))
    (:p (:raw (ap:content object)))
    (j:when_ (ap:attachment object)
      (:raw (object->html (ap:attachment object) :card)))))

(defmethod object->html ((object ap:page) (format (eql :card)))
  (spinneret:with-html-string
    (:h2 (:raw (anchor object)))
    (j:when_ (ap:content object)
      (:pre (sera:ellipsize (string-trim sera:whitespace (plump:text (plump:parse (ap:content object)))) 300)))
    (j:when_ (ap:url* object)
      (:raw (anchor object)))))

(defun render-html-page (object)
  (spinneret:with-html-string
    (j:when_ (ap:name* object)
      (:h1 (ap:name* object)))
    (:i "by " (j:when_ (ap:author* object) (:raw (anchor (ap:author* object)))))
    (j:when_ (ap:content object)
      (:p (:raw (ap:content object))))))

(defmethod object->html ((object ap:page) (format (eql :page)))
  (render-html-page object))

(defmethod object->html ((object ap:article) (format (eql :page)))
  (render-html-page object))

;; Activity card rendering

(defmethod object->html ((object ap:activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (ap:actor object))) (format nil " ~(~a~)-ed" (ap:object-type object)))
    (:div (:raw (object->html (ap:object object) :card)))))

(defmethod object->html ((object ap:create-activity) (format (eql :card)))
  (object->html (ap:object object) :card))

(defmethod object->html ((object ap:add-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (ap:actor object)))
        " added " (j:when_ (ap:target object)
                    (:span "into " (:a :href (http->ap (ap:url* (ap:target object)))
                                       (ap:name* (ap:target object))))))
    (:div (:raw (object->html (ap:object object) :card)))))

(defmethod object->html ((object ap:delete-activity) (format (eql :card)))
  (with-card
    (:i (:a :href (http->ap (ap:url* (ap:actor object))) (ap:name* (ap:actor object)))
        " deleted " (j:when_ (ap:origin object)
                      (:span "from " (:raw (anchor (ap:origin object))))))
    (:div (:raw (object->html (ap:object object) :card)))))

(defmethod object->html ((object ap:invite-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (ap:actor object)))
        " invited " (:raw (anchor (ap:object object)))
        (j:when_ (ap:target object)
          (:span " into " (:raw (anchor (ap:target object))))))))

(defmethod object->html ((object ap:offer-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (ap:actor object)))
        " offered " (:raw (anchor (ap:object object)))
        (j:when_ (ap:target object)
          (:span " to " (:raw (anchor (ap:target object))))))))

(defmethod object->html ((object ap:remove-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (ap:actor object)))
        " removed " (:raw (anchor (ap:object object)))
        (j:when_ (ap:origin object)
          (:span " from " (:raw (anchor (ap:origin object))))))))

(defmethod object->html ((object ap:travel-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (ap:actor object)))
        " travelled "
        (j:when_ (ap:origin object)
          (:span " from " (:raw (anchor (ap:origin object)))))
        (j:when_ (ap:target object)
          (:span " to " (:raw (anchor (ap:target object))))))))

(defmethod object->html ((object ap:question-activity) (format (eql :card)))
  (with-card
    (:i (:raw (anchor (ap:actor object))) " asks: ")
    (:h2 (ap:name* object))
    (cond
      ((j:truep (ap:closed object))
       (:p "closed"))
      ((j:or_ (ap:one-of object) (ap:any-of object))
       (dolist (option (j:or_ (ap:one-of object) (ap:any-of object)))
         (:raw (object->html option :card)))))))

;;; Everything else card rendering

;; FIXME: What is a "card view" for a collection? A card with condensed
;; link-like content or a container for cards? It used to be the former, now
;; it's the latter.
(defmethod object->html ((object ap:collection) (format (eql :card)))
  (declare (ignorable format))
  (let* ((items (ap:items object))
         (ordered-items (ap:ordered-items object))
         (items (or items ordered-items))
         (first-item (ap:first-item object))
         (id (nyxt:new-id)))
    (spinneret:with-html-string
      (:div
       :id id
       (cond
         ((j:truep items)
          (dolist (item items)
            (:raw (object->html item :card))))
         ((ap:collection-page-p first-item)
          (dolist (item (j:or_ (ap:items first-item) (ap:ordered-items first-item)))
            (:raw (object->html item :card))))
         ((ap:base-p first-item)
          (loop for item = first-item then (ap:next item)
                collect (:raw (object->html item :card)))))
       (when (and (ap:collection-page-p first-item)
                  (not (equal first-item (ap:last-item object))))
         (:a :class "button" :href (http->ap (ap:url* (ap:next first-item))) "More..."))))))

(defmethod object->html ((object ap:collection-page) (format (eql :page)))
  (declare (ignorable format))
  (spinneret:with-html-string
    (:h1 "Page of " (:raw (anchor (ap:part-of object))))
    (dolist (item (or (ap:items object) (ap:ordered-items object)))
      (:raw (object->html item :card)))
    (j:when_ (ap:prev object)
      (:a :class "button" :href (http->ap (ap:url* (ap:prev object))) "Previous page"))
    (j:when_ (ap:next object)
      (:a :class "button" :href (http->ap (ap:url* (ap:next object))) "Next page"))))

(defmethod object->html ((object ap:actor) (format (eql :card)))
  (declare (ignorable format))
  (with-card
    (:h2 (:a :href (http->ap (ap:id object))
             (format nil "~a (@~a)" (ap:name* object) (ap:preferred-username object))))
    (j:when_ (ap:summary object)
      (:raw (ap:summary object)))))

(defmethod object->html ((object ap:actor) (format (eql :page)))
  (spinneret:with-html-string
    (:h1 (format nil "~a (@~a)" (ap:name* object) (ap:preferred-username object)))
    (j:when_ (ap:summary object)
      (:raw (ap:summary object)))
    (:br)
    (when (and (ap:following object)
               (or (null (ap:total-items (ap:following object)))
                   (not (zerop (ap:total-items (ap:following object))))))
      (:raw (object->html (ap:following object) :link)))
    (when (and (ap:followers object)
               (or (null (ap:total-items (ap:followers object)))
                   (not (zerop (ap:total-items (ap:followers object))))))
      (:raw (object->html (ap:followers object) :link)))
    (when (and (ap:featured object)
               (or (null (ap:total-items (ap:featured object)))
                   (not (zerop (ap:total-items (ap:featured object))))))
      (:raw (object->html (ap:featured object) :link)))
    (when (and (ap:outbox object)
               (or (null (ap:total-items (ap:outbox object)))
                   (not (zerop (ap:total-items (ap:outbox object))))))
      (:raw (object->html (ap:outbox object) :card)))))

;;; The internal scheme definition

(define-internal-scheme "ap"
    (lambda (url buffer)
      (enable-modes '(activitypub-mode) buffer)
      (let ((mode (find-submode 'activitypub-mode buffer)))
        (values
         (alex:if-let ((object (setf (model mode)
                                     (ignore-errors
                                      (ap:fetch-object (str:concat "https://" (subseq url 3))
                                                       :auth-token (auth-token mode))))))
           (spinneret:with-html-string
             (:head
              (:style (style buffer))
              (:style (style mode)))
             (:body
              (:raw (object->html object :page))))
           nil)
         "text/html;charset=utf8")))
  :local-p t)
