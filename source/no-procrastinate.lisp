;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;;; We don't use CL-prevalence to serialize / deserialize no-procrastinate-hosts for a couple for reasons:
;;; - It's too verbose, e.g. a list is
;;; (:SEQUENCE 3 :CLASS CL:LIST :SIZE 2 :ELEMENTS ( "bar" "baz" ) )
;;;
;;; - We lack control on the linebreaks.
;;;
;;; - It needs IDs for every object, which makes it hard for the user to
;;;   hand-edit the file without breaking it.
;;;
;;; - Un-explicitly-set class slots are exported if they have an initform;
;;;   removing the initform forces us to put lots of (slot-boundp ...).

(define-class no-procrastinate-hosts-file (nfiles:data-file nyxt-lisp-file)
  ((nfiles:base-path #p"no-procrastinate-hosts")
   (nfiles:name "no-procrastinate-hosts"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class no-procrastinate-entry ()
  ((url (quri:uri ""))
   (hostname "")
   (title "")
   (date (local-time:now))
   (tags
    '()
    :type list-of-strings))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((entry no-procrastinate-entry))
  `(("URL" ,(render-url (url entry)))
    ("Hostname" ,(quri:uri-host (url entry)))
    ("Title" ,(title entry))
    ("Tags" ,(format nil "~{~a ~}" (tags entry)))
    ("Date" ,(princ-to-string (date entry)))))

(defmethod equals ((e1 no-procrastinate-entry) (e2 no-procrastinate-entry))
  "Entries are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
  (url-equal (url e1) (url e2)))

(-> no-procrastinate-add
    (quri:uri &key (:title string)
                   (:date (or local-time:timestamp null))
                   (:tags t)
                   (:hostname string))
    t)
(defun no-procrastinate-add  (url &key date title tags hostname)
  (nfiles:with-file-content (no-procrastinate-hosts-list (no-procrastinate-hosts-file (current-buffer)))
    (unless (or (url-empty-p url)
                (string= "about:blank" (render-url url)))
      (multiple-value-bind (entries no-procrastinate-hosts-without-url)
          (sera:partition (sera:partial #'url-equal url) no-procrastinate-hosts-list :key #'url)
        (let ((entry (if entries
                         (first entries)
                         (make-instance 'no-procrastinate-entry
                                        :url url))))
          (unless (str:emptyp title)
            (setf (title entry) title))
          (unless (str:emptyp hostname)
            (setf (hostname entry) hostname))
          (setf tags (delete "" tags :test #'string=))
          (setf tags (delete-duplicates tags :test #'string=))
          (setf (tags entry) (sort tags #'string<))
          (when date
            (setf (date entry) date))
          (push entry no-procrastinate-hosts-without-url)
          (setf no-procrastinate-hosts-list no-procrastinate-hosts-without-url))))))

(define-class no-procrastinate-source (prompter:source)
  ((prompter:name "Hosts to avoid procrastination")
   (prompter:constructor (nfiles:content (no-procrastinate-hosts-file (current-buffer))))
   (prompter:multi-selection-p t)
   (prompter:active-attributes-keys '("URL" "Title" "Tags"))))

(defun url-no-procrastinate-host-tags (url)
  "Return the list of tags of the host corresponding to URL."
  (let ((no-procrastinate-hosts-entry-list (nfiles:content (no-procrastinate-hosts-file (current-buffer)))))
    (alex:when-let ((existing (find url no-procrastinate-hosts-entry-list :key #'url :test #'url-equal)))
      (tags existing))))

(define-command no-procrastinate-current-host (&optional (buffer (current-buffer)))
  "Block procrastination on current hostname of BUFFER's URL."
  (if (url-empty-p (url buffer))
      (echo "Buffer has no URL.")
      (let* ((tags (prompt
                    :prompt (format nil "Tag(s) for ~a " (render-url (url buffer)))
                    :sources (list
                              (make-instance 'prompter:word-source
                                             :name "New tags"
                                             ;; On no input, suggest the empty tag which effectively acts as "no tag".
                                             ;; Without it, we would be force to specify a tag.
                                             :filter-postprocessor
                                             (lambda (suggestions source input)
                                               (declare (ignore source input))
                                               (or suggestions
                                                   (list "")))
                                             :multi-selection-p t)
                              (make-instance 'keyword-source
                                             :buffer buffer)
                              (make-instance 'tag-source
                                             :marks (url-no-procrastinate-host-tags (url buffer))))))
             (homepage-url-string (render-host-and-scheme (url buffer)))
             (homepage-title (plump:text (aref (clss:select "title" (plump:parse (dex:get homepage-url-string)))
                                               0)))
             (homepage-url-object (quri:uri homepage-url-string))
             (hostname (quri:uri-host homepage-url-object)))
        (no-procrastinate-add homepage-url-object
                              :title homepage-title
                              :tags tags
                              :hostname hostname)
        (echo "Associated as a host to avoid procrastination ~a." hostname))))

(define-command no-procrastinate-buffer-host ()
  "Add to the list of hostnames to avoid procrastination the currently opened
page(s) in the active buffer."
  (prompt
   :prompt "Avoid procrastination on HOSTS from buffer(s)"
   :sources (make-instance 'user-buffer-source
                           :multi-selection-p t
                           :actions (list (make-mapped-command no-procrastinate-current-host)))))


(define-command no-procrastinate-host (&key url)
  "Allow the user to avoid procrastination on a HOST via prompt buffer input."
  (let* ((url (or url
                  (ignore-errors
                   (quri:uri
                    (prompt1
                     :prompt "Avoid procrastination the URL's host"
                     :sources (list
                               (make-instance 'prompter:raw-source
                                              :name "New URL")))))))
         (homepage-url-string (render-host-and-scheme url))
         (homepage-title (plump:text (aref (clss:select "title" (plump:parse (dex:get homepage-url-string)))
                                           0)))
         (homepage-url-object (quri:uri homepage-url-string))
         (hostname (quri:uri-host homepage-url-object)))
    (if (not (valid-url-p url))
        (echo "Invalid URL '~a'" url)
        (let* ((url (quri:uri url))
               (tags (prompt
                      :prompt "Tag(s)"
                      :sources (list
                                (make-instance 'prompter:word-source
                                               :name "New tags"
                                               :multi-selection-p t)
                                (make-instance 'tag-source
                                               :marks (url-no-procrastinate-host-tags url))))))
          (no-procrastinate-add homepage-url-object
                                :title homepage-title
                                :hostname hostname
                                :tags tags)))))

(define-command delete-no-procrastinate-host (&optional urls-or-no-procrastinate-host-entries)
  "Delete host(s) matching URLS-OR-NO-PROCRASTINATE-HOST-ENTRIES
URLS is either a list or a single element."
  (if urls-or-no-procrastinate-host-entries
      (nfiles:with-file-content (no-procrastinate-hosts (no-procrastinate-hosts-file (current-buffer)))
        (setf no-procrastinate-hosts
              (set-difference
               no-procrastinate-hosts
               (mapcar (lambda (url)
                         (if (no-procrastinate-entry-p url)
                             url
                             (make-instance 'no-procrastinate-entry :url (quri:uri url))))
                       (uiop:ensure-list urls-or-no-procrastinate-host-entries))
               :test #'equals)))
      (let ((entries (prompt
                      :prompt "Delete host(s)"
                      ;; :default-modes '(minibuffer-tag-mode minibuffer-mode)
                      :sources (make-instance 'no-procrastinate-source
                                              :multi-selection-p t))))
        (delete-no-procrastinate-host entries))))

(defmethod serialize-object ((entry no-procrastinate-entry) stream)
  (unless (url-empty-p (url entry))
    (flet ((write-slot (slot)
             (let ((entry-slot (funcall slot entry)))
               (unless (str:emptyp entry-slot)
                 (format t " :~a ~s"
                         (str:downcase slot)
                         entry-slot)))))
      (let ((*standard-output* stream))
        (write-string "(:url ")
        (format t "~s" (render-url (url entry)))
        (write-slot 'title)
        (write-slot 'hostname)
        (when (date entry)
          (write-string " :date ")
          ;; If we don't force the timezone, the timestamp could be serialized
          ;; differently depending on the local timezone, e.g.
          ;;     2020-12-10T11:46:02.500515+01:00
          ;; instead of
          ;;     2020-12-10T10:46:02.500515Z
          (format t "~s" (local-time:format-timestring nil (date entry)
                                                       :timezone local-time:+utc-zone+)))
        (when (tags entry)
          (write-string " :tags (")
          (format t "~s" (first (tags entry)))
          (dolist (tag (rest (tags entry)))
            (write-string " ")
            (write tag))
          (write-string ")"))
        (write-string ")")))))

(defmethod nfiles:serialize ((profile nyxt-profile) (file no-procrastinate-hosts-file) stream &key)
  (let ((content
          (nfiles:content file)))
    (write-string "(" stream)
    (dolist (entry content)
      (write-string +newline+ stream)
      (serialize-object entry stream))
    (format stream "~%)~%")
    (echo "Saved ~a host to avoid procrastination to ~s."
          (length content)
          (nfiles:expand file))))

(defmethod nfiles:deserialize ((profile nyxt-profile) (path no-procrastinate-hosts-file) raw-content &key)
  (let ((entries (read raw-content)))
    (mapcar (lambda (entry)
              (when (getf entry :url)
                (setf (getf entry :url)
                      (quri:uri (getf entry :url))))
              (when (getf entry :date)
                (setf (getf entry :date)
                      (local-time:parse-timestring (getf entry :date))))
              (apply #'make-instance 'no-procrastinate-entry
                     entry))
            entries)))

