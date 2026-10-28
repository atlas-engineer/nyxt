;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/annotate
  (:documentation "Package for `annotate-mode', mode to annotate documents.

The most important piece of functionality is the `annotation' class and its
subclasses: `url-annotation' and `snippet-annotation'.

See the `annotate-mode' for the external user-facing APIs."))
(in-package :nyxt/mode/annotate)

(define-mode annotate-mode ()
  "Annotate document with arbitrary comments.
Annotations are persisted to disk, see the `annotations-file' mode slot.

See `nyxt/mode/annotate' package documentation for implementation details and
internal programming APIs."
  ((visible-in-status-p nil)
   (annotations-file
    (make-instance 'annotations-file)
    :type annotations-file
    :documentation "File where annotations are saved.")))

(define-configuration context-buffer
  ((default-modes (cons 'annotate-mode %slot-value%))))

(defmethod annotations-file ((buffer buffer))
  (annotations-file (find-submode 'annotate-mode buffer)))

(define-class annotations-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"annotations")
   (files:name "annotations"))
  (:export-class-name-p t))

(define-class annotation ()
  ((data
    ""
    :export nil
    :documentation "The annotation data.")
   (tags
    '()
    :type (list-of string))
   (date (time:now)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "An umbrella annotation type.
Should not be instantiated on its own. Instead, use `url-annotation' and
`snippet-annotation'."))

(define-class url-annotation (annotation)
  ((url nil)
   (page-title ""))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Annotation for a page with a certain URL.
Command to create one is `annotate-current-url'."))

(define-class snippet-annotation (url-annotation)
  ((snippet nil :documentation "The snippet of text being annotated."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Annotation in relation to a text on a certain page.
The page is handled by underlying `url-annotation', while the snippet is
extracted by `annotate-highlighted-text' command."))

(-> annotation-url (annotation) (values (maybe quri:uri) &optional))
(defun annotation-url (annotation)
  "Return ANNOTATION's URL as a `quri:uri', or NIL when it has none."
  (let ((url (url annotation)))
    (etypecase url
      (null nil)
      (quri:uri url)
      (string (quri:uri url)))))

(-> annotation-groups (list) (values list &optional))
(defun annotation-groups (annotations)
  "Group ANNOTATIONS by page, newest group first.
Each group is a list of the URL, the page title, and the notes, newest
first. ANNOTATIONS are already newest first."
  (flet ((same-url-p (left right)
           (or (eq left right)
               (and left right (url-equal left right)))))
    (let ((groups '()))
      (dolist (annotation annotations)
        (let ((group (find (annotation-url annotation) groups
                           :key #'first :test #'same-url-p)))
          (if group
              (push annotation (third group))
              (push (list (annotation-url annotation)
                          (page-title annotation)
                          (list annotation))
                    groups))))
      (dolist (group groups)
        (setf (third group) (nreverse (third group)))
        (when (str:emptyp (second group))
          (setf (second group)
                (or (some (lambda (note)
                            (let ((title (page-title note)))
                              (unless (str:emptyp title)
                                title)))
                          (third group))
                    ""))))
      ;; New groups were pushed, so the newest page is last until reversed.
      (nreverse groups))))

(-> annotation-date (annotation) (values string &optional))
(defun annotation-date (annotation)
  "Return ANNOTATION's date as a day, a short month, and a year."
  (time:format-timestring
   nil (date annotation)
   :format '(:day #\space :short-month #\space :year)))

(-> annotation-meta (annotation) (values string &optional))
(defun annotation-meta (annotation)
  "Return the tags and the date, separated by a middle dot."
  (str:join " · "
            (append (remove-if #'str:emptyp (tags annotation))
                    (and (date annotation)
                         (list (annotation-date annotation))))))

(defun annotation-note (annotation)
  "Present ANNOTATION as the comment, the highlighted passage, and its tags and date."
  (let ((text (str:trim (or (data annotation) "")))
        (snippet (if (snippet-annotation-p annotation)
                     (str:trim (or (snippet annotation) ""))
                     ""))
        (meta (annotation-meta annotation)))
    (spinneret:with-html
      (:li
       (unless (str:emptyp text)
         (:p :class "text" text))
       (unless (str:emptyp snippet)
         (:p :class "snippet" snippet))
       (unless (str:emptyp meta)
         (:p :class "meta" meta))))))

(defun annotation-group (group)
  "Present GROUP as the page, then its notes."
  (destructuring-bind (url title notes) group
    (let* ((url-string (and url (render-url url)))
           (heading (if (str:emptyp title) (or url-string "") title))
           (detail (and url-string
                        (not (string= heading url-string))
                        url-string)))
      (spinneret:with-html
        (:section :class "annotation-group"
          (if url-string
              (:a :class "source"
                  :href url-string
                  :title (or detail heading)
                  (:span :class "heading" heading)
                  (:span :class "url" detail))
              (:div :class "source"
                    (:span :class "heading" heading)
                    (:span :class "url" detail)))
          (:ul :class "notes"
            (dolist (annotation notes)
              (annotation-note annotation))))))))

(defun annotation-add (annotation)
  (files:with-file-content (annotations (annotations-file (current-buffer)))
    (push annotation annotations)))

(defun annotations ()
  (files:content (annotations-file (current-buffer))))

(define-command annotate-current-url
    (&key (buffer (current-buffer))
     (data (prompt1 :prompt "Annotation"
                    :sources (make-instance 'prompter:raw-source
                                            :name "Note")))
     (tags (prompt
            :prompt "Tag(s)"
            :sources (list (make-instance 'prompter:word-source
                                          :name "New tags"
                                          :enable-marks-p t)
                           (make-instance 'keyword-source :buffer buffer)
                           (make-instance 'annotation-tag-source)))))
  "Create an annotation of the URL of BUFFER.

DATA and TAGS are passed as arguments to `url-annotation' make-instance."
  (annotation-add (make-instance 'url-annotation
                                 :url (url buffer)
                                 :data data
                                 :page-title (title buffer)
                                 :tags tags)))

(define-command annotate-highlighted-text
    (&key (buffer (current-buffer))
     (snippet (ffi-buffer-copy buffer))
     (data (prompt1 :prompt "Annotation"
                    :sources (make-instance 'prompter:raw-source
                                            :name "Note")))
     (tags (prompt
            :prompt "Tag(s)"
            :sources (list (make-instance 'prompter:word-source
                                          :name "New tags"
                                          :enable-marks-p t)
                           (make-instance 'keyword-source :buffer buffer)
                           (make-instance 'annotation-tag-source)))))
  "Create an annotation for the highlighted text of BUFFER.

DATA, SNIPPET, and TAGS are passed as arguments to `snippet-annotation'
make-instance."
  (annotation-add (make-instance 'snippet-annotation
                                 :snippet snippet
                                 :url (url buffer)
                                 :page-title (title buffer)
                                 :data data
                                 :tags tags)))

(defun render-annotations (annotations buffer)
  "Show ANNOTATIONS in BUFFER.
Notes for one page stay together, newest page first. The page is a title
over its address, the same row as a list. Under it, each note is the
comment, then the highlighted passage, then the tags and the date."
  ;; 17px inside a page, 37px between pages. The page row is 55px:
  ;; 7 + 24 + 17 + 7. 7px keeps a passage and the tags with the comment.
  (let ((groups (annotation-groups annotations)))
    (spinneret:with-html-string
      (:nstyle
        `(".source"
          :display "block"
          :box-sizing "border-box"
          :height "55px"
          :margin "0 -17px 17px"
          :padding "7px 17px"
          :border-radius "7px"
          :text-decoration "none"
          :color ,theme:on-background-color)
        `("a.source:hover"
          :background-color ,theme:background-color-
          :opacity "1")
        `("a.source:focus-visible"
          :outline "2px solid"
          :outline-color ,theme:action-color
          :outline-offset "2px"
          :opacity "1")
        '(".source span"
          :display "block"
          :overflow "hidden"
          :text-overflow "ellipsis"
          :white-space "nowrap")
        '(".source span.heading"
          :height "24px"
          :font-size "17px"
          :font-weight "600"
          :line-height "24px")
        `(".source span.url"
          :height "17px"
          :font-size "11px"
          :font-weight "400"
          :line-height "17px"
          :color ,theme:primary-color)
        '("ul.notes"
          :list-style "none"
          :margin "0"
          :padding "0")
        '("ul.notes > li"
          :position "relative"
          :margin "0"
          :padding "17px 0")
        '("ul.notes > li:first-child"
          :padding-top "0")
        '("ul.notes > li:last-child"
          :padding-bottom "0")
        `("ul.notes > li:not(:last-child)::after"
          :content ""
          :position "absolute"
          :right "0"
          :bottom "0"
          :left "0"
          :height "1px"
          :background-color ,theme:background-color-)
        '("ul.notes .text"
          :margin "0"
          :font-size "17px"
          :font-weight "400"
          :line-height "24px"
          :overflow-wrap "anywhere")
        `("ul.notes .snippet"
          :margin "7px 0 0"
          :font-size "17px"
          :font-weight "400"
          :line-height "24px"
          :overflow-wrap "anywhere"
          :color ,theme:primary-color)
        `("ul.notes .meta"
          :margin "7px 0 0"
          :font-size "11px"
          :font-weight "400"
          :line-height "17px"
          :color ,theme:primary-color)
        '("ul.notes > li > :first-child"
          :margin-top "0")
        '("h1.grouped"
          :margin-bottom "0")
        '(".annotation-groups"
          :padding-top "17px")
        '(".annotation-group"
          :margin-top "37px")
        '(".annotation-group:first-of-type"
          :margin-top "0")
        `("p.empty"
          :margin "0"
          :font-size "17px"
          :line-height "24px"
          :color ,theme:primary-color))
      (render-menu 'annotate-mode buffer)
      (cond
        (groups
         (:h1 :class "grouped" "Annotations")
         (:div :class "annotation-groups"
           (dolist (group groups)
             (annotation-group group))))
        (t
         (:h1 "Annotations")
         (:p :class "empty" "No annotations."))))))

(define-internal-page show-annotations-for-current-url
    (&key (id (id (current-buffer))))
    (:title "*Annotations*")
  "Display the annotations associated to buffer with ID."
  (let ((buffer (nyxt::buffer-get id)))
    (render-annotations (sera:filter (curry #'url-equal (url buffer))
                                     (files:content (annotations-file buffer))
                                     :key (compose #'quri:uri #'url))
                        %buffer%)))

(define-command-global show-annotations-for-current-url
    (&key (buffer (current-buffer)))
  "Create a new buffer with the annotations of the current URL of BUFFER."
  (buffer-load-internal-page-focus 'show-annotations-for-current-url
                                   :id (id buffer)))

(define-class annotation-source (prompter:source)
  ((prompter:name "Annotations")
   (prompter:constructor (files:content (annotations-file (current-buffer))))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:enable-marks-p t)))

(defmethod prompter:object-attributes ((annotation annotation)
                                       (source prompter:source))
  (declare (ignore source))
  `(("Data" ,(data annotation) (:width 3))
    ("Tags" ,(tags annotation) (:width 3))))

(define-class annotation-tag-source (prompter:source)
  ((prompter:name "Tags")
   (prompter:filter-preprocessor
    (lambda (initial-suggestions-copy source input)
      (prompter:delete-inexact-matches
       initial-suggestions-copy
       source
       (last-word input))))
   (prompter:filter
    (lambda (suggestion source input)
      (prompter:fuzzy-match suggestion source (last-word input))))
   (prompter:enable-marks-p t)
   (prompter:constructor
    (let ((annotations (files:content (annotations-file (current-buffer)))))
      (sort (remove-duplicates
             (mappend #'tags annotations)
             :test #'string-equal)
            #'string-lessp)))))

(define-internal-page-command-global show-annotation ()
    (buffer "*Annotations*")
  "Show prompted annotations."
  (handler-case (render-annotations
                 (prompt :prompt "Show annotation(s)"
                         :sources (make-instance 'annotation-source))
                 buffer)
    (nyxt:prompt-buffer-canceled () (render-annotations nil buffer))))

(define-internal-page-command-global show-annotations ()
    (buffer "*Annotations*")
  "Show all annotations"
  (render-annotations (files:content (annotations-file buffer)) buffer))
