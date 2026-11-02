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

(defun edit-glyph ()
  "Return the edit icon used by the bookmark list."
  (or (gethash "edit.svg" *static-data*)
      (setf (gethash "edit.svg" *static-data*)
            (alex:read-file-into-string
             (asdf:system-relative-pathname :nyxt "assets/glyphs/edit.svg")))))

(defun annotation-note (annotation buffer refresh)
  "Present ANNOTATION as the comment, the highlighted passage, its tags and date,
and edit and delete controls."
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
         (:p :class "meta" meta))
       (:div :class "controls"
         (:button :class "control edit"
                  :type "button"
                  :title "Edit"
                  :onclick
                  (ps:ps
                    (nyxt/ps:lisp-eval
                     (:title "edit-annotation"
                      :buffer buffer)
                     (edit-annotation-note annotation)
                     (funcall refresh)))
                  (:raw (edit-glyph)))
         (:button :class "control close"
                  :type "button"
                  :title "Delete"
                  :onclick
                  (ps:ps
                    (nyxt/ps:lisp-eval
                     (:title "delete-annotation"
                      :buffer buffer)
                     (remove-annotation annotation)
                     (funcall refresh)))
                  "×"))))))

(defun annotation-group (group buffer refresh)
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
              (annotation-note annotation buffer refresh))))))))

(defun annotation-add (annotation)
  (files:with-file-content (annotations (annotations-file (current-buffer)))
    (push annotation annotations)))

(defun annotations ()
  (files:content (annotations-file (current-buffer))))

(-> annotation-same-p (annotation annotation) boolean)
(defun annotation-same-p (left right)
  "Return T when LEFT and RIGHT record the same note."
  (and (eq (sera:class-name-of left) (sera:class-name-of right))
       (url-annotation-p left)
       (url-annotation-p right)
       (string= (or (data left) "") (or (data right) ""))
       (equal (tags left) (tags right))
       (let ((left-date (date left))
             (right-date (date right)))
         (or (and (null left-date) (null right-date))
             (and left-date right-date
                  (time:timestamp= left-date right-date))))
       (let ((left-url (annotation-url left))
             (right-url (annotation-url right)))
         (or (eq left-url right-url)
             (and left-url right-url (url-equal left-url right-url))))
       (string= (or (page-title left) "") (or (page-title right) ""))
       (string= (if (snippet-annotation-p left) (or (snippet left) "") "")
                (if (snippet-annotation-p right) (or (snippet right) "") ""))))

(-> stored-annotation (annotation list) (values (maybe annotation) &optional))
(defun stored-annotation (annotation annotations)
  "Return the file copy of ANNOTATION, matching the object, then its contents."
  (or (find annotation annotations :test #'eq)
      (find annotation annotations :test #'annotation-same-p)))

(-> annotation-tag-list (t) (values (list-of string) &optional))
(defun annotation-tag-list (tags)
  "Return TAGS as a list of non-empty strings, without duplicates."
  (remove-if #'str:emptyp
             (delete-duplicates
              (mapcar (lambda (tag)
                        (cond
                          ((stringp tag) tag)
                          ((symbolp tag) (string-downcase (symbol-name tag)))
                          (t (princ-to-string tag))))
                      (uiop:ensure-list tags))
              :test #'string=
              :from-end t)))

(-> save-annotation
    (annotation &key (:data string) (:tags t))
    (values (maybe annotation) &optional))
(defun save-annotation (annotation &key data tags)
  "Store DATA and TAGS on the file copy of ANNOTATION."
  (files:with-file-content (annotations (annotations-file (current-buffer)))
    (when-let ((stored (stored-annotation annotation annotations)))
      (setf (data stored) (or data "")
            (tags stored) (annotation-tag-list tags))
      stored)))

(-> remove-annotation (annotation) (values (maybe annotation) &optional))
(defun remove-annotation (annotation)
  "Remove one ANNOTATION from the annotations file."
  (files:with-file-content (annotations (annotations-file (current-buffer)))
    (when-let ((stored (stored-annotation annotation annotations)))
      (setf annotations (remove stored annotations :count 1 :test #'eq))
      stored)))

(-> annotation-list-page-p (buffer) boolean)
(defun annotation-list-page-p (buffer)
  "Return T when BUFFER is a page that lists annotations without prompting."
  (and (url buffer)
       (member (internal-page-name (url buffer))
               '(show-annotations show-annotations-for-current-url))
       t))

(-> show-annotation-page-p (buffer) boolean)
(defun show-annotation-page-p (buffer)
  "Return T when BUFFER is the prompted annotations page."
  (and (url buffer)
       (eq 'show-annotation (internal-page-name (url buffer)))))

(defun reload-annotation-pages ()
  "Reload every open annotations list."
  (dolist (buffer (buffer-list))
    (when (annotation-list-page-p buffer)
      (ffi-buffer-reload buffer))))

(-> live-notes (list buffer) (values list &optional))
(defun live-notes (notes buffer)
  "Return NOTES still stored for BUFFER, in the same order, after edits."
  (let ((current (copy-list (or (files:content (annotations-file buffer)) '()))))
    (remove nil
            (mapcar (lambda (note)
                      (when-let ((stored (stored-annotation note current)))
                        (setf current
                              (remove stored current :count 1 :test #'eq))
                        stored))
                    notes))))

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
comment, then the highlighted passage, then the tags and the date.
Edit and delete sit on the note, the same controls as the bookmark list."
  ;; 17px inside a page, 37px between pages. The page row is 55px:
  ;; 7 + 24 + 17 + 7. 7px keeps a passage and the tags with the comment.
  (let ((shown annotations))
    (labels ((refresh ()
               (reload-annotation-pages)
               (when (show-annotation-page-p buffer)
                 (setf shown (live-notes shown buffer))
                 (nyxt::html-set (markup shown) buffer)))
             (markup (notes)
               (let ((groups (annotation-groups notes)))
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
                       :min-height "28px"
                       :margin "0 -17px"
                       :padding "17px 97px 17px 17px")
                     '("ul.notes > li:first-child"
                       :padding-top "0")
                     '("ul.notes > li:not(:first-child)"
                       :min-height "45px")
                     '("ul.notes > li:last-child"
                       :padding-bottom "0")
                     `("ul.notes > li:hover"
                       :background-color ,theme:background-color-
                       :border-radius "7px")
                     `("ul.notes > li:not(:last-child)::after"
                       :content ""
                       :position "absolute"
                       :right "17px"
                       :bottom "0"
                       :left "17px"
                       :height "1px"
                       :background-color ,theme:background-color-)
                     '("ul.notes > li:hover::after, ul.notes > li:has(+ li:hover)::after"
                       :background-color "transparent")
                     '("ul.notes .controls"
                       :display "contents")
                     `("ul.notes button.control"
                       :position "absolute"
                       :z-index "1"
                       :top "50%"
                       :right "17px"
                       :transform "translateY(-50%)"
                       :display "flex"
                       :align-items "center"
                       :justify-content "center"
                       :box-sizing "border-box"
                       :width "28px"
                       :height "28px"
                       :margin "0"
                       :padding "0"
                       :border "none"
                       :border-radius "14px"
                       :appearance "none"
                       :background-color "transparent"
                       :color ,theme:primary-color
                       :font-size "17px"
                       :line-height "17px"
                       :cursor "pointer"
                       :opacity "0"
                       :transition "opacity 120ms ease, background-color 120ms ease, color 120ms ease")
                     '("ul.notes button.edit"
                       :right "52px")
                     '("ul.notes button.edit svg"
                       :display "block"
                       :width "17px"
                       :height "17px"
                       :stroke "currentColor")
                     `("ul.notes > li:hover button.control"
                       :opacity "1"
                       :color ,theme:on-background-color)
                     `("ul.notes > li:focus-within button.control"
                       :opacity "1"
                       :color ,theme:on-background-color)
                     `("ul.notes button.control:hover"
                       :opacity "1"
                       :background-color ,theme:background-color
                       :color ,theme:on-background-color)
                     `("ul.notes button.control:focus-visible"
                       :outline "2px solid"
                       :outline-color ,theme:action-color
                       :outline-offset "2px")
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
                          (annotation-group group buffer #'refresh))))
                     (t
                      (:h1 "Annotations")
                      (:p :class "empty" "No annotations.")))))))
      (markup shown))))

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

(-> edit-annotation-note (annotation) (values (maybe annotation) &optional))
(defun edit-annotation-note (annotation)
  "Prompt for a new note and tags for ANNOTATION and store them."
  (let* ((data (prompt1
                :prompt "Annotation"
                :input (or (data annotation) "")
                :sources (make-instance 'prompter:raw-source
                                        :name "Note")))
         (tags (prompt
                :prompt "Tag(s)"
                :sources (list
                          (make-instance 'prompter:word-source
                                         :name "New tags"
                                         :filter-postprocessor
                                         (lambda (suggestions source input)
                                           (declare (ignore source input))
                                           (or suggestions
                                               (list ""))))
                          (make-instance 'keyword-source)
                          (make-instance 'annotation-tag-source
                                         :marks (tags annotation))))))
    (save-annotation annotation :data data :tags tags)))

(define-command edit-annotation (&optional annotations)
  "Edit annotation(s)."
  (let ((annotations (uiop:ensure-list
                      (or annotations
                          (prompt
                           :prompt "Edit annotation(s)"
                           :sources (make-instance 'annotation-source))))))
    (unwind-protect
         (dolist (annotation annotations)
           (edit-annotation-note annotation))
      (reload-annotation-pages))))

(define-command delete-annotation (&optional annotations)
  "Delete annotation(s)."
  (let ((annotations (uiop:ensure-list
                      (or annotations
                          (prompt
                           :prompt "Delete annotation(s)"
                           :sources (make-instance 'annotation-source))))))
    (dolist (annotation annotations)
      (remove-annotation annotation))
    (reload-annotation-pages)))

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
