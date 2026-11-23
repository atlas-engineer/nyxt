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
  (if-let ((mode (find-mode buffer 'annotate-mode)))
    (annotations-file mode)
    (make-instance 'annotations-file)))

(define-class annotations-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"annotations")
   (files:name "annotations")
   (revision
    0
    :type integer
    :documentation "How many times this file's annotations have been saved.
Set this place after a save. A `define-setf-handler' on it redraws
an open annotations list."))
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

(-> annotation-note (annotation buffer) t)
(defun annotation-note (annotation buffer)
  "Return the row for ANNOTATION.
The row is the comment, the highlighted passage, the tags and the date,
and edit and delete."
  (let ((text (str:trim (or (data annotation) "")))
        (snippet (if (snippet-annotation-p annotation)
                     (str:trim (or (snippet annotation) ""))
                     ""))
        (meta (annotation-meta annotation)))
    (make-instance
     'user-interface:element
     :tag :li
     :children
     (append
      (unless (str:emptyp text)
        (list (make-instance 'user-interface:element
                             :tag :p
                             :html-class "text"
                             :text text)))
      (unless (str:emptyp snippet)
        (list (make-instance 'user-interface:element
                             :tag :p
                             :html-class "snippet"
                             :text snippet)))
      (unless (str:emptyp meta)
        (list (make-instance 'user-interface:element
                             :tag :p
                             :html-class "meta"
                             :text meta)))
      (list (make-instance
             'user-interface:element
             :tag :div
             :html-class "controls"
             :children
             (list (make-instance 'user-interface:button
                                  :html-class "control edit"
                                  :alt-text "Edit"
                                  :attributes '(:type "button")
                                  :raw (edit-glyph)
                                  :action (ps:ps
                                            (nyxt/ps:lisp-eval
                                             (:title "edit-annotation"
                                              :buffer buffer)
                                             (edit-annotation-note buffer annotation))))
                   (make-instance 'user-interface:button
                                  :html-class "control close"
                                  :alt-text "Delete"
                                  :text "×"
                                  :attributes '(:type "button")
                                  :action (ps:ps
                                            (nyxt/ps:lisp-eval
                                             (:title "delete-annotation"
                                              :buffer buffer)
                                             (remove-annotation buffer annotation)))))))))))

(-> annotation-section (list buffer) t)
(defun annotation-section (group buffer)
  "Return the section for GROUP: the page, then its notes."
  (destructuring-bind (url title notes) group
    (let* ((title (or title ""))
           (url-string (and url (render-url url)))
           (heading (if (str:emptyp title) (or url-string "") title))
           (detail (and url-string
                        (not (string= heading url-string))
                        url-string))
           (heading-element (make-instance 'user-interface:element
                                           :tag :span
                                           :html-class "heading"
                                           :text heading))
           (url-element (make-instance 'user-interface:element
                                       :tag :span
                                       :html-class "url"
                                       :text (or detail ""))))
      (make-instance
       'user-interface:element
       :tag :section
       :html-class "annotation-group"
       :children
       (list (if url-string
                 (make-instance 'user-interface:element
                                :tag :a
                                :html-class "source"
                                :title (or detail heading)
                                :attributes (list :href url-string)
                                :children (list heading-element url-element))
                 (make-instance 'user-interface:element
                                :tag :div
                                :html-class "source"
                                :children (list heading-element url-element)))
             (make-instance 'user-interface:element
                            :tag :ul
                            :html-class "notes"
                            :children (mapcar (lambda (note)
                                                (annotation-note note buffer))
                                              notes)))))))

(defmacro with-annotations ((annotations buffer) &body body)
  "Bind ANNOTATIONS to BUFFER's annotation list for BODY, then save it.
Saving bumps `revision', which redraws an open annotations list."
  (alex:with-gensyms (file)
    `(let ((,file (annotations-file ,buffer)))
       (prog1
           (files:with-file-content (,annotations ,file)
             ,@body)
         (incf (revision ,file))))))

(defun annotation-add (buffer annotation)
  (with-annotations (annotations buffer)
    (push annotation annotations)))

(defun annotations (buffer)
  (files:content (annotations-file buffer)))

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
    (buffer annotation &key (:data string) (:tags t))
    (values (maybe annotation) &optional))
(defun save-annotation (buffer annotation &key data tags)
  "Store DATA and TAGS on the file copy of ANNOTATION in BUFFER."
  (with-annotations (annotations buffer)
    (when-let ((stored (stored-annotation annotation annotations)))
      (setf (data stored) (or data "")
            (tags stored) (annotation-tag-list tags))
      stored)))

(-> remove-annotation (buffer annotation) (values (maybe annotation) &optional))
(defun remove-annotation (buffer annotation)
  "Remove one ANNOTATION from BUFFER's annotations file."
  (with-annotations (annotations buffer)
    (when-let ((stored (stored-annotation annotation annotations)))
      (setf annotations (remove stored annotations :count 1 :test #'eq))
      stored)))

(-> live-notes (list buffer) (values list &optional))
(defun live-notes (notes buffer)
  "Return NOTES still stored for BUFFER, in the same order, after edits."
  (let ((current (copy-list (files:content (annotations-file buffer)))))
    (remove nil
            (mapcar (lambda (note)
                      (when-let ((stored (stored-annotation note current)))
                        (setf current
                              (remove stored current :count 1 :test #'eq))
                        stored))
                    notes))))

(define-command annotate-current-url
    (buffer &key
     (data (prompt1 :prompt "Annotation"
                    :sources (make-instance 'prompter:raw-source
                                            :name "Note")))
     (tags (prompt
            :prompt "Tag(s)"
            :sources (list (make-instance 'prompter:word-source
                                          :name "New tags"
                                          :enable-marks-p t)
                           (make-instance 'keyword-source :buffer buffer)
                           (make-instance 'annotation-tag-source :buffer buffer)))))
  "Create an annotation of the URL of BUFFER.

DATA and TAGS are passed as arguments to `url-annotation' make-instance."
  (annotation-add buffer
                  (make-instance 'url-annotation
                                 :url (url buffer)
                                 :data data
                                 :page-title (title buffer)
                                 :tags tags)))

(define-command annotate-highlighted-text
    (buffer &key
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
                           (make-instance 'annotation-tag-source :buffer buffer)))))
  "Create an annotation for the highlighted text of BUFFER.

DATA, SNIPPET, and TAGS are passed as arguments to `snippet-annotation'
make-instance."
  (annotation-add buffer
                  (make-instance 'snippet-annotation
                                 :snippet snippet
                                 :url (url buffer)
                                 :page-title (title buffer)
                                 :data data
                                 :tags tags)))

(-> annotation-stamp (annotation) list)
(defun annotation-stamp (annotation)
  "Return the parts of ANNOTATION the list shows."
  (list (or (data annotation) "")
        (when (snippet-annotation-p annotation)
          (or (snippet annotation) ""))
        (and (tags annotation) (copy-list (tags annotation)))
        (and (date annotation) (annotation-date annotation))
        (or (page-title annotation) "")
        (let ((url (annotation-url annotation)))
          (and url (render-url url)))))

(-> annotation-group-of (annotation list) (values (maybe list) &optional))
(defun annotation-group-of (annotation annotations)
  "Return the group of ANNOTATION in ANNOTATIONS."
  (find-if (lambda (group)
             (find annotation (third group) :test #'eq))
           (annotation-groups annotations)))

(define-mode annotation-listing-mode ()
  "Mode for the annotations pages."
  ((listing
    nil
    :export nil
    :documentation "The list shown on the annotations pages.
A `user-interface:dynamic-element'.")
   (filter
    nil
    :type (maybe function)
    :export nil
    :documentation "Function of the annotation list, or nil.
Nil shows every annotation. The function returns the annotations to show."))
  (:toggler-command-p nil))

(-> visible-annotations (annotation-listing-mode) list)
(defun visible-annotations (mode)
  "Return the annotations MODE shows."
  (let ((annotations (or (files:content (annotations-file (buffer mode))) '())))
    (if (filter mode)
        (or (funcall (filter mode) annotations) '())
        annotations)))

(-> annotation-item (annotation annotation-listing-mode) t)
(defun annotation-item (annotation mode)
  "Return the section for ANNOTATION's page."
  (let ((group (annotation-group-of annotation (visible-annotations mode))))
    (when group
      (annotation-section group (buffer mode)))))

(-> annotation-item-key (annotation annotation-listing-mode) t)
(defun annotation-item-key (annotation mode)
  "Return the value that changes when ANNOTATION's page should be drawn again."
  (let ((group (annotation-group-of annotation (visible-annotations mode))))
    (when group
      (mapcar #'annotation-stamp (third group)))))

(-> render-annotations (annotation-listing-mode) t)
(defun render-annotations (mode)
  "Show MODE's annotations.
Notes for one page stay together, newest page first. The list element keeps
a group it has already built."
  (let* ((element (listing mode))
         (groups (annotation-groups (visible-annotations mode))))
    (user-interface:batch (element)
      (cond
        ((null groups)
         (setf (user-interface:items element) '()
               (user-interface:tag element) :p
               (user-interface:html-class element) "empty"
               (user-interface:text element) "No annotations."))
        (t
         (setf (user-interface:items element)
               (mapcar (lambda (group) (first (third group))) groups)
               (user-interface:tag element) :div
               (user-interface:html-class element) "annotation-groups"
               (user-interface:text element) nil))))))

(defmethod enable ((mode annotation-listing-mode) &key)
  "Keep MODE's list in step with the annotations file."
  (let ((buffer (buffer mode)))
    (unless (listing mode)
      (setf (listing mode)
            (make-instance 'user-interface:dynamic-element
                           :tag :p
                           :html-class "empty"
                           :text "No annotations."
                           :template (lambda (annotation)
                                       (annotation-item annotation mode))
                           :item-key (lambda (annotation)
                                       (annotation-item-key annotation mode)))))
    (define-setf-handler annotations-file revision mode
      (lambda (file)
        (let ((page-file (annotations-file buffer)))
          (when (or (eq file page-file)
                    (uiop:pathname-equal (files:expand file)
                                         (files:expand page-file)))
            (render-annotations mode)))))
    (render-annotations mode)))

(defmethod disable ((mode annotation-listing-mode) &key)
  (user-interface:disconnect (listing mode)))

(-> annotation-page (buffer) string)
(defun annotation-page (buffer)
  "Return the annotations page for BUFFER."
  (let ((listing (listing (find-mode buffer 'annotation-listing-mode))))
    (user-interface:connect listing buffer)
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
        '("h1.grouped + p.empty"
          :margin-top "24px")
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
      (:h1 :class "grouped" "Annotations")
      (user-interface:to-html listing))))

(define-internal-page show-annotations-for-current-url (&key id)
    (:title "*Annotations*" :page-mode 'annotation-listing-mode)
  "Display the annotations associated to buffer with ID."
  (let ((source (nyxt::buffer-get id))
        (mode (find-mode %buffer% 'annotation-listing-mode)))
    (setf (filter mode)
          (if source
              (let ((url (url source)))
                (lambda (annotations)
                  (sera:filter (curry #'url-equal url)
                               annotations
                               :key (compose #'quri:uri #'url))))
              (constantly '())))
    (render-annotations mode)
    (annotation-page %buffer%)))

(define-command-global show-annotations-for-current-url (buffer)
  "Create a new buffer with the annotations of the current URL of BUFFER."
  (buffer-load-internal-page-focus 'show-annotations-for-current-url
                                   :id (id buffer)))

(define-class annotation-source (prompter:source)
  ((prompter:name "Annotations")
   (buffer nil :type (maybe buffer))
   (prompter:constructor (lambda (source)
                           (files:content (annotations-file (buffer source)))))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:enable-marks-p t)))

(defmethod prompter:object-attributes ((annotation annotation)
                                       (source prompter:source))
  (declare (ignore source))
  `(("Data" ,(data annotation) (:width 3))
    ("Tags" ,(tags annotation) (:width 3))))

(define-class annotation-tag-source (prompter:source)
  ((prompter:name "Tags")
   (buffer nil :type (maybe buffer))
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
    (lambda (source)
      (let ((annotations (files:content (annotations-file (buffer source)))))
        (sort (remove-duplicates
               (mappend #'tags annotations)
               :test #'string-equal)
              #'string-lessp))))))

(-> edit-annotation-note (buffer annotation) (values (maybe annotation) &optional))
(defun edit-annotation-note (buffer annotation)
  "Prompt for a new note and tags for ANNOTATION in BUFFER and store them."
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
                          (make-instance 'keyword-source :buffer buffer)
                          (make-instance 'annotation-tag-source
                                         :buffer buffer
                                         :marks (tags annotation))))))
    (save-annotation buffer annotation :data data :tags tags)))

(define-command edit-annotation (buffer &optional annotations)
  "Edit annotation(s) in BUFFER."
  (let ((annotations (uiop:ensure-list
                      (or annotations
                          (prompt
                           :prompt "Edit annotation(s)"
                           :sources (make-instance 'annotation-source
                                                   :buffer buffer))))))
    (dolist (annotation annotations)
      (edit-annotation-note buffer annotation))))

(define-command delete-annotation (buffer &optional annotations)
  "Delete annotation(s) in BUFFER."
  (let ((annotations (uiop:ensure-list
                      (or annotations
                          (prompt
                           :prompt "Delete annotation(s)"
                           :sources (make-instance 'annotation-source
                                                   :buffer buffer))))))
    (dolist (annotation annotations)
      (remove-annotation buffer annotation))))

(define-internal-page-command-global show-annotation ()
    (buffer "*Annotations*" 'annotation-listing-mode)
  "Show prompted annotations."
  (let ((mode (find-mode buffer 'annotation-listing-mode)))
    (handler-case
        (let ((selected (prompt :prompt "Show annotation(s)"
                                :sources (make-instance 'annotation-source
                                                        :buffer buffer))))
          (setf (filter mode)
                (lambda (annotations)
                  (declare (ignore annotations))
                  (live-notes selected buffer))))
      (nyxt:prompt-buffer-canceled ()
        (setf (filter mode) (constantly '()))))
    (render-annotations mode)
    (annotation-page buffer)))

(define-internal-page-command-global show-annotations ()
    (buffer "*Annotations*" 'annotation-listing-mode)
  "Show all annotations."
  (annotation-page buffer))
