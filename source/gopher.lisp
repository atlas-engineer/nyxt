;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defgeneric gopher-line->html (line)
  (:documentation "Transform a gopher line to a reasonable HTML representation."))

(defgeneric render-gopher-contents (contents)
  (:documentation "Produce a page content string/array given CONTENTS.
Second return value should be the MIME-type of the content."))

(defmethod gopher-line->html ((line cl-gopher:gopher-line))
  (spinneret:with-html-string
    (:pre (cl-gopher:display-string line))))

(defmethod gopher-line->html ((line cl-gopher:info-message))
  (spinneret:with-html-string
    (if (str:emptyp (cl-gopher:display-string line))
        (:br)
        (:pre :style "padding: 0; margin: 0; border-radius: 0;"
              (cl-gopher:display-string line)))))

(defmethod gopher-line->html ((line cl-gopher:submenu))
  (spinneret:with-html-string
    (:a :class "button" :href (cl-gopher:uri-for-gopher-line line)
        (cl-gopher:display-string line))
    (:br)))

(defmethod render-gopher-contents ((contents cl-gopher:text-file-contents))
  (values (str:join +newline+ (cl-gopher:lines contents)) "text/plain"))

(defmethod render-gopher-contents ((contents cl-gopher:submenu-contents))
  (spinneret:with-html-string
    (loop for line in (cl-gopher:lines contents)
          collect (:raw (gopher-line->html line)))))
