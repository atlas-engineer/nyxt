;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defgeneric gopher-line->html (line parent)
  (:documentation "Transform a gopher line to a reasonable HTML representation."))

(defgeneric render-gopher-contents (contents)
  (:documentation "Produce a page content string given CONTENTS."))

(defmethod gopher-line->html ((line cl-gopher:gopher-line) parent)
  (declare (ignore parent))
  (spinneret:with-html-string
    (:pre (cl-gopher:display-string line))))

(defmethod gopher-line->html ((line cl-gopher:info-message) parent)
  (declare (ignore parent))
  (spinneret:with-html-string
    (if (str:emptyp (cl-gopher:display-string line))
        (:br)
        (:pre (cl-gopher:display-string line)))))

(defmethod gopher-line->html ((line cl-gopher:submenu) parent)
  (let* ((selector (cl-gopher:selector line))
         (url (str:concat "gopher://"
                          (cl-gopher:hostname line)
                          selector
                          (if (str:ends-with-p "/" selector) "" "/")))
         (lines (cl-gopher:lines parent)))
    (spinneret:with-html-string
     (:a :onclick (ps:ps (setf (ps:chain window location href)
                               (ps:lisp url)))
         (cl-gopher:display-string line))
      (:br))))

(defmethod render-gopher-contents ((contents cl-gopher:text-file-contents))
  (values (str:join +newline+ (cl-gopher:lines contents)) "text/plain"))

(defmethod render-gopher-contents ((contents cl-gopher:submenu-contents))
  (spinneret:with-html-string
    (loop for line in (cl-gopher:lines contents)
          collect (:raw (gopher-line->html line contents)))))
