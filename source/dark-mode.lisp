;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/dark-mode
    (:use :common-lisp :nyxt)
  (:documentation "Mode for darkening documents."))

(in-package :nyxt/dark-mode)

(defstruct style-association
  (url)
  (predicate)
  (style)
  (style-file))

(define-mode dark-mode ()
  "Mode for darkening documents."
  ((style-associations (list
                        (make-style-association
                         :url "https://example.org"
                         :style (cl-css:css
                                 '((body
                                    :background-color "black"))))))
   (constructor
    (lambda (mode)
      (initialize mode)))))

(defmethod initialize ((mode dark-mode))
  (loop for association in (style-associations mode)
        do ;; Set string URLs to Quri objects in style associations
           (when (typep (style-association-url association) 'string)
             (setf (style-association-url association)
                   (quri:uri (style-association-url association))))
           ;; Load style files into memory
           (when (style-association-style-file association)
             (setf (style-association-style association)
                   (uiop:read-file-string
                    (style-association-style-file association)))))
  (darken-display mode (url (buffer mode))))

(defmethod darken-display ((mode dark-mode) url)
  (let ((style (style-for-url mode url)))
    (if style
        (let ((style (markup:markup (:style style))))
          (ffi-buffer-evaluate-javascript-async
           (buffer mode)
           (ps:ps (ps:chain document body
                            (|insertAdjacentHTML| "afterbegin"
                                                  (ps:lisp style))))))
        (nyxt::darken (buffer mode)))))

(defmethod nyxt:on-signal-notify-uri ((mode dark-mode) url)
  (darken-display mode url))

(defmethod style-for-url ((mode dark-mode) url)
  (loop for association in (style-associations mode)
        when (and (style-association-url association)
                  (equal
                   (quri:uri-domain url)
                   (quri:uri-domain (style-association-url association))))
        return (style-association-style association)
        when (and (style-association-predicate association)
                  (funcall (style-association-predicate association) url))
        return (style-association-style association)))


