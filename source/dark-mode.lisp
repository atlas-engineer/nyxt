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
  (style-file)
  (style-url))

(define-mode dark-mode ()
  "Mode for darkening documents."
  ((css-cache-path (make-instance 'mode-css-cache-data-path
                                  :dirname (uiop:xdg-data-home
                                            nyxt::+data-root+
                                            "dark-mode-css-cache")))
   (style-associations (list
                        (make-style-association
                         :url "https://example.org"
                         :style (cl-css:css
                                 '((body
                                    :background-color "black"))))))
   (constructor
    (lambda (mode)
      (initialize mode)))))

(defmethod initialize ((mode dark-mode))
  (ensure-parent-exists (expand-path (css-cache-path mode)))
  (loop for association in (style-associations mode)
        do ;; Set string URLs to Quri objects in style associations
           (when (typep (style-association-url association) 'string)
             (setf (style-association-url association)
                   (quri:uri (style-association-url association))))
           (when (typep (style-association-style-url association) 'string)
             (setf (style-association-style-url association)
                   (quri:uri (style-association-style-url association))))
           ;; Load style files into memory
           (when (style-association-style-file association)
             (setf (style-association-style association)
                   (uiop:read-file-string
                    (style-association-style-file association))))
           (when (style-association-style-url association)
             (setf (style-association-style association)
                   (open-or-cache-url mode (style-association-style-url association)))))
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

(defmethod open-or-cache-url ((mode dark-mode) url)
  (let ((path (uri-file-path mode url)))
    (ensure-parent-exists (expand-path path))
    (handler-case (uiop:read-file-string (expand-path path))
      (error (c)
        (log:warn "File ~a does not exist in cache, attempting download. ~a" (expand-path path) c)
        (let ((file-contents (dex:get (quri:render-uri url))))
          (with-open-file (f (expand-path path) :direction :output :if-exists :supersede)
            (format f "~s" file-contents))
          file-contents)))))

(defmethod uri-file-path ((mode dark-mode) uri)
  (flet ((uri->name (uri)
           (str:replace-all "/" "-" (quri:uri-path uri))))
    (make-instance 'mode-css-cache-data-path
                   :dirname (dirname (css-cache-path mode))
                   :basename (uri->name uri))))

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


