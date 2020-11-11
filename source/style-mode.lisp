;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/style-mode
    (:use :common-lisp :nyxt)
  (:documentation "Mode for styling documents."))

(in-package :nyxt/style-mode)

;;; When creating a style association you have two choices for
;;; matching, URL or predicate. By specifying a URL, the base domain
;;; is checked. If you specify a predicate, your predicate must accept
;;; a single argument (the URL), and return t or nil.
;;;
;;; You have three ways you can supply a style. You can supply it
;;; directly by specifying the style. You can supply it by specifying
;;; a style-file. You can supply it by specifying a style-url. If you
;;; specify a style-url, the resource at the URL will be downloaded
;;; and cached. To refresh the cached files, you must manually delete
;;; the old copies.
(defstruct style-association
  (url)
  (predicate)
  (style)
  (style-file)
  (style-url))

(define-mode style-mode ()
  "Mode for styling documents."
  ((css-cache-path (make-instance 'css-cache-data-path
                                  :dirname (uiop:xdg-data-home
                                            nyxt::+data-root+
                                            "style-mode-css-cache")))
   (style-associations (list
                        (make-style-association
                         :url "https://example.org"
                         :style (cl-css:css
                                 '((body
                                    :background-color "black")))))
                       :documentation "The style-associations list
provides a list of predicates/URL checkers and associated styles. When
a style-association matches, its style will be applied. A
style-association can be specified in multiple ways, please view the
style-association struct for more details.")
   (constructor
    (lambda (mode)
      (initialize mode)))))

(defmethod initialize ((mode style-mode))
  (ensure-parent-exists (expand-path (css-cache-path mode)))
  (dolist (association (style-associations mode))
    ;; Set string URLs to Quri objects in style associations
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
  (style-display mode (url (buffer mode))))

(defmethod style-display ((mode style-mode) url)
  (let ((style (style-for-url mode url)))
    (when style
        (let ((style (markup:markup (:style style))))
          (ffi-buffer-evaluate-javascript-async
           (buffer mode)
           (ps:ps (ps:chain document body
                            (|insertAdjacentHTML| "afterbegin"
                                                  (ps:lisp style)))))))))

(defmethod open-or-cache-url ((mode style-mode) url)
  (let ((path (uri-file-path mode url)))
    (ensure-parent-exists (expand-path path))
    (handler-case (uiop:read-file-string (expand-path path))
      (error ()
        (log:info "Downloading ~s." (expand-path path))
        (let ((file-contents (dex:get (quri:render-uri url))))
          (with-open-file (f (expand-path path) :direction :output :if-exists :supersede)
            (format f "~s" file-contents))
          file-contents)))))

(defmethod uri-file-path ((mode style-mode) uri)
  (flet ((uri->name (uri)
           (str:replace-all "/" "-" (quri:uri-path uri))))
    (make-instance 'css-cache-data-path
                   :dirname (dirname (css-cache-path mode))
                   :basename (uri->name uri))))

(defmethod nyxt:on-signal-notify-uri ((mode style-mode) url)
  (style-display mode url))

(defmethod style-for-url ((mode style-mode) url)
  (loop for association in (style-associations mode)
        when (and (style-association-url association)
                  (equal
                   (quri:uri-domain url)
                   (quri:uri-domain (style-association-url association))))
        return (style-association-style association)
        when (and (style-association-predicate association)
                  (funcall (style-association-predicate association) url))
        return (style-association-style association)))

(define-mode dark-mode (style-mode)
  "Mode for styling documents."
  ((css-cache-path (make-instance 'css-cache-data-path
                                  :dirname (uiop:xdg-data-home
                                            nyxt::+data-root+
                                            "dark-mode-css-cache")))))

(defmethod style-display ((mode dark-mode) url)
  (let ((style (style-for-url mode url)))
    (if style
        (let ((style (markup:markup (:style style))))
          (ffi-buffer-evaluate-javascript-async
           (buffer mode)
           (ps:ps (ps:chain document body
                            (|insertAdjacentHTML| "afterbegin"
                                                  (ps:lisp style))))))
        (nyxt::darken (buffer mode)))))
