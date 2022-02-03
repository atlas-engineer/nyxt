;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/style-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:documentation "Mode for styling documents."))
(in-package :nyxt/style-mode)
(use-nyxt-package-nicknames)

(define-class css-cache-directory (nfiles:data-file nyxt-file)
  ((nfiles:base-path "style-mode-css-cache/")
   (nfiles:name "mode-css-cache"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

;; (defmethod nfiles:resolve ((profile application-profile) (file css-cache-directory))
;;   ;; TODO: Can we be more dynamic and reuse CLOS more?
;;   (serapeum:path-join
;;    (nfiles:base-path file)
;;    (name profile)
;;    (uiop:xdg-data-home (call-next-method))))

(define-mode style-mode ()
  "A mode for styling documents."
  ((css-cache-directory (make-instance 'css-cache-directory))
   (style-url nil
              :type (or null string quri:uri)
              :documentation "Remote CSS file.  If supplied, set `style' to the
content of the URL.  The resource is cached so it needs to be downloaded only
once.  To refresh the cached files, you must manually delete the old copies.")
   (style-file nil
               :type (or null string pathname)
               :documentation "Local CSS file.
If supplied, set `style' to the content of the file.
Otherwise, look for CSS in `style-url'.")
   (style nil
          :type (or null string)
          :documentation "Style as CSS.
If nil, look for CSS in `style-file' or `style-url'.")
   (constructor
    (lambda (mode)
      (initialize mode))))
  :documentation
  "Style can be set by one of the `style', `style-file' or `style-url' slots.")

(defmethod initialize ((mode style-mode))
  (ensure-directories-exist (nfiles:expand (css-cache-directory mode)))
  (unless (style mode)
    (setf (style mode)
          (or (ignore-errors (uiop:read-file-string
                              (style-file mode)))
              (open-or-cache-url mode (style-url mode)))))
  (apply-style mode))

(defmethod apply-style ((mode style-mode))
  (when (style mode)
    (nyxt::html-set-style (style mode) (buffer mode))))

(defmethod open-or-cache-url ((mode style-mode) url) ; TODO: Leverage `nfiles' to make this happen in the background.  See `blocker-mode'.
  (when (and url
             (or (quri:uri-p url)
                 (valid-url-p url)))
    (let* ((url (if (stringp url)
                    (quri:uri url)
                    url))
           (path (url-file-path mode url)))
      (ensure-directories-exist (nfiles:expand path))
      (log:info "Loading CSS from ~s." (nfiles:expand path))
      (handler-case (uiop:read-file-string (nfiles:expand path))
        (error ()
          (log:info "Downloading ~s." (nfiles:expand path))
          (let ((file-contents (dex:get (render-url url))))
            (with-open-file (f (nfiles:expand path) :direction :output :if-exists :supersede)
              (format f "~a" file-contents))
            file-contents))))))

(defmethod url-file-path ((mode style-mode) url)
  (flet ((url->name (url)
           (str:replace-all "/" "-" (quri:uri-path url))))
    (make-instance 'css-cache-data-path
                   :base-path (url->name url))))

(defmethod nyxt:on-signal-load-finished ((mode style-mode) url)
  (declare (ignore url))
  (apply-style mode))

(define-mode dark-mode (style-mode)
  "A mode for styling documents with a dark background. Unlike other modes, to
effectively disable `dark-mode' you must also reload the buffer."
  ((css-cache-directory (make-instance 'css-cache-directory
                                       :base-path "style-mode-css-cache/"))))

(defmethod apply-style ((mode dark-mode))
  (if (style mode)
      (nyxt::html-set-style (style mode) (buffer mode))
      (nyxt/web-mode::darken (buffer mode))))
