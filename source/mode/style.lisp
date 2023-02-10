;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/style-mode
    (:documentation "Mode for styling documents."))
(in-package :nyxt/style-mode)

(define-class css-cache-directory (files:data-file nyxt-file)
  ((files:base-path #p"style-mode-css-cache/")
   (files:name "mode-css-cache"))
  (:export-class-name-p t))

(define-mode style-mode ()
  "A mode for styling documents.
Style can be set by one of the `style', `style-file' or `style-url' slots."
  ((visible-in-status-p nil)
   (css-cache-directory (make-instance 'css-cache-directory))
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
If nil, look for CSS in `style-file' or `style-url'.")))

(defmethod enable ((mode style-mode) &key)
  (ensure-directories-exist (files:expand (css-cache-directory mode)))
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
      (ensure-directories-exist (files:expand path))
      (log:info "Loading CSS from ~s." (files:expand path))
      (handler-case (uiop:read-file-string (files:expand path))
        (error ()
          (log:info "Downloading ~s." (files:expand path))
          (let ((file-contents (dex:get (render-url url))))
            (with-open-file (f (files:expand path) :direction :output :if-exists :supersede)
              (format f "~a" file-contents))
            file-contents))))))

(defmethod url-file-path ((mode style-mode) url)
  (flet ((url->name (url)
           (str:replace-all "/" "-" (quri:uri-path url))))
    (make-instance 'css-cache-data-path
                   :base-path (uiop:ensure-pathname (url->name url)))))

(defmethod nyxt:on-signal-load-finished ((mode style-mode) url)
  (declare (ignore url))
  (apply-style mode))

(define-mode dark-mode (style-mode)
  "A mode for styling documents with a dark background. Unlike other modes, to
effectively disable `dark-mode' you must also reload the buffer."
  ((visible-in-status-p nil)
   (css-cache-directory (make-instance 'css-cache-directory
                                       :base-path #p"style-mode-css-cache/"))))

(defmethod apply-style ((mode dark-mode))
  (if (style mode)
      (nyxt::html-set-style (style mode) (buffer mode))
      (nyxt/bookmarklets-mode:darken (buffer mode))))
