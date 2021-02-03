;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/style-mode
    (:use :common-lisp :nyxt)
  (:documentation "Mode for styling documents."))

(in-package :nyxt/style-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(define-mode style-mode ()
  "Mode for styling documents."
  ((css-cache-path (make-instance 'css-cache-data-path
                                  :dirname (uiop:xdg-data-home
                                            nyxt::+data-root+
                                            "style-mode-css-cache")))
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
  (ensure-parent-exists (expand-path (css-cache-path mode)))
  (unless (style mode)
    (setf (style mode)
          (or (ignore-errors (uiop:read-file-string
                              (style-file mode)))
              (open-or-cache-url mode (style-url mode)))))
  (apply-style mode))

(defmethod apply-style ((mode style-mode))
  (when (style mode)
    (nyxt::html-set-style (style mode) (buffer mode))))

(defmethod open-or-cache-url ((mode style-mode) url)
  (when (and url
             (or (quri:uri-p url)
                 (valid-url-p url)))
    (let* ((url (if (stringp url)
                    (quri:uri url)
                    url))
           (path (uri-file-path mode url)))
      (ensure-parent-exists (expand-path path))
      (log:info "Loading CSS from ~s." (expand-path path))
      (handler-case (uiop:read-file-string (expand-path path))
        (error ()
          (log:info "Downloading ~s." (expand-path path))
          (let ((file-contents (dex:get (quri:render-uri url))))
            (with-open-file (f (expand-path path) :direction :output :if-exists :overwrite)
              (format f "~a" file-contents))
            file-contents))))))

(defmethod uri-file-path ((mode style-mode) uri)
  (flet ((uri->name (uri)
           (str:replace-all "/" "-" (quri:uri-path uri))))
    (make-instance 'css-cache-data-path
                   :dirname (dirname (css-cache-path mode))
                   :basename (uri->name uri))))

(defmethod nyxt:on-signal-notify-uri ((mode style-mode) url)
  (declare (ignore url))
  (apply-style mode))

(define-mode dark-mode (style-mode)
  "Mode for styling documents."
  ((css-cache-path (make-instance 'css-cache-data-path
                                  :dirname (uiop:xdg-data-home
                                            nyxt::+data-root+
                                            "dark-mode-css-cache")))))

(defmethod apply-style ((mode dark-mode))
  (if (style mode)
      (nyxt::html-set-style (style mode) (buffer mode))
      (nyxt::darken (buffer mode))))
