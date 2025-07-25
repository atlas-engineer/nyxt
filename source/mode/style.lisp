;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/style
  (:documentation "Package for `style-mode', a customizable document styling
facility.
It also hosts the submode `dark-mode'."))
(in-package :nyxt/mode/style)

(define-mode style-mode ()
  "A mode for styling documents.
Style can be set by one of the `style', `style-file' or `style-url' slots."
  ((visible-in-status-p nil)
   (style-file
    nil
    :type (or null string pathname)
    :documentation "Local CSS file.
If supplied, sets `style' to the content of the file.
Otherwise, looks for CSS in `style-url'.")
   (style
    nil
    :type (or null string)
    :documentation "Style as a CSS string.
If nil, look for CSS in `style-file' or `style-url'.")))

(defmethod enable ((mode style-mode) &key)
  (unless (style mode)
    (setf (style mode)
          (ignore-errors
           (uiop:read-file-string
            (style-file mode)))))
  (apply-style mode))

(defmethod apply-style ((mode style-mode))
  (when (style mode)
    (nyxt::html-set-style (style mode) (buffer mode))))

(defmethod nyxt:on-signal-load-finished ((mode style-mode) url title)
  (declare (ignore url title))
  (apply-style mode))

(define-mode dark-mode (style-mode)
  "A `style-mode' for styling documents with a dark background.
Unlike other modes, to effectively disable `dark-mode' you must also reload the
buffer."
  ((visible-in-status-p nil)))

(defmethod apply-style ((mode dark-mode))
  (if (style mode)
      (nyxt::html-set-style (style mode) (buffer mode))
      (nyxt/mode/bookmarklets:darken (buffer mode))))
