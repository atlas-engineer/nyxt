;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defclass download ()
  ((uri :accessor uri :initarg :uri :documentation "A string
representation of a URL to be shown in the interface.")
   (paragraph :accessor paragraph :initform (make-instance 'user-interface:paragraph))
   (progress :accessor progress :initform (make-instance 'user-interface:progress-bar)))
  (:documentation "This class is used to represent a download within
the *Downloads* buffer. The browser class contains a list of these
download objects: `downloads'."))

(defmethod connect ((download download) buffer)
  "Connect the user-interface objects within the download to the
buffer. This allows the user-interface objects to update their
appearance in the buffer when they are setf'd."
  (user-interface:connect (paragraph download) buffer)
  (user-interface:connect (progress download) buffer))

(define-mode download-mode ()           ; TODO: Move to separate package?
  "Display list of downloads."
  ((style
    (cl-css:css
     '((".download-url"
        :overflow "auto"
        :white-space "nowrap")
       (".download-url a"
        :color "black")
       (".progress-bar-container"
        :height "20px"
        :width "100%")
       (".progress-bar-base"
        :height "100%"
        :background-color "lightgray")
       (".progress-bar-fill"
        :height "100%"
        :background-color "dimgray"))))))

(defun list-downloads ()
  "Display a buffer listing all downloads. We iterate through the
browser's downloads to draw every single download."
  (with-current-html-buffer (buffer "*Downloads*" 'download-mode)
    (markup:markup
     (:style (style buffer))
     (:style (style (make-instance 'download-mode)))
     (:h1 "Downloads")
     (:hr)
     (:body
      (loop for download in (downloads *browser*)
            for uri = (uri download)
            for paragraph = (paragraph download)
            for progress = (progress download)
            do (connect download buffer)
            collect
               (markup:markup
                (:div
                 (:p :class "download-url" (:b "URL: ") (:a :href uri uri))
                 (:span (markup:raw (user-interface:object-string paragraph)))
                 (:div :class "progress-bar-container"
                       (markup:raw (user-interface:object-string progress))))))))))

(define-command download-url ()
  "Download the page or file of the current buffer."
  (download (current-buffer) (url (current-buffer))))

(defun get-downloaded-filenames ()
  "Return the list of downloaded filenames of the current session, as strings."
  (mapcar #'download-manager:filename
          (get-data (download-path (current-buffer)))))

(defun downloaded-files-suggestion-filter ()
  (let ((filenames (get-downloaded-filenames)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) filenames))))

(define-command download-open-file ()
  "Open a downloaded file. This command only works for downloads
started by the :lisp download engine.
See also `open-file'."
  (let ((filename (prompt-minibuffer
                   :input-prompt "Open file"
                   :suggestion-function (downloaded-files-suggestion-filter))))
    (nyxt/file-manager-mode:open-file-function filename)))
