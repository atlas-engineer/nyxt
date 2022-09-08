;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :download-manager)

(defvar *default-download-directory* #p"~/Downloads/")
(defun default-download-directory ()
  (let ((dir (ignore-errors (uiop:run-program '("xdg-user-dir" "DOWNLOAD")
                                              :output '(:string :stripped t)))))
    (when (or (null dir) (string= dir (uiop:getenv "HOME")))
      (setf dir (uiop:getenv "XDG_DOWNLOAD_DIR")))
    (unless dir
      (setf dir *default-download-directory*))
    dir))

(defun download-directory (&optional (directory (default-download-directory)))
  "Return path to download directory.
Create it if it does not exist."
  (unless directory
    (setf directory (default-download-directory)))
  (unless (string= "" (file-namestring directory))
    (setf directory (format nil "~a/" (namestring directory))))
  (truename (ensure-directories-exist directory)))

(defun ensure-unique-file (file)
  "Return FILE if unique or suffix it with a number otherwise."
  (loop with original-name = file
        with suffix = 1
        while (uiop:file-exists-p file)
        do (setf file (make-pathname :defaults original-name
                                     :name (format nil "~a.~d" (pathname-name (pathname original-name))
                                                   suffix)))
        do (incf suffix))
  (namestring (pathname file)))

(defvar *notifications* nil
  "A channel which can be queried for download notifications.
The channel return value is a `download'.")

(defclass download ()
  ((requested-url
    :accessor requested-url
    :initarg :requested-url
    :initform (quri:uri "")
    :type quri:uri
    :documentation "The URL that the user requested.
This may be different from the actual location of the download, e.g. in case of
automatic redirection.  See RESOLVED-URL.")
   (resolved-url
    :accessor resolved-url
    :initarg :resolved-url
    :initform (quri:uri "")
    :type quri:uri
    :documentation "The actual source of the download.
This may be different from the URL the user requested, see REQUESTED-URL.")
   (file
    :accessor file
    :initarg :file
    :initform ""
    :documentation "Path pointing to the local storage location of the
downloaded file.")
   (downstream
    :accessor downstream
    :initarg :downstream
    :initform nil
    :documentation "The stream which can be read from to do the actual
download.")
   (status
    :accessor status
    :initarg :status
    ;; TODO: String?
    :initform nil)
   (header
    :accessor header
    :initarg :header
    :initform "")
   (update-interval
    :type alexandria:non-negative-real
    :accessor update-interval
    :initarg :update-interval
    :initform 1.0
    :documentation "Time in seconds after which a notification is sent to the
`*notifications*' channel.")
   (last-update
    :type alexandria:non-negative-real
    :accessor last-update
    :initarg :last-update
    :initform 0.0
    :documentation "Time in seconds when the last notification was sent.")
   (finished-p
    :accessor finished-p
    :initform nil
    :documentation "Non-nil if it has finished downloading.")
   (bytes-fetched
    :accessor bytes-fetched
    :initform 0)
   (bytes-last-update
    :accessor bytes-last-update
    :initform 0
    :documentation "Bytes fetched when last `update' was called.")
   (last-update-speed
    :accessor last-update-speed
    :initform 0
    :documentation "Download speed in B/s when last `update' was called.")))

(defmethod filename ((download download))
  "Return the full name of this downloaded file, as a string."
  (format nil "~a" (file download)))

(defmethod temp-file ((download download))
  "Return a file name suitable for unfinished
downloads."
  (ensure-unique-file
   (format nil "~a.part" (namestring (file download)))))

(defmethod bytes-total ((download download))
  (let ((bytes (gethash "content-length"
                        (header download) 0)))
    (if (stringp bytes) (parse-integer bytes) bytes)))

(defmethod progress ((download download))
  "Return progress ratio.
When download is completed, return 1.0.
When progress cannot be computer (because bytes-total is unknown), return
(values 0 'unknown)."
  (cond
    ((finished-p download) 1)
    ((if (> (bytes-total download) 0)
         (/ (float (bytes-fetched download))
            (float (bytes-total download)))
         (values 0 'unknown)))))

(defmethod update ((download download))
  "Send DOWNLOAD to the `notifications' channel.
Only send if last update was more than `update-interval' seconds ago."
  (let* ((new-time (/ (get-internal-real-time) (float internal-time-units-per-second)))
         (time-diff (- new-time (last-update download))))
    (when (or (< (update-interval download) time-diff)
              (finished-p download))
      (calispel:! *notifications* download)
      (setf (last-update-speed download)
            (if (= 0 time-diff)
                0
                (round (/ (float (- (bytes-fetched download) (bytes-last-update download)))
                          time-diff))))
      (setf (bytes-last-update download) (bytes-fetched download))
      (setf (last-update download) new-time))))

(declaim (ftype (function (quri:uri &key (:directory (or string pathname))
                                    (:proxy (or quri:uri null))
                                    (:cookies (or string null))))
                resolve))
(defun resolve (url &key
                      (directory (default-download-directory))
                      proxy
                      cookies)
  "Start downloading URL concurrently and return a corresponding `download' object.
If DIRECTORY is nil, `default-download-directory' will be used.  COOKIES can
specify a cookie jar as a string, which is useful for authenticated downloads.
PROXY is the full proxy address, e.g. \"socks5://127.0.0.1:9050\"."
  (unless *notifications*
    (setf *notifications* (make-instance 'calispel:channel)))
  (let ((download (cache :url url
                         :directory (download-directory directory)
                         :cookies cookies
                         :proxy proxy)))
    ;; TODO: We just use bt:make-thread, no need for a channel... Unless need to
    ;; watch for unfinished downloads and warn the user before closing.
    (bt:make-thread
     (lambda ()
       (fetch download))
     :name "download-manager")
    download))
