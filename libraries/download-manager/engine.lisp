(in-package :download-manager)

(defgeneric cache (type uri))

(defvar notifications nil
  "A channel which can be queried for download notifications.
The channel return value is a `download'.")

(defun init-kernel ()
  (setf lparallel:*kernel* (lparallel:make-kernel 8 :name "next-kernel"))
  (setf notifications (lparallel:make-channel)))

(defun kill-kernel ()
  (lparallel:end-kernel :wait t))

(defclass download ()
  ((requested-uri :accessor requested-uri :initarg :requested-uri
                  :initform ""
                  :documentation "The URI that the user requested.  This may be
different from the actual location of the download, e.g. in case of automatic
redirection.  See RESOLVED-URI.")
   (resolved-uri :accessor resolved-uri :initarg :resolved-uri
                 :initform ""
                 :documentation "The actual source of the download.
This may be different from the URI the user requested, see REQUESTED-URI.")
   (file :accessor file :initarg :file
         :initform ""
         :documentation "The path to the local storage location of the
downloaded file.")
   (downstream :accessor downstream :initarg :downstream
               :initform nil
               :documentation "The stream which can be read from to do the actual
download.")
   (status :accessor status :initarg :status
           ;; TODO: String?
           :initform nil)
   (header :accessor header :initarg :header
           :initform "")
   (update-interval :accessor update-interval :initarg :update-interval
                    :initform 1.0
                    :documentation "Time in floating seconds to wait before
sending a notification to the `notifications' channel.")
   (last-update :accessor last-update :initarg :last-update
                :initform 0.0
                :documentation "Internal time when last notification was sent.
This is a floating seconds.")
   (finished-p :accessor finished-p
               :initform nil
               :documentation "Non-nil if it has finished downloading.")
   (bytes-fetched :accessor bytes-fetched
                  :initform 0)
   (bytes-last-update :accessor bytes-last-update
                      :initform 0
                      :documentation "Bytes fetched when last `update' was called.")
   (last-update-speed :accessor last-update-speed
                      :initform 0
                      :documentation "Download speed in B/s when last `update' was called.")))

(defmethod bytes-total ((download download))
  (gethash "content-length"
           (header download) 0))

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
      (lparallel:submit-task notifications (constantly download))
      (setf (last-update-speed download)
            (if (= 0 time-diff)
                0
                (round (/ (float (- (bytes-fetched download) (bytes-last-update download)))
                          time-diff))))
      (setf (bytes-last-update download) (bytes-fetched download))
      (setf (last-update download) new-time))))

(defun resolve (uri)
  "Resolve and locally cache URI."
  (unless lparallel:*kernel*
    (init-kernel))
  (let ((download (cache :uri uri))
        (channel (lparallel:make-channel)))
    (lparallel:submit-task channel #'download download)
    download))
