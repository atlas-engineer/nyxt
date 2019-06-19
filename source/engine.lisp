(in-package :next/engine)
;; TODO: Move download manager files to a separate directory.

(defgeneric cache (type uri))

(defun init-kernel ()
  (setf lparallel:*kernel* (lparallel:make-kernel 8 :name "next-kernel")))

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
           :initform "")))

(defmethod downloaded-bytes ((download download))
  ;; TODO: If we need more than file size, switch to Osicat library.
  (with-open-file (stream (file download))
    (file-length stream)))

(defmethod total-bytes ((download download))
  (gethash "content-length"
           (header download)))

(defmethod progress ((download download))
  "Return progress ration.
When download is completed, return 1.0."
  (/ (float (downloaded-bytes download))
     (float (total-bytes download))))

(defun resolve (uri)
  "Resolve and locally cache URI."
  (unless lparallel:*kernel*
    (init-kernel))
  (let ((download (cache :uri uri))
        (channel (lparallel:make-channel)))
    (lparallel:submit-task channel #'download download)
    download))
