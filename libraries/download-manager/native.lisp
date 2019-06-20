;;; Native Common Lisp download manager backend.
(in-package :download-manager)

(defmethod cache ((type (eql :uri)) uri)
  (locally-cache uri))

(defun downloads-directory ()
  (ensure-directories-exist (truename #p"~/Downloads/")))

(defun ensure-unique-file (file)
  "Return FILE if unique or suffix it with a number otherwise."
  (loop with original-name = file
        with suffix = 1
        while (probe-file file)
        do (setf file (format nil  "~a.~a" original-name suffix) )
        do (incf suffix))
  file)

(defun locally-cache (requested-uri
                      &key
                        (directory (downloads-directory)))

  (multiple-value-bind (stream status response-headers resolved-uri)
      (dex:get requested-uri :want-stream t :force-binary t :keep-alive nil)
    (let* ((file (ensure-unique-file
                  (merge-pathnames directory (extract-filename requested-uri)))))
      ;; TODO: Touch file now to ensure uniqueness when actually downloading?
      (make-instance 'download
                     :requested-uri requested-uri
                     :resolved-uri resolved-uri
                     :header response-headers
                     :file file
                     :status status
                     :downstream stream))))

;; TODO: Rename DOWNLOAD method to FETCH?
(defmethod download ((download download)
                     &key (buffer-size 16)) ; Small for testing.
  "Return the number of bytes fetched."
  (let* ((buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
         (bytes-read 0))
    (with-open-file (output (file download)
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (log:info "Downloading~%  ~a~%to~%  '~a'.~%"
                (resolved-uri download) (file download))
      (loop :for byte-position = (read-sequence buffer (downstream download))

            :do (update download)

            :when (plusp byte-position)
              :do (incf bytes-read byte-position)

            :if (plusp byte-position)
              :do (write-sequence buffer output :end byte-position)
            :else :return nil))
    (update download)
    bytes-read))

#|
(defun extract-filename (u
                         &key
                           (verbose *standard-output*)
                           (fallback-uri-string "file:///var/tmp/killroy.cache"))
  "Extract a filename to save the contents of a URI under"
  (let ((s (typecase u
             (string
              u)
             (quri:uri
              (quri::uri-string u))
             (pathname
              (namestring u))
             (t
              (format verbose "Failed to extract a filename from ~a.  Defaulting to ~a"
                      u fallback-uri-string)
              fallback-uri-string))))
    (let ((rightmost-path-separator (position #\/ s :from-end t)))
      (let ((result
             (when rightmost-path-separator
               (subseq s (1+ rightmost-path-separator)))))
        (if (or (null result)
                (not (plusp (length result))))
            "killroy.cache" ;; FIXME
            result)))))
|#

#|
(defun extract-filename (uri)
  "Extract a filename to save the contents of a URI under"
  (let ((s (typecase uri
             (string
              uri)
             (quri:uri
              (quri::uri-string uri)))))
    (let ((rightmost-path (position #\/ s :from-end t)))
      (let ((result
             (when rightmost-path
               (subseq s (1+ rightmost-path)))))
        (if (or (null result)
                (not (plusp (length result))))
            "unparsed" ;; FIXME
            result)))))
|#

(defun extract-filename (uri)
  "Extract a filename to save the contents of a URI under."
    (let ((rightmost-path (position #\/ uri :from-end t)))
      (let ((result
             (when rightmost-path
               (subseq uri (1+ rightmost-path)))))
        (if (or (null result)
                (not (plusp (length result))))
            "killroy.raw" ;; FIXME
            result))))
