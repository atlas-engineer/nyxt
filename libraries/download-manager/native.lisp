;;; Native Common Lisp download manager backend.
(in-package :download-manager)

(defmethod cache ((type (eql :uri)) uri &rest args)
  (log:info uri args)
  (apply #'locally-cache uri args))

(defun locally-cache (requested-uri
                      &key
                        (directory (download-directory))
                        proxy)

  (handler-case
      (multiple-value-bind (stream status response-headers resolved-uri)
          (dex:get requested-uri :want-stream t :force-binary t :keep-alive nil
                                 :proxy proxy)
        (let* ((file (ensure-unique-file
                      ;; TODO: Allow caller to set the target filename?
                      (merge-pathnames directory (extract-filename requested-uri)))))
          ;; TODO: Touch file now to ensure uniqueness when actually downloading?
          (make-instance 'download
                         :requested-uri requested-uri
                         :resolved-uri resolved-uri
                         :header response-headers
                         :file file
                         :status status
                         :downstream stream)))
    (error (c)
      (error c))))

;; TODO: Rename DOWNLOAD method to FETCH?
(defmethod download ((download download)
                     &key (buffer-size 16)) ; Small for testing.
  "Return the number of bytes fetched."
  (let* ((buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (with-open-file (output (file download)
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (log:info "Downloading~%  ~a~%to~%  '~a'.~%"
                (resolved-uri download) (file download))
      (loop :for byte-position = (read-sequence buffer (downstream download))

            :do (update download)

            :when (plusp byte-position)
              :do (incf (bytes-fetched download) byte-position)

            :if (plusp byte-position)
              :do (write-sequence buffer output :end byte-position)
            :else :return nil))
    ;; TODO: Report something if bytes-fetched is not the same as bytes-total.
    (setf (finished-p download) t)
    (update download)
    (bytes-fetched download)))

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
