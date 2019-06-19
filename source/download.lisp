;;; Download manager backend.
(in-package :next/engine)

(defmethod cache ((type (eql :uri)) uri)
  (locally-cache uri))

(defun downloads-directory (&optional args)
  (truename #p"~/Downloads/"))

(defun locally-cache (requested-uri
                      &key
                        (directory (downloads-directory))
                        (buffer-size 16)) ;; small for testing

  (multiple-value-bind (stream status response-headers resolved-uri)
      (dex:get requested-uri :want-stream t :force-binary t :keep-alive nil)
    (let* ((buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
           (file (merge-pathnames directory (extract-filename requested-uri)))
           (bytes-read 0))
      (with-open-file (output file
                              :direction :output
                              :if-exists :supersede ;;; FIXME: rename to NAME-1.TYPE, NAME-2.TYPE
                              :element-type '(unsigned-byte 8))
        (log:info "Downloading~%  ~a~%to~%  '~a'.~%"
                  resolved-uri file)
        (loop :for byte-position = (read-sequence buffer stream)

              :when (plusp byte-position)
                :do (incf bytes-read byte-position)

              :if (plusp byte-position)
                :do (write-sequence buffer output :end byte-position)
              :else :return nil))
      (values file resolved-uri status response-headers
              requested-uri))))

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
