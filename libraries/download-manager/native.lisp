;;; Native Common Lisp download manager backend.
(in-package :download-manager)

(defmethod cache ((type (eql :uri)) uri &rest args)
  (log:info uri args)
  (apply #'locally-cache uri args))

(defun parse-cookie-jar-string (cookie-jar-string host path)
  "Host is for instance \"example.org\" and path is \"/foo/bar\"."
  (cl-cookie:make-cookie-jar
   :cookies (mapcar (lambda (c)
                      (cl-cookie:parse-set-cookie-header c host path))
                    (cl-ppcre:split " *; *" cookie-jar-string))))

(defun locally-cache (requested-uri
                      &key
                      (directory (download-directory))
                      cookies
                      proxy)
  (let* ((uri (quri:uri requested-uri))
         (cookies-jar
           (unless (str:emptyp cookies)
             (parse-cookie-jar-string cookies (quri:uri-host uri) (quri:uri-path uri)))))
    (handler-case
        (multiple-value-bind (stream status response-headers resolved-uri)
            (dex:get requested-uri :want-stream t :force-binary t :keep-alive nil
                                   :proxy proxy
                                   :cookie-jar cookies-jar)
          ;; TODO: Allow caller to set the target filename?
          (let* ((file (merge-pathnames directory (extract-filename requested-uri
                                                                    response-headers))))
            ;; TODO: Touch file now to ensure uniqueness when actually downloading?
            (make-instance 'download
                           :requested-uri requested-uri
                           :resolved-uri resolved-uri
                           :header response-headers
                           :file file
                           :status status
                           :downstream stream)))
      (error (c)
        (error c)))))

(defmethod fetch ((download download)
                     &key (buffer-size 16)) ; Small for testing.
  "Return the number of bytes fetched."
  (let* ((buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
         (temp-file (temp-file download)))
    (with-open-file (output temp-file
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
    (uiop:rename-file-overwriting-target temp-file
                                         (ensure-unique-file (file download)))
    (update download)
    (bytes-fetched download)))

(defun parse-http-header (header-entry)
  "Return the alist of key-value paris in HEADER-ENTRY."
  (mapcar (lambda (key-value)
            (cl-ppcre:split "=" key-value))
          ;; TODO: Don't split at escaped or quoted semicolons?
          (cl-ppcre:split " *; *" header-entry)))

(defun normalize-filename (filename)
  "Remove surrounding quotes and return the basename as a string.
Return NIL if filename is not a string or a pathname."
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (when (stringp filename)
    (file-namestring (string-trim "\"" filename))))

(defun extract-filename (uri &optional headers)
  "Extract a filename to save the contents of a URI under."
  ;; See https://en.wikipedia.org/wiki/List_of_HTTP_header_fields.
  (or (normalize-filename
       (second (assoc "filename"
                      (parse-http-header
                       (gethash "content-disposition" headers))
                      :test #'string=)))
      (let ((basename (file-namestring uri)))
        (if (string= "" basename)
            "index.html"
            basename))))
