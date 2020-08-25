;;; Native Common Lisp download manager backend.
(in-package :download-manager)

(defmethod cache ((type (eql :uri)) uri &rest args)
  (log:debug uri args)
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
  (let* ((cookies-jar
           (unless (str:emptyp cookies)
             (parse-cookie-jar-string cookies (quri:uri-host requested-uri)
                                      (quri:uri-path requested-uri)))))
    (handler-case
        (multiple-value-bind (stream status response-headers resolved-uri)
            (dex:get (quri:render-uri requested-uri)
                     :want-stream t :force-binary t :keep-alive nil
                     :proxy (and proxy (quri:render-uri proxy)) :cookie-jar cookies-jar)
          ;; TODO: Allow caller to set the target filename?
          (let* ((file (merge-pathnames
                        directory (extract-filename requested-uri
                                                    response-headers))))
            ;; TODO: Touch file now to ensure uniqueness when actually downloading?
            (make-instance 'download
                           :requested-uri requested-uri
                           :resolved-uri (quri:uri resolved-uri)
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
         ;; Without `uiop:parse-native-namestring' `with-open-file' would fail
         ;; if `temp-file' had a wildcard.
         (temp-file (uiop:parse-native-namestring (temp-file download))))
    (with-open-file (output temp-file
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (log:info "Downloading ~s~%  to ~s."
                (or (ignore-errors (quri:url-decode (quri:render-uri (resolved-uri download))))
                    (quri:render-uri (resolved-uri download)))
                (namestring (file download)))
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
      (let ((basename
              (ignore-errors (file-namestring (quri:uri-path uri)))))
        (if (or (null basename) (string= "" basename))
            "index.html"
            basename))))
