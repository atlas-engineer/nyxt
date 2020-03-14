(in-package :next)
(annot:enable-annot-syntax)

(defun buffer-history (buffer)
  "Return the buffer history of BUFFER."
  (match (find-submode buffer 'web-mode)
    ((guard m m) (next/web-mode:history m))))

(defun web-buffers ()
  "Return list of web buffers.
I.e. non-special buffers, those with a non-empty URL slot."
  (delete-if (alexandria:compose #'str:emptyp #'url)
             (buffer-list)))

(defun session-data ()
  "Return the data that needs to be serialized.
This data can be used to restore the session later, e.g. when starting a new
instance of Next."
  ;; TODO: What should the session root be?  A hash-table?  A plist?
  ;; For now, it's just a simple list where the first element is the version.
  ;; Maybe not future-proof, but it's unclear that session structure will ever
  ;; change much.
  (list +version+
        (delete-if #'null (mapcar #'buffer-history (web-buffers)))))

(defun store-sexp-session ()
  "Store the current Next session to the browser `session-path'.
Currently we store the list of current URLs of all buffers."
  ;; TODO: Should we persist keymaps, constructors, etc.?  For instance, should
  ;; we restore the proxy value?  It may be wiser to let the user configure
  ;; whitelitss / blacklists instead.  It's also easier
  (with-open-file (file (session-path *browser*)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    ;; We READ the output of serialize-sexp to make it more human-readable.
    (let ((*package* *package*))
      ;; We need to make sure current package is :next so that
      ;; symbols a printed with consistent namespaces.
      (in-package :next)
      (format file
              "~s"
              (with-input-from-string (in (with-output-to-string (out)
                                            (s-serialization:serialize-sexp (session-data) out)))
                (read in))))))

(defun restore-sexp-session ()
  "Restore the current Next session from the browser `session-path'."
  (let ((session-path (session-path *browser*)))
    (handler-case
        (match (with-open-file (file session-path
                                     :direction :input
                                     :if-does-not-exist nil)
                 (when file
                   ;; We need to make sure current package is :next so that
                   ;; symbols a printed with consistent namespaces.
                   (let ((*package* *package*))
                     (in-package :next)
                     (s-serialization:deserialize-sexp
                      file))))
          ((list version buffer-histories)
           (unless (string= version +version+)
             (log:warn "Session version ~s differs from current version ~s" version +version+))
           (setf buffer-histories (delete-if-not #'htree:current buffer-histories))
           (when buffer-histories
             (log:info "Restoring ~a"
                       (mapcar #'object-string (mapcar (alexandria:compose #'htree:data #'htree:current)
                                                       buffer-histories)))
             ;; Delete the old buffers?
             ;; (maphash (lambda (id buffer)
             ;;            (declare (ignore id))
             ;;            (buffer-delete buffer))
             ;;          buffers)
             ;; Make the new ones.
             ;; TODO: Replace the loop with a function `make-buffer-from-history' in web-mode?
             (loop for history in buffer-histories
                for buffer = (make-buffer)
                for mode = (find-submode buffer 'web-mode)
                do (set-url (url (htree:data (htree:current history))) :buffer buffer)
                do (setf (next/web-mode:history mode) history))
             ;; TODO: Switch to the last active buffer.  We probably need to serialize *browser*.
             ;; Or else we could include `access-time' in the buffer class.
             )))
      (error (c)
        (log:warn "Failed to restore session from ~a: ~a" session-path c)))))
