;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; TODO: Make this a separate library?

(defvar +data-root+ "nyxt")

(defvar *gpg-program* "gpg")

(define-class cache ()
  ((table (make-hash-table :test #'equal)
          :type hash-table
          :documentation "The content of the cache.
Keys are expanded data paths as strings, values are `user-data'.")
   (lock (bt:make-lock)
         :type bt:lock
         :documentation "Protect against concurrent accesses."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod getcache (key (cache cache))
  (gethash key (table cache)))

(defmethod (setf getcache) (value key (cache cache))
  (setf (gethash key (table cache)) value))

(defmacro with-locked-cache (cache &body body)
  `(bt:with-lock-held ((lock ,cache))
     ,@body))

(define-class data-path ()
  ((dirname ""
            :type (or string pathname)
            :documentation "The directory of `basename'.")
   (basename ""
             :type (or string pathname)
             :documentation "The basename of data-path.
It can be appended with an extension if necessary.
When omitted, data-path designates a directory.")
   (ref ""
        :documentation "The reference name of the data-path.
This can be used to set the path from command line.  See
`expand-default-path'.")
   (editable t
             :documentation "If the file can be editted using a text editor.
It's not always the case, take the socket for instance."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defvar *data-paths* (tg:make-weak-hash-table :weakness :key :test 'equal)
  "Set of all `data-path's objects.
It's a weak hash table to that garbage-collected data-paths are automatically
removed, for instance on buffer deletion.")

(defmethod initialize-instance :after ((path data-path) &key)
  (setf (gethash path *data-paths*) path))

(defmethod prompter:object-attributes ((path data-path))
  `(("Path" ,(expand-path path))
    ("Exists?" ,(if (uiop:file-exists-p (uiop:ensure-pathname (expand-path path)))
                    "yes"
                    "no"))
    ("Type" ,(string (sera:class-name-of path)))
    ("Reference" ,(ref path))
    ("Dirname" ,(namestring (dirname path)))
    ("Basename" ,(namestring (basename path)))))

(define-class data-path-source (prompter:source)
  ((prompter:name "User files")
   (prompter:active-attributes-keys '("Path" "Exists?" "Type" "Reference"))
   (prompter:constructor (let ((path-map (make-hash-table :test 'equal)))
                           (dolist (path (alex:hash-table-keys *data-paths*))
                             (sera:and-let* ((data-path-p path)
                                             (editable? (editable path))
                                             (full-path (expand-path path)))
                               (unless (uiop:directory-pathname-p full-path)
                                 (setf (gethash full-path path-map) path))))
                           (alexandria:hash-table-values path-map)))))

(define-class async-data-path (data-path)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class cookies-data-path (data-path)
  ((basename "cookies.txt")
   (ref "cookies"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-class bookmarks-data-path (data-path)
  ((basename "bookmarks")
   (ref "bookmarks"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-class annotations-data-path (data-path)
  ((basename "annotations")
   (ref "annotations"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-class history-data-path (async-data-path)
  ((basename "default")
   (dirname (uiop:xdg-data-home +data-root+ "history"))
   (ref "history"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-class download-data-path (data-path) ; TODO: Rename to downloads-data-path?
  ((dirname (xdg-download-dir))
   (ref "download"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-class auto-mode-rules-data-path (data-path)
  ((basename "auto-mode-rules")
   (ref "auto-mode-rules"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-class standard-output-data-path (data-path)
  ((basename "standard-out.txt")
   (ref "standard-output")
   (editable nil))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-class error-output-data-path (data-path)
  ((basename "standard-error.txt")
   (ref "error-output")
   (editable nil))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-class css-cache-data-path (data-path)
  ((dirname (uiop:xdg-data-home nyxt::+data-root+ "style-mode-css-cache"))
   (ref "mode-css-cache"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always 'xdg-download-dir)
(defun xdg-download-dir ()
  (let ((dir (ignore-errors (uiop:run-program '("xdg-user-dir" "DOWNLOAD")
                                              :output '(:string :stripped t)))))
    (when (or (null dir) (string= dir (uiop:getenv "HOME")))
      (setf dir (uiop:getenv "XDG_DOWNLOAD_DIR")))
    (unless dir
      (setf dir (str:concat (uiop:getenv "HOME") "/Downloads/")))
    (unless (str:ends-with? "/" dir)
      (setf dir (str:concat dir "/")))
    dir))

(define-class data-profile ()
  ((name ""
         :documentation "The name of the profile to refer it with."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class default-data-profile (data-profile)
  ((name :initform "default"))
  (:export-class-name-p t)
  (:documentation "With the default profile all data is persisted to the standard locations."))

(define-class nosave-data-profile (data-profile)
  ((name :initform "nosave")
   (user-data-cache (make-instance 'cache)
                    :type cache
                    :documentation "Buffer-local `user-data-cache' to isolate the data of a nosave buffer."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "With the nosave profile no data should be persisted to disk.
No data should be shared with other buffers either."))

(export-always '*global-data-profile*)
(defvar *global-data-profile* (make-instance 'default-data-profile)
  "The profile to use in the absence of buffers and on browser-less variables.")

(export-always 'current-data-profile)
(defun current-data-profile ()
  "If `%buffer' is non-nil, return its data-profile.
Return `*global-data-profile*' otherwise."
  ;; TODO: %BUFFER is not defined yet. Move %BUFFER there?
  ;; `current-data-profile' may be called during startup when no window exists,
  ;; which means `current-buffer' neither.  But `current-buffer' calls
  ;; `current-window' which calls `ffi-window-active' and may result in a thread
  ;; dead-lock.  To prevent this, we look for the last active window without
  ;; relying on the renderer.
  (if (and *browser* (slot-value *browser* 'last-active-window))
      (let ((buffer (or (current-buffer (slot-value *browser* 'last-active-window))
                        (make-instance 'user-buffer))))
        (or (and buffer (data-profile buffer))
            *global-data-profile*))
      *global-data-profile*))

(defun package-data-profiles ()
  "Return the list of data profiles as a (DATA-PROFILE-SYM NAME DOCSTRING) tuples."
  (sort (mapcar (lambda (profile)
                  (list profile
                        (getf (mopu:slot-properties profile 'name)
                              :initform)
                        (documentation profile 'type)))
                (delete-if (lambda (sym)
                             (not (ignore-errors
                                   (and (not (eq sym 'data-profile))
                                        (mopu:subclassp sym 'data-profile)))))
                           (package-classes)))
        #'string< :key #'second))

(export-always 'find-data-profile)
(defun find-data-profile (name)
  "Return profile matching NAME.
Return NIL on no match."
  (first (find name (package-data-profiles) :test #'string= :key #'second)))

(-> find-ref-path (string) (or string null))
(defun find-ref-path (ref)
  "Return the value of the REF found in `*options*'s `:with-path'.
Example: when passed command line option --with-path foo=bar,
\(find-ref-path \"foo\") return \"bar\"."
  (second
   (assoc ref
          (loop for (opt value . nil) on *options*
                when (eq opt :with-path)
                  collect value)
          :test #'string=)))

(export-always 'expand-default-path)
(-> expand-default-path (data-path &key (:root string)) (or string null))
(defun expand-default-path (path &key (root (namestring (if (str:emptyp (namestring (dirname path)))
                                                            (uiop:xdg-data-home +data-root+)
                                                            (dirname path)))))
  "Derive file from command line option or PATH.
If PATH `ref' is specified in the `:with-path' command line option, use it in
place of PATH `basename'.
- When PATH has no basename, return ROOT.  This is useful to refer to
  directories.
- If basename has a slash, return basename.
- Otherwise expand to 'ROOT/basename.lisp' or 'ROOT/basename' if the basename
  already contains a period."
  (let ((name (or (find-ref-path (ref path))
                  (namestring (basename path)))))
    (cond
      ((uiop:emptyp name)
       (namestring (uiop:ensure-directory-pathname root)))
      ((search "/" name)
       name)
      (t
       (let ((fullname (str:concat root "/" name)))
         (unless (search "." name)
           (setf fullname (str:concat fullname ".lisp")))
         fullname)))))

(export-always 'expand-data-path)
(defmethod expand-data-path ((profile data-profile) (path data-path))
  "Return finalized path.
Return NIL when path must not be used.  This makes it possible to use the
function result as a boolean in conditions."
  (expand-default-path path))

(defmethod expand-data-path ((profile nosave-data-profile) (path cookies-data-path))
  "We shouldn't store cookies for `nosave-data-profile'."
  nil)

(defmethod expand-data-path ((profile nosave-data-profile) (path standard-output-data-path))
  "We shouldn't store `*standard-output*' for `nosave-data-profile'."
  nil)

(defmethod expand-data-path ((profile nosave-data-profile) (path error-output-data-path))
  "We shouldn't store `*error-output*' for `nosave-data-profile'."
  nil)

(export-always 'ensure-parent-exists)
(-> ensure-parent-exists (trivial-types:pathname-designator) trivial-types:pathname-designator)
(defun ensure-parent-exists (path)
  "Create parent directories of PATH if they don't exist and return coerced PATH."
  (let ((path (uiop:ensure-pathname path)))
    (ensure-directories-exist (directory-namestring path))
    path))

(export-always 'store)
(defgeneric store (profile path &key &allow-other-keys)
  (:method ((profile data-profile) (path data-path) &key &allow-other-keys)
    nil)
  (:documentation "The generic way to store data to the given path type.
Define a method for your `data-path' type to make it storable."))

(export-always 'restore)
(defgeneric restore (profile path &key &allow-other-keys)
  (:method ((profile data-profile) (path data-path) &key &allow-other-keys)
    nil)
  (:documentation "The generic way to restore data from the given path type.
Define a method for your `data-path' type to make it restorable."))

(defmethod store ((profile nosave-data-profile) (path data-path) &key &allow-other-keys)
  "This method guarantees PATH will not be persisted to disk in NOSAVE-DATA-PROFILE."
  nil)

(defmethod store :around ((profile data-profile) (path async-data-path) &key &allow-other-keys)
  (labels ((worker ()
             (let* ((user-data (get-user-data profile path))
                    (store-ops (drain-channel (channel user-data) (timeout user-data))))
               (when (< 1 (length store-ops))
                 (log:debug "Skipping ~a unnecessary ~a store ops"
                            (1- (length store-ops))
                            path))
               ;; TODO: Lock here? Seems to dead-lock history forward/backward.
               ;; (bt:with-recursive-lock-held ((lock (get-user-data profile path))))
               (call-next-method))
             (worker)))
    (let ((user-data (get-user-data profile path)))
      (unless (channel user-data)
        (setf (channel user-data) (make-channel))
        (let ((thread (run-thread "async-data-path worker"
                        (worker)
                        :name "Nyxt async-data-path worker")))
          (push thread (non-terminating-threads *browser*))))
      ;; We pass `path', but anything would do since the value is ignored.
      (calispel:! (channel user-data) path))))

(export-always 'expand-path)
(-> expand-path ((or null data-path)) (or string null))
(defun expand-path (data-path)
  "Return the expanded path of DATA-PATH or nil if there is none.
`expand-data-path' is dispatched against `data-path' and `current-data-profile'
`data-profile' if there are buffers, `*global-data-profile*' otherwise.
This function can be used on browser-less globals like `*init-file-path*'."
  (when data-path
    (the (values (or string null) &optional)
         (match (expand-data-path (current-data-profile) data-path)
           ("" nil)
           (m (uiop:native-namestring m))))))

(defun parent (data-path)
  (alex:when-let ((path (expand-path data-path)))
    (uiop:pathname-directory-pathname path)))

;; TODO: create subclasses for history, bookmark, auto-mode data to ensure typing?
(define-class user-data ()
  ((data nil
         :type t
         :documentation "The meaningful data to store. Examples: history data, session, bookmarks.")
   (restored-p nil
               :type boolean
               :export nil
               :documentation "Whether the data was `restore'd.")
   (lock (bt:make-recursive-lock)
         :type bt:lock
         :documentation "The lock to guard from race conditions when accessing this data."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "In-memory data wrapper with lock."))

(define-class async-user-data (user-data)
  ((channel nil
            :type (or null calispel:channel)
            :documentation "Channel that can be used to communicate with the
thread persisting the data to disk.")
   (timeout 0.05
            :type (or null real)
            :documentation "Time to wait on channel for other messages before
storing.  A timeout of 0 means that as soon as 1 message is received, the thread
will store the data immediately.  This can waste a store cycle if another
messages is receieve a fraction of a second after that.  Increasing the timeout
allows the thread to capture more batches together.  Obviously a higher timeout
means the store operations are systematically delayed."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "The class to mediate the data keeping."))

(export-always 'get-user-data)
(defgeneric get-user-data (profile path)
  (:documentation "Return the `user-data' object associated to PROFILE and PATH."))

(defgeneric set-user-data (profile path value)
  (:documentation "Set the `user-data' object's `data' to VALUE."))

(defgeneric user-data-type (profile path)
  (:documentation "Return the type symbol of the `user-data'"))

(defmethod user-data-type ((profile data-profile) (path data-path))
  'user-data)

(defmethod user-data-type ((profile data-profile) (path async-data-path))
  'async-user-data)

(defmethod get-user-data ((profile nosave-data-profile) (path data-path))
  "Look up the buffer-local data in case of `nosave-data-profile'."
  (%get-user-data profile path (user-data-cache profile)))

(defmethod set-user-data ((profile nosave-data-profile) (path data-path) value)
  (setf (data (%get-user-data profile path (user-data-cache profile)))
        value))

(export-always 'get-data)               ; TODO: Unexport?
;; TODO: Better name? Isn't it too wide?
(defmethod get-data ((path data-path))
  "Return the data for PATH.
Data is restored with the `restore' method if required.
Second value is NIL if `get-user-data' returned NIL.
Prefer the thread-safe `with-data-access', or the non-thread-safe
`with-data-unsafe' which won't execute the body if the data is NIL."
  (let ((profile (current-data-profile)))
    (alex:if-let ((user-data (get-user-data profile path)))
      (progn
        (bt:with-recursive-lock-held ((lock user-data))
          (unless (restored-p user-data)
            (%set-data path (restore profile path))
            (setf (restored-p user-data) t)))
        (values (data user-data) user-data))
      (values nil nil))))

(defmethod %set-data ((path data-path) value)
  (set-user-data (current-data-profile) path value))

(export-always 'with-data-access)
(defmacro with-data-access ((data-var data-path &key default) &body body)
  "Lock the data for the BODY to avoid race conditions and safely modify it.
Bind the DATA-VAR to the value of the data from DATA-PATH to reuse it.
In case there's no data, bind DATA-VAR to DEFAULT and set data to it.

The body is not executed when `get-user-data' expands to nil.

Unlike `with-data-unsafe', the body is executed even when DATA-VAR is bound to
NIL.

For a faster and modification-unsafe version, see `with-data-unsafe'."
  (alex:with-gensyms (path-name user-data lock)
    `(sera:and-let* ((,path-name ,data-path)
                     (,user-data (get-user-data (current-data-profile) ,path-name))
                     (,lock (lock ,user-data)))
       (bt:with-recursive-lock-held (,lock)
         (let ((,data-var (or (get-data ,path-name) ,default)))
           (unwind-protect (progn ,@body)
             (%set-data ,path-name ,data-var)
             (store (current-data-profile) ,path-name)))))))

(export-always 'with-data-unsafe)
(defmacro with-data-unsafe ((data-var data-path &key default key) &body body)
  "Bind the data to DATA-VAR for a fast non-modifying lookup.
Bind the DATA-VAR to the value of the data from DATA-PATH to reuse it.
In case there's no data, bind DATA-VAR to DEFAULT.

If DATA-VAR is NIL, do nothing.

If KEY is used, bind the result of applying KEY to the data, to
DATA-VAR.

Beware: this is not a thread-safe macro, so data may be altered while
you use this macro! For a modification-safe macro, see `with-data-access'."
  `(sera:and-let* ((,data-var (or (get-data ,data-path)
                                  ,default))
                   (,data-var ,(if key
                                   `(funcall ,key ,data-var)
                                   data-var)))
     ,@body))

(defvar *gpg-default-recipient* nil)

(defstruct gpg-uid
  validity
  user-id)

(defstruct gpg-key
  length
  algorithm                             ; See https://tools.ietf.org/html/rfc4880#page-62 for the meaning of the algorithm ID.
  key-id
  creation-date
  expiry-date
  uids
  fingerprint
  keygrip)

(defun gpg-private-keys ()
  "Return list of private `gpg-key's."
  (let* ((entries (delete ""
                          (ppcre:split "\\bsec"
                                       (with-output-to-string (s)
                                         (uiop:run-program (list *gpg-program* "--list-secret-keys" "--with-colons")
                                                           :output s)))
                          :test #'string=))
         (entries (mapcar (lambda (s) (str:concat "sec" s)) entries))
         (entries (mapcar (lambda (s) (str:split +newline+ s :omit-nulls t)) entries))
         (entries (mapcar (lambda (entry) (mapcar (lambda (s) (str:split ":" s)) entry)) entries)))
    (mapcar (lambda (entry)
              (let ((key (first entry))
                    (uids (remove-if (lambda (e) (not (string= "uid" (first e)))) entry)))
                (make-gpg-key
                 :length (parse-integer (third key) :junk-allowed t)
                 :algorithm (fourth key)
                 :key-id (fifth key)
                 :creation-date (ignore-errors (local-time:unix-to-timestamp (parse-integer (sixth key))))
                 :expiry-date (ignore-errors (local-time:unix-to-timestamp (parse-integer (seventh key))))
                 :uids (mapcar (lambda (uid-entry)
                                 (make-gpg-uid
                                  :validity (second uid-entry)
                                  :user-id (nth 9 uid-entry)))
                               uids)
                 :fingerprint (nth 9 (assoc "fpr" entry :test #'string=))
                 :keygrip (nth 9 (assoc "grp" entry :test #'string=)))))
            entries)))

(define-class gpg-key-source (prompter:source)
  ((prompter:name "GPG Private Keys")
   (prompter:constructor (gpg-private-keys))))

(defmethod prompter:object-attributes ((gpg-key gpg-key))
  `(("ID" ,(gpg-key-key-id gpg-key))
    ("Additional" ,(str:join ", " (mapcar #'gpg-uid-user-id (gpg-key-uids gpg-key))))))

(defun gpg-recipient (file)             ; TODO: Find a proper way to do this.
  "Return the key of FILE's recipient if any, `*gpg-recipient*' otherwise.
As second value the email.
As third value the name."
  (let ((file (uiop:native-namestring file)))
    (if (uiop:file-exists-p file)
        (handler-case
            (let* ((output (sera:lines (with-output-to-string (s)
                                         (uiop:run-program (list *gpg-program* "--decrypt" file)
                                                           :output nil :error-output s))))
                   (first-line-tokens (str:split " " (first output)))
                   (key (let ((key-string (second (member "ID" first-line-tokens :test #'string=))))
                          (if (str:ends-with? "," key-string)
                              (subseq key-string 0 (1- (length key-string)))
                              key-string)))
                   (second-line (str:trim (second output)))
                   (mail-start (position #\space second-line :from-end t))
                   (mail (str:trim (reduce (lambda (target rep) (str:replace-all rep "" target))
                                           '(">" "<" "\"") :initial-value (subseq second-line mail-start))))
                   (name (str:replace-all "\"" "" (subseq second-line 0 mail-start))))
              (values key mail name))
          (error ()
            *gpg-default-recipient*))
        *gpg-default-recipient*)))

(defun gpg-write (stream gpg-file &optional recipient)
  "Write STREAM to GPG-FILE using RECIPIENT key.
If RECIPIENT is not provided, use default key."
  (let ((native-file (uiop:native-namestring gpg-file)))
    (uiop:run-program
     `(,*gpg-program* "--output" ,native-file
                      ,@(if recipient
                            `("--recipient" ,recipient)
                            '("--default-recipient-self"))
                      "--batch" "--yes" "--encrypt")
     :input stream)))

(defun call-with-gpg-file (gpg-file options fun)
  "Like `call-with-open-file' but use `gpg' to read and write to file.
OPTIONS are as for `open''s `:direction'.
Other options are not supported.  File is overwritten if it exists, while
nothing is done if file is missing."
  ;; TODO: Support all of `open' options.
  (if (member (getf options :direction) '(:io :input nil))
      (when (uiop:file-exists-p gpg-file)
        (let ((clear-data (with-output-to-string (out)
                            (uiop:run-program
                             (list *gpg-program* "--decrypt" gpg-file)
                             :output out))))
          (with-input-from-string (stream clear-data)
            (prog1                ; TODO: Shouldn't we `unwind-protect' instead?
                (funcall fun stream)
              (when (eq (getf options :direction) :io)
                ;; TODO: Need to handle error when gpg-file key is not available.
                (gpg-write stream gpg-file (gpg-recipient gpg-file)))))))
      (let ((result nil)
            (recipient (or (gpg-recipient gpg-file)
                           (and *browser*
                                (ignore-errors
                                 (gpg-key-key-id
                                  (first (prompt
                                          :prompt "Recipient:"
                                          :sources '(gpg-key-source)))))))))
        (with-input-from-string (in (with-output-to-string (stream)
                                      (setf result (funcall fun stream))))
          (gpg-write in gpg-file recipient))
        result)))

(defun call-with-open-file (filespec options fun) ; Inspired by SBCL's `with-open-file'.
  (let ((stream (apply #'open filespec options))
        (abortp t))
    (unwind-protect
         (multiple-value-prog1
             (funcall fun stream)
           (setq abortp nil))
      (when stream
        (close stream :abort abortp)))))

(defun call-with-maybe-gpg-file (filespec options fun)
  "Call FUN over a stream bound to FILESPEC.
See `with-data-file' for OPTIONS."
  (when (eq (getf options :direction) :output) ; TODO: Also :io?
    (let ((option (nth-value 2 (get-properties options '(:if-exists)))))
      (if (null option)
          (alex:appendf options '(:if-exists :supersede))
          ;; TODO: Check at compile time?  Would need to do in `with-data-file' then.
          (check-type (getf option :if-exists)
                      (member :error nil :append :supersede)))))

  (let ((option (nth-value 2 (get-properties options '(:direction)))))
    (when (or (null option)
              (eq (getf option :direction) :input))
      (let ((option (nth-value 2 (get-properties options '(:if-does-not-exist)))))
        (when (null option)
          (alex:appendf options '(:if-does-not-exist nil))))))

  (when (member (getf options :direction) '(:io :output))
    (let ((option (nth-value 2 (get-properties options '(:if-does-not-exist)))))
      (when (null option)
        (alex:appendf options '(:if-does-not-exist :create)))
      (when (or (or (null option)
                    (eq (getf option :if-does-not-exist) :create)))
        (ensure-parent-exists filespec))))

  (flet ((defer-open (filespec options)
           (if (string-equal "gpg" (pathname-type filespec))
               (call-with-gpg-file filespec options fun)
               (call-with-open-file filespec options fun))))

    (let ((exists? (uiop:file-exists-p filespec)))
      (if (and (eq (getf options :direction) :output)
               (or (and exists?
                        (not (member (getf options :if-exists) '(:error nil))))
                   (and (not exists?)
                        (not (member (getf options :if-does-not-exist) '(:error nil))))))

          ;; File to be written to.
          (let* ((abs-file (uiop:ensure-pathname filespec))
                 (file-var abs-file))
            (uiop:with-staging-pathname (file-var)
              (when exists?
                (setf (iolib/os:file-permissions file-var)
                      ;; iolib takes native-namestrings.
                      ;; For instance, CCL escapes ".", `native-namestring
                      ;; ensures to unescape it.
                      (iolib/os:file-permissions
                       (uiop:native-namestring abs-file))))
              (when (and (eq (getf options :if-exists) :append)
                         exists?)
                (uiop:copy-file abs-file file-var))
              (let ((output-options (append '(:direction :output :if-exists :append)
                                            (alex:remove-from-plist options :direction :if-exists :if-does-not-exist))))

                (defer-open file-var output-options))))

          ;; File not written to.
          (defer-open filespec options)))))

(export-always 'with-data-file)
(defmacro with-data-file ((stream data-path &rest options) &body body)
  "Evaluate BODY with STREAM bound to DATA-PATH.
DATA-PATH can be a GPG-encrypted file if it ends with a .gpg extension.
See `call-with-gpg-file'.
If DATA-PATH expands to NIL or the empty string, do nothing.

When `:direction' is `:output', all writes go through temporary file (see
`uiop:with-staging-pathname') which gets renamed to the final file.  On error,
the final file remains untouched, or absent if it didn't exist in the first place.

OPTIONS are as for `open', except

- when `:direction' is `:output'
  - `:if-exists' can be `:error', `nil', `:append' or `:supersede' (the default);
  - `:if-does-not-exists' is `:create' by default;

- when `:direction' is `:input'
  - `:if-does-not-exists' is `nil' by default.

Parent directories are created if necessary."
  (alex:with-gensyms (path)
    `(let ((,path (expand-path ,data-path)))
       (when ,path
         (call-with-maybe-gpg-file ,path ',options (lambda (,stream) ,@body))))))
