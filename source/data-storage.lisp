(in-package :next)

;; TODO: Make this a separate library?

(defvar +data-root+ "next")

(defvar *gpg-program* "gpg")

(defclass-export data-profile ()
  ((name :initarg :name
         :accessor name
         :type string
         :initform ""
         :documentation "The name of the profile to refer it with.")))

(export-always '+default-data-profile+)
(defvar +default-data-profile+ (make-instance 'data-profile :name "default")
  "With the default profile all data is persisted to the standard locations.")

(export-always '+private-data-profile+)
(defvar +private-data-profile+ (make-instance 'data-profile :name "private")
  "With the private profile no data should be persisted to disk.")

(defun package-data-profiles ()
  "Return the list of data profiles a (DATA-PROFILE NAME DOCSTRING) tuples."
  (sort (mapcar (lambda (profile)
                  (list (symbol-value profile)
                        (name (symbol-value profile))
                        (documentation profile 'variable)))
                (delete-if (lambda (sym)
                             (not (ignore-errors
                                   (mopu:subclassp (class-of (symbol-value sym))
                                                   'data-profile))))
                           (package-variables)))
        #'string< :key #'second))

(defun find-profile (name)
  "Return profile matching NAME.
Return NIL on no match."
  (first (find name (package-data-profiles) :test #'string= :key #'second)))

(defclass-export data-path ()
  ((dirname :initarg :dirname
            :accessor dirname
            :type string
            :initform (uiop:xdg-data-home +data-root+)
            :documentation "The directory of `basename'.")
   (basename :initarg :basename
             :accessor basename
             :type string
             :initform ""
             :documentation "The basename of data-path.
It can be appended with an extension if necessary.
When omitted, data-path designates a directory.")
   (ref :initarg :ref
        :accessor ref
        :type string
        :initform ""
        :documentation "The reference name of the data-path.
This can be used to set the path from command line.  See
`expand-default-path'.")))

(defclass-export session-data-path (data-path)
  ((ref :initform "session")))
(defclass-export cookies-data-path (data-path)
  ((ref :initform "cookies")))
(defclass-export bookmarks-data-path (data-path)
  ((ref :initform "bookmarks")))
(defclass-export history-data-path (data-path)
  ((ref :initform "history")))
(defclass-export download-data-path (data-path) ; TODO: Rename to downloads-data-path?
  ((ref :initform "download")))

(defun find-ref-path (ref)
  (second
   (assoc ref
          (loop for (opt value . _) on *options*
                when (eq opt :with-path)
                  collect value)
          :test #'string=)))

(declaim (ftype (function (data-path) (or string null)) expand-default-path))
(defun expand-default-path (path)
  "Derive file from command line option or PATH.
If PATH `ref' is specified in the `:with-path' command line option, use it in
place of PATH `basename'.
- When PATH has no basename, return its directory.  This is useful to refer to
  directories.
- If basename has a slash, return basename.
- Otherwise expand to 'directory/basname.lisp' or 'directory/basname.lisp' is
  basename already contains a period."
  (let ((name (match (find-ref-path (ref path))
                (nil (basename path))
                (m m))))
    (cond
      ((uiop:emptyp name)
       (namestring (dirname path)))
      ((search "/" name)
       (namestring name))
      (t
       (let ((fullname (str:concat (namestring (dirname path)) "/" (namestring name))))
         (unless (search "." name)
           (setf fullname (str:concat fullname ".lisp")))
         fullname)))))

(defmethod expand-data-path ((path data-path) (profile data-profile))
  "Return finalized path.
Return NIL when path must not be used.  This makes it possible to use the
function result as a boolean in conditions."
  (expand-default-path path))

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

(defmethod expand-data-path ((path data-path) (profile (eql +private-data-profile+)))
  "Don't persist anything in private mode."
  nil)

(declaim (ftype (function (trivial-types:pathname-designator) trivial-types:pathname-designator)
                ensure-parent-exists))
(defun ensure-parent-exists (path)
  "Create parent directories of PATH if they don't exist and return PATH."
  (ensure-directories-exist (directory-namestring path))
  path)

(defvar *gpg-default-recipient* nil)

(defun gpg-recipient (file)             ; TODO: Find a proper way to do this.
  "Return the key of FILE's recipient if any, `*gpg-recipient*' otherwise.
As second value the email.
As third value the name."
  (if (uiop:file-exists-p file)
      (let* ((output (str:split (string #\newline)
                                (with-output-to-string (s)
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
      *gpg-default-recipient*))

(defmacro with-gpg-file ((stream gpg-file &rest options) &body body)
  "Like `with-open-file' but use
OPTIONS are as for `open''s `:direction'.
Other options are not supported.  File is overwritten if it exists, while
nothing is done if file is missing."
  ;; TODO: Support all of `open' options.
  (alex:with-gensyms (recipient in s clear-data)
    `(let ((,recipient (gpg-recipient ,gpg-file)))
       (flet ((gpg-write (,s)
                (uiop:run-program
                 (list *gpg-program* "--output" ',gpg-file "--recipient" ,recipient
                       "--encrypt")
                 :input ,s)))
         (if ,recipient
             ;; TODO: Ask for recipient manually.
             (warn "Please set `*gpg-default-recipient*'.")
             ,(if (match (getf options :direction)
                    ((or :io :input nil) t))
                  `(when (uiop:file-exists-p ,gpg-file)
                     (let ((,clear-data (with-output-to-string (out)
                                          (uiop:run-program
                                           (list *gpg-program* "--decrypt" ,gpg-file)
                                           :output out))))
                       (with-input-from-string (,stream ,clear-data)
                         ,@body
                         ,(unless (eq (getf options :direction) :io)
                            `(gpg-write ,stream)))))
                  `(with-input-from-string (,in (with-output-to-string (,stream)
                                                  (progn
                                                    ,@body)))
                     (gpg-write ,in))))))))

(defmacro with-maybe-gpg-file ((stream filespec &rest options) &body body)
  "Evaluate BODY with STREAM bound to DATA-PATH.
DATA-PATH can be a GPG-encrypted file if it ends with a .gpg extension.
If DATA-PATH expands to NIL or the empty string, do nothing.
OPTIONS are as for `open'.
Parent directories are created if necessary."
  `(if (str:ends-with? ".gpg" ,filespec :ignore-case t)
       (with-gpg-file (,stream ,filespec ,@options)
         ,@body)
       (progn
         ,(when (and (match (getf options :direction)
                       ((or :io :output) t))
                     (match (nth-value 2 (get-properties options '(:if-does-not-exist)))
                       (nil t)
                       ((guard l (eq (getf l :if-does-not-exist) :create)) t)))
            `(ensure-parent-exists ,filespec))
         (with-open-file (,stream ,filespec ,@options)
           ,@body))))
