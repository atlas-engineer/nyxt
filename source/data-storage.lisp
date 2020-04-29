(in-package :next)

;; TODO: Make this a separate library?
;; DONE: Test order of combinations with (sub-data-path, sub-profile) when only cross methods exist.  Left-most specific specializer has precedence.
;; DONE: Dir support. TEST!
;; TODO: Download manager directory.
;; TODO: Video-mode dir.  file-manager-mode.
;; TODO: Add CLI arguments --profile, --list-profiles, --with-path.
;; TODO: Move root to `data-path' class?

(defvar *gpg-program* "gpg")

(defclass data-profile ()
  ()                                    ; TODO: Add description for the CLI --help?  Or is variable documentation enough?
  (:documentation ""))

(defvar +default-data-profile+ (make-instance 'data-profile))
(defvar +private-data-profile+ (make-instance 'data-profile))

(defclass-export data-path ()
  ((basename :initarg :basename
             :accessor basename
             :type string
             :initform ""
             :documentation "The basename of data-path.
It can be appended with an extension if necessary.
When appended with a slash (/), it designates a folder."))
  (:documentation ""))

;; TODO: These 2 are global, no need for a separate class then, only need for an eql specializer.
(defclass socket-data-path (data-path) ())
(defclass init-file-data-path (data-path) ())

(defclass session-data-path (data-path) ())
(defclass cookies-data-path (data-path) ())
(defclass bookmarks-data-path (data-path) ())
(defclass history-data-path (data-path) ())
(defclass download-data-path (data-path) ())

(defvar +data-root+ "next")

(declaim (ftype (function (data-path &key (:root trivial-types:pathname-designator))
                          (or string null))
                expand-default-path))
(defun expand-default-path (path &key (root (uiop:xdg-data-home +data-root+)))
  "Derive file from command line option or NAME.
When PATH has no basename, return ROOT.  This is useful to refer to directories.
If NAME has a slash, return NAME.
Without slash, NAME is expanded to 'ROOT/NAME.lisp' or 'ROOT/NAME' is NAME
already contains a period."
  ;; TODO: Get command line option.
  ;; (setf name (or (getf *options* :with-path ...) name))
  (with-slots (basename) path
    (cond
      ((uiop:emptyp basename)
       root)
      ((search "/" basename)
       basename)
      (t
       (let ((name (str:concat (namestring root) "/" basename)))
         (unless (search "." basename)
           (setf name (str:concat name ".lisp")))
         name)))))

(defmethod expand-data-path ((path data-path) (profile data-profile))
  "Return finalized path.
Return NIL when path must not be used.  This makes it possible to use the
function result as a boolean in conditions."
  (expand-default-path path))

(defmethod expand-data-path ((path init-file-data-path) (profile data-profile))
  "Return finalized path.
Return NIL when path must not be used.  This makes it possible to use the
function result as a boolean in conditions."
  (expand-default-path path :root (uiop:xdg-config-home +data-root+)))

(defmethod expand-data-path ((path session-data-path) (profile data-profile))
  (expand-default-path path :root (uiop:xdg-data-home +data-root+ "sessions")))

(defun xdg-download-dir ()
  (let ((dir (ignore-errors (uiop:run-program '("xdg-user-dir" "DOWNLOAD")
                                              :output '(:string :stripped t)))))
    (when (or (null dir) (string= dir (uiop:getenv "HOME")))
      (setf dir (uiop:getenv "XDG_DOWNLOAD_DIR")))
    (unless dir
      (setf dir (str:concat (uiop:getenv "HOME") "/Downloads/")))))

(defmethod expand-data-path ((path download-data-path) (profile data-profile))
  ;; TODO: Move default-download-directory to here?
  (expand-default-path path :root (xdg-download-dir)))

(defmethod expand-data-path ((path data-path) (profile (eql +private-data-profile+)))
  "Don't persist anything in private mode."
  ;; (eql (bookmarks-path *browser*)) won't work because *browser* is not instantiated.
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
  (or (when (uiop:file-exists-p file)
        (let* ((output (str:split (string #\newline)
                                  (with-output-to-string (s)
                                    (uiop:run-program (list *gpg-program* "--decrypt" file)
                                                      :output nil :error-output s))))
               (first-line-tokens (str:split " " (first output)))
               (key (second (member "ID" first-line-tokens :test #'string=)))
               (second-line (str:trim (second output)))
               (mail-start (position #\space second-line :from-end t))
               (mail (str:trim (reduce (lambda (target rep) (str:replace-all rep "" target))
                                       '(">" "<" "\"") :initial-value (subseq second-line mail-start))))
               (name (str:replace-all "\"" "" (subseq second-line 0 mail-start))))
          (values key mail name)))
      *gpg-default-recipient*))

(export-always 'expand-path)
(declaim (ftype (function (data-path) (or string null)) expand-path))
(defun expand-path (data-path)
  "Return the expanded path of DATA-PATH.
`expand-data-path' is dispatched against `data-path' and `*browser*'s
`data-profile' if `*browser*' is instantiated, `+default-data-profile+'
otherwise.
This function can be used on browser-less globals like `*init-file-path*'."
  (the (values (or string null) &optional)
       (if *browser*
           (expand-data-path data-path (data-profile *browser*))
           (expand-data-path data-path +default-data-profile+))))

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

(defmacro with-data-file ((stream data-path &rest options) &body body)
  "Evaluate BODY with STREAM bound to DATA-PATH.
DATA-PATH can be a GPG-encrypted file if it ends with a .gpg extension.
If DATA-PATH expands to NIL or the empty string, do nothing.
OPTIONS are as for `open'.
Parent directories are created if necessary."
  `(let ((path (expand-path ,data-path)))
     (when path
       (if (str:ends-with? ".gpg" path :ignore-case t)
           (with-gpg-file (,stream path ,@options)
             ,@body)
           (progn
             ,(when (and (match (getf options :direction)
                           ((or :io :output) t))
                         (match (nth-value 2 (get-properties options '(:if-does-not-exist)))
                           (nil t)
                           ((guard l (eq (getf l :if-does-not-exist) :create)) t)))
                `(ensure-parent-exists path))
             (with-open-file (,stream path ,@options)
               ,@body))))))
