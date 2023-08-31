;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nasdf)

(export-always 'nasdf-file)
(defclass nasdf-file (asdf:static-file)
  ((if-does-not-exist
    :initform :error
    :initarg :if-does-not-exist
    :type (member :error nil)
    :documentation "What to do when input file is missing:
- `:error': Signal an error.
- `nil': Skip it."))
  (:documentation "Component type for files to install."))
(import 'nasdf-file :asdf-user)

(export-always 'nasdf-binary-file)
(defclass nasdf-binary-file (nasdf-file) ()
  (:documentation "Component type for executables to install."))
(import 'nasdf-binary-file :asdf-user)

(export-always 'nasdf-library-file)
(defclass nasdf-library-file (nasdf-binary-file) ()
  (:documentation "Component type for libraries (shared objects) to install."))
(import 'nasdf-library-file :asdf-user)

(export-always 'nasdf-desktop-file)
(defclass nasdf-desktop-file (nasdf-file) ()
  (:documentation "Component type for XDG .desktop files to install."))
(import 'nasdf-desktop-file :asdf-user)

(export-always 'nasdf-appdata-file)
(defclass nasdf-appdata-file (nasdf-file) ()
  (:documentation "Component type for Appdata files to install."))
(import 'nasdf-appdata-file :asdf-user)

(export-always 'nasdf-icon-directory)
(defclass nasdf-icon-directory (nasdf-file)
  ((asdf/interface::type :initform "png")) ; TODO: Is there a standard way to access the type?
  (:documentation "Component type for directory containing icon files to install.
File ot type `type' are looked for.
The last number found in the file name is used to install the icon in the right directory."))
(import 'nasdf-icon-directory :asdf-user)

;; TODO: Is it possible to list all files targetted by an ASDF system?
(export-always 'nasdf-source-directory)
(defclass nasdf-source-directory (nasdf-file)
  ((exclude-subpath
    :initform '()
    :type (or null (cons string *))
    :accessor exclude-subpath
    :initarg :exclude-subpath
    :documentation "Subpath to exclude from installation.
Subpaths are relative to the component, so

  (:nasdf-source-directory \"foo\" :exclude-subpath (\"bar\"))

means that foo/bar is excluded, but foo/baz is not.

If subpath is a directory, then all its subpaths are excluded as well.")
   (exclude-types
    :initform '("fasl")
    :type (or null (cons string *))
    :accessor exclude-types
    :initarg :exclude-types
    :documentation "Pattern of files to exclude when not using Git."))
  (:documentation "Directory of Common Lisp source files.
Subdirectory is included.
Git is used to list the tracked files -- untracked files will be ignored.
If Git is not found, fall back to copying everything except files of type in `exclude-types'.

Destination directory is given by the `dest-source-dir' generic function."))
(import 'nasdf-source-directory :asdf-user)

(defun nil-pathname-p (pathname)
  "Return non-nil if PATHNAME is `uiop:*nil-pathname*' or nil."
  (the (values boolean &optional)
       (or (null pathname)
           (uiop:pathname-equal pathname uiop:*nil-pathname*))))

(defun basename (pathname)              ; From nfiles.
  "Return the basename, that is:
- if it's a directory, the name of the directory,
- if it's a file, the name of the file including its type (extension),
- nil if it's a nil-pathname (#p\"\")."
  (if (nil-pathname-p pathname)
      nil                               ; TODO: Shouldn't we return #p"" instead?
      (first (last (pathname-directory
                    ;; Ensure directory _after_ truenamizing, otherwise if
                    ;; non-directory file exists it may not yield a directory.
                    (ensure-directory-pathname
                     (ensure-pathname pathname :truenamize t)))))))

(defun path-from-env (environment-variable default)
  (let ((env (getenv environment-variable)))
    (if env
        (ensure-directory-pathname env)
        default)))

(defun relative-path-from-env (environment-variable default)
  (let ((env (getenv environment-variable)))
    (if env
        (relativize-pathname-directory (ensure-directory-pathname env))
        default)))

;; We use `defparameter' so that paths are re-computed on system reload.
(export-always '*destdir*)
(defparameter *destdir* (if (getenv "DESTDIR")
                            (ensure-directory-pathname (getenv "DESTDIR"))
                            #p"/"))

(export-always '*prefix*)
(defparameter *prefix* (merge-pathnames* (relative-path-from-env "PREFIX" #p"usr/local/")
                                         *destdir*))

(export-always '*datadir*)
(defparameter *datadir* (path-from-env "DATADIR" (merge-pathnames* "share/" *prefix*)))
(export-always '*bindir*)
(defparameter *bindir* (path-from-env "BINDIR" (merge-pathnames* "bin/" *prefix*)))
(export-always '*libdir*)
(defparameter *libdir* (path-from-env "LIBDIR" (merge-pathnames* "lib/" *prefix*)))
(export-always 'libdir)
(defmethod libdir ((component nasdf-library-file))
  *libdir*)

(export-always '*dest-source-dir*)
(defvar *dest-source-dir* (path-from-env "NASDF_SOURCE_PATH" *datadir*)
  "Root of where the source will be installed.
Final path is resolved in `dest-source-dir'.")

(export-always 'dest-source-dir)
(defmethod dest-source-dir ((component nasdf-source-directory))
  "The directory into which the source is installed."
  (let ((name (asdf:primary-system-name (asdf:component-system component))))
    (ensure-directory-pathname
     (merge-pathnames* name *dest-source-dir*))))

(export-always '*chmod-program*)
(defvar *chmod-program* "chmod")
(export-always '*chmod-executable-arg*)
(defvar *chmod-executable-arg* "+x")

(export-always 'make-executable)
(defun make-executable (file)
  "Does nothing if files does not exist."
  ;; TODO: Use iolib/os:file-permissions instead of chmod?  Too verbose?
  (when (file-exists-p file)
    (run-program (list *chmod-program* *chmod-executable-arg* (native-namestring file)))))

(export-always 'install-file)
(defun install-file (file dest)
  "Like `copy-file' but ensures all parent directories are created if necessary."
  (ensure-all-directories-exist
   (list (directory-namestring dest)))
  (copy-file file dest))

(defmethod asdf:perform ((op asdf:compile-op) (c nasdf-file)) ; REVIEW: load-op?
  (loop for input in (asdf:input-files op c)
        for output in (asdf:output-files op c)
        do (if (or (file-exists-p input)
                   (slot-value c 'if-does-not-exist))
               (progn
                 (install-file input output)
                 ;; (format *error-output* "~&; installing file~%;  ~s~%; to~%;  ~s~%" source dest) ; Too verbose?
                 (logger "installed ~s" output))
               (logger "skipped ~s" output)))
  nil)

(defmethod asdf:output-files ((op asdf:compile-op) (c nasdf-file))
  (values (list (merge-pathnames* (pathname-name (asdf:component-name c))
                                  *prefix*))
          t))

(defmethod asdf:output-files ((op asdf:compile-op) (c nasdf-binary-file))
  (values (list (merge-pathnames* (basename (asdf:component-name c)) *bindir*))
          t))

(defmethod asdf:perform ((op asdf:compile-op) (c nasdf-binary-file))
  (call-next-method)
  (mapc #'make-executable (asdf:output-files op c))
  nil)

(defmethod asdf:output-files ((op asdf:compile-op) (c nasdf-library-file))
  (values (list (merge-pathnames* (basename (asdf:component-name c)) (libdir c)))
          t))

(defmethod asdf:output-files ((op asdf:compile-op) (c nasdf-desktop-file))
  (values (list (merge-pathnames* (merge-pathnames*
                                   (basename (asdf:component-name c))
                                   "applications/")
                                  *datadir*))
          t))

(defmethod asdf:output-files ((op asdf:compile-op) (c nasdf-appdata-file))
  (values (list (merge-pathnames* (merge-pathnames*
                                   (basename (asdf:component-name c))
                                   "metainfo/")
                                  *datadir*))
          t))

(defun scan-last-number (path)
  "Return the last number found in PATH.
Return NIL is there is none."
  (let ((result (block red
                  (reduce (lambda (&optional next-char result)
                            (if (parse-integer (string next-char) :junk-allowed t)
                                (cons next-char result)
                                (if result
                                    (return-from red result)
                                    result)))
                          (native-namestring path)
                          :initial-value '()
                          :from-end t))))
    (when result
      (coerce result 'string))))

(defmethod asdf:input-files ((op asdf:compile-op) (c nasdf-icon-directory))
  "Return all files of NASDF-ICON-DIRECTORY `type' in its directory.
File must contain a number in their path."
  (let ((result (remove-if (complement #'scan-last-number)
                           (directory-files (asdf:component-pathname c)
                                            (strcat "*." (asdf:file-type c))))))
    (let* ((dimensions (mapcar #'scan-last-number result))
           (dups (set-difference dimensions
                                 (remove-duplicates dimensions)
                                 :test 'string=)))
      (if (= 0 (length dups))
          result
          (error "Directory contains icons with duplicate dimensions: ~a" dups)))))

(defmethod asdf:output-files ((op asdf:compile-op) (c nasdf-icon-directory))
  (let ((name (asdf:primary-system-name (asdf:component-system c))))
    (values
     (mapcar (lambda (path)
               (let ((icon-size (scan-last-number path)) )
                 (format nil "~a/icons/hicolor/~ax~a/apps/~a.png"
                         *datadir*
                         icon-size icon-size
                         name)))
             (asdf:input-files op c))
     t)))

(defun git-ls-files (root dir)
  (split-string
   (run-program (append (list *git-program*
                              "-C" (native-namestring root)
                              "ls-files" (native-namestring dir)))
                :output '(:string :stripped t))
   :separator '(#\newline #\return #\linefeed)))

(defun file-excluded-type (file exclude-types)
  (member (pathname-type file) exclude-types :test 'equalp))

(defun list-directory (directory &key exclude-subpath (exclude-types '("fasl")))
  (let ((result '()))
    (collect-sub*directories
     (ensure-directory-pathname directory)
     (constantly t)
     (lambda (dir)
       (notany (lambda (exclusion)
                 (string-suffix-p (basename dir) exclusion))
               (mapcar #'basename exclude-subpath)))
     (lambda (subdirectory)
       (setf result (append result
                            (remove-if
                             (lambda (file) (file-excluded-type file exclude-types))
                             (directory-files subdirectory))))))
    result))

(export-always 'copy-directory)
(defun copy-directory (source destination &key exclude-subpath (exclude-types '("fasl")) verbose-p) ; REVIEW: Unused, but seem quite useful.
  "Copy the content (the file tree) of SOURCE to DESTINATION."
  (when verbose-p
    (logger "copy ~s/* inside ~s." source destination))
  (mapc (lambda (file)
          (unless (member (pathname-type file) exclude-types :test 'equalp)
            (let ((destination-file
                    (merge-pathnames*
                     (subpathp file (ensure-directory-pathname source))
                     (ensure-pathname destination :truenamize t :ensure-directory t))))
              (install-file file destination-file))))
        (list-directory source :exclude-subpath exclude-subpath
                               :exclude-types exclude-types)))


(defmethod asdf:input-files ((op asdf:compile-op) (component nasdf-source-directory))
  "Return all files of NASDF-SOURCE-DIRECTORY.
They are either listed with 'git ls-files' or directly if Git is not found."
  (let ((source (asdf:component-pathname component))
        (root (asdf:system-source-directory (asdf:component-system component))))
    (handler-case
        (with-current-directory (root)
          (let ((absolute-exclusions (mapcar (lambda (exclusion)
                                               (namestring
                                                (merge-pathnames*
                                                 (ensure-directory-pathname exclusion)
                                                 (ensure-directory-pathname source))))
                                             (exclude-subpath component))))
            (remove-if (lambda (file)
                         (or (file-excluded-type file (exclude-types component))
                             (let ((file-string (namestring file)))
                               (some (lambda (exclusion)
                                       (string-prefix-p exclusion file-string))
                                     absolute-exclusions))))
                       (mapcar (lambda (path)
                                 (ensure-pathname path :truenamize t))
                               (git-ls-files
                                root
                                source)))))
      (error (c)
        (warn "~a~&Git error, falling back to direct listing." c)
        (with-current-directory (root)
          (list-directory source :exclude-subpath (exclude-subpath component)
                                 :exclude-types (exclude-types component)))))))

(defmethod asdf:output-files ((op asdf:compile-op) (component nasdf-source-directory))
  (let ((root (asdf:system-source-directory (asdf:component-system component))))
    (values
     (mapcar (lambda (path)
               (merge-pathnames* (uiop:subpathp path root) (dest-source-dir component)))
             (asdf:input-files op component))
     t)))

(export-always 'nasdf-source-file)
(defclass nasdf-source-file (nasdf-file) ()
  (:documentation "Common Lisp source files.

Destination directory is given by the `dest-source-dir' generic function."))
(import 'nasdf-source-file :asdf-user)

(defmethod dest-source-dir ((component nasdf-source-file)) ; TODO: Factor with other method?
  "The directory into which the source is installed."
  (let ((name (asdf:primary-system-name (asdf:component-system component))))
    (ensure-directory-pathname
     (merge-pathnames* name *dest-source-dir*))))

(defmethod asdf:output-files ((op asdf:compile-op) (c nasdf-source-file))
  (values (list (merge-pathnames* (basename (asdf:component-name c)) (dest-source-dir c)))
          t))
