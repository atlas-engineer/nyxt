;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; We define our own 'default profile' (instead of using `nfiles:profile'
;; directly) so that we can specialize the methods
(define-class nyxt-profile (nfiles:profile)
  ((nfiles:name "nyxt"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "With the default profile all data is persisted to the
standard locations."))

(export-always '*global-profile*)
(defvar *global-profile* (make-instance 'nyxt-profile)
  "The profile to use in the absence of buffers and on browser-less variables.")

(define-class nyxt-file (nfiles:gpg-file)
  ((nfiles:profile *global-profile*)
   (nfiles:on-external-modification 'nfiles:reload)
   (editable-p
    t
    :type boolean
    :documentation "If the file can be editted using a text editor.
It's not always the case, take the socket for instance."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "All Nyxt files.
By default, a file that fails to be loaded is automatically backed up.
If the file is modified externally, Nyxt automatically reloads it."))

(define-class nyxt-data-directory (nfiles:data-file nyxt-file)
  ((nfiles:base-path #p""))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class nyxt-temporary-directory (nfiles:data-file nyxt-file)
  ((nfiles:base-path #p""))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:resolve ((profile nyxt-profile) (path nyxt-temporary-directory))
  "Expand all data paths inside a temporary directory."
  (uiop:ensure-pathname
   (uiop:merge-pathnames* (nfiles:name profile) (uiop:temporary-directory))
   :ensure-directory t))

(define-class nyxt-lisp-file (nfiles:gpg-lisp-file nyxt-file)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Nyxt Lisp files."))

(define-class nosave-profile (nfiles:read-only-profile nyxt-profile)
  ((nfiles:name "nosave"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "With the nosave profile no data should be persisted to disk.
No data should be shared with other nosave buffers either."))

(defun find-file-name-path (ref)
  "Return the value of the REF found in `*options*'s `:with-file'.
Example: when passed command line option --with-file foo=bar,
\(find-file-name-path \"foo\") returns \"bar\"."
  (unless (uiop:emptyp ref)
    (uiop:ensure-pathname
     (second
      (assoc ref
             (loop for (opt value . nil) on *options*
                   when (eq opt :with-file)
                     collect value)
             :test #'string=)))))

(defmethod nfiles:resolve ((profile nfiles:profile) (file nyxt-file))
  (sera:path-join (uiop:ensure-directory-pathname (nfiles:name profile)) (call-next-method)))

(defmethod nfiles:read-file :around ((profile nyxt-profile) (file nyxt-file) &key)
  (unless (typep file 'nfiles:virtual-file)
    (let ((path (nfiles:expand file)))
      (unless (nfiles:nil-pathname-p path)
        (log:info "Loading ~s." path)
        (if *run-from-repl-p*
            (call-next-method)
            (handler-case (call-next-method)
              (error (c)
                (log:info "Failed to load ~s: ~a" path c)
                (handler-case (let ((backup (nfiles:backup path)))
                                (log:info "Erroring file backed up at ~s." backup))
                  (error (c)
                    (log:info "Failed to back up file: ~a" c)
                    nil))
                nil)))))))

(defmethod nfiles:write-file :around ((profile nyxt-profile) (file nyxt-file) &key)
  (if *run-from-repl-p*
      (call-next-method)
      (handler-case (call-next-method)
        (error (c)
          (log:info "Failed to save ~s: ~a" (nfiles:expand file) c)
          nil))))

(defmethod nfiles:serialize ((profile nyxt-profile) (file nyxt-lisp-file) stream &key)
  ;; We need to make sure current package is :nyxt so that symbols are printed
  ;; with consistent namespaces.
  (let ((*package* (find-package :nyxt))
        (*print-length* nil))
    (s-serialization:serialize-sexp (nfiles:content file) stream)))

(defmethod nfiles:deserialize ((profile nyxt-profile) (file nyxt-lisp-file) raw-content &key)
  ;; We need to make sure current package is :nyxt so that symbols are printed
  ;; with consistent namespaces.
  (let ((*package* (find-package :nyxt)))
    (s-serialization:deserialize-sexp raw-content)))

(defmethod prompter:object-attributes ((file nfiles:file))
  `(("Path" ,(uiop:native-namestring (nfiles:expand file)))      ; TODO: Trim if too long?
    ("Exists?" ,(if (uiop:file-exists-p (uiop:ensure-pathname (nfiles:expand file)))
                    "yes"
                    "no"))
    ("Type" ,(string (sera:class-name-of file)))
    ("Name" ,(nfiles:name file))))

(define-class user-file-source (prompter:source)
  ((prompter:name "User files")
   (prompter:active-attributes-keys '("Path" "Exists?" "Type" "Name"))
   (prompter:constructor (let ((path-map (make-hash-table :test 'equal)))
                           (dolist (file (nfiles:all-files :nyxt)) ; TODO: Filter by subclasses instead?
                             (sera:and-let* ((nyxt-file-p file)
                                             (editable? (editable-p file))
                                             (full-path (nfiles:expand file)))
                               (unless (uiop:directory-pathname-p full-path)
                                 (setf (gethash full-path path-map) file))))
                           (alexandria:hash-table-values path-map)))))

(define-class standard-output-file (nfiles:data-file nyxt-file)
  ((nfiles:base-path #p"standard-output.txt")
   (nfiles:name "standard-output"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class error-output-file (nfiles:data-file nyxt-file)
  ((nfiles:base-path #p"error-output.txt")
   (nfiles:name "error-output"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always 'xdg-download-dir)
(defun xdg-download-dir ()
  (let ((dir (ignore-errors (uiop:run-program '("xdg-user-dir" "DOWNLOAD")
                                              :output '(:string :stripped t)))))
    (when (or (null dir) (uiop:pathname-equal dir (user-homedir-pathname)))
      (setf dir (uiop:getenv "XDG_DOWNLOAD_DIR")))
    (unless dir
      (setf dir (uiop:merge-pathnames* #p"Downloads/" (user-homedir-pathname))))
    (uiop:ensure-pathname dir :ensure-directory t)))

(define-class download-directory (nyxt-file)
  ((nfiles:base-path (xdg-download-dir))
   (nfiles:name "downloads"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always 'profile-class-name)
(defun profile-class-name (profile-class)
  (getf (mopu:slot-properties (closer-mop:ensure-finalized profile-class)
                              'nfiles:name)
        :initform))

(export-always 'list-profile-classes)
(defun list-profile-classes ()
  (cons (find-class 'nyxt:nyxt-profile)
        (mopu:subclasses 'nyxt:nyxt-profile)))

(export-always 'find-profile-class)
(defun find-profile-class (name)
  (find name
        (list-profile-classes)
        :test 'string=
        :key #'profile-class-name))
