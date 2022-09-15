;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; We define our own 'default profile' (instead of using `files:profile'
;; directly) so that we can specialize the methods
(define-class nyxt-profile (files:profile)
  ((files:name "nyxt"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "With the default profile all data is persisted to the
standard locations."))

(export-always '*global-profile*)
(defvar *global-profile* (make-instance 'nyxt-profile)
  "The profile to use in the absence of buffers and on browser-less variables.")

(define-class nyxt-file (files:gpg-file)
  ((files:profile *global-profile*)
   (files:on-external-modification 'files:reload)
   (editable-p
    t
    :type boolean
    :documentation "Whether the file can be edited using a text editor.
It's not always the case, take the socket for instance."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "All Nyxt files.
By default, a file that fails to be loaded is automatically backed up.
If the file is modified externally, Nyxt automatically reloads it."))

(define-class nyxt-remote-file (nyxt-file files:remote-file)
  ()
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A `files:remote-file' with specialized methods."))

(defmethod files:fetch ((profile nyxt-profile) (file nyxt-remote-file) &key)
  (dex:get (files:url file)))

(define-class nyxt-data-directory (files:data-file nyxt-file)
  ((files:base-path #p""))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class nyxt-temporary-directory (files:data-file nyxt-file)
  ((files:base-path #p""))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod files:resolve ((profile nyxt-profile) (path nyxt-temporary-directory))
  "Expand all data paths inside a temporary directory."
  (uiop:ensure-pathname
   (uiop:merge-pathnames* (files:name profile) (uiop:temporary-directory))
   :ensure-directory t))

(define-class nyxt-lisp-file (files:gpg-lisp-file nyxt-file)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Nyxt Lisp files."))

(define-class nosave-profile (files:read-only-profile nyxt-profile)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "With the nosave profile no data should be persisted to disk.
No data should be shared with other nosave buffers either."))

(define-class nofile-profile (files:virtual-profile nyxt-profile)
  ((files:name "nofile"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Data is neither read nor persisted persisted to disk."))

(defun find-file-name-path (ref)
  "Return the value of the REF found in `*options*'s `:with-file'.
An empty path can be used to disable file persistence for the referenced `nyxt-file'.

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

(defmethod files:resolve ((profile files:profile) (file nyxt-file))
  (or (find-file-name-path (files:name file))
      (sera:path-join (uiop:ensure-directory-pathname (files:name profile)) (call-next-method))))

(defmethod files:read-file :around ((profile nyxt-profile) (file nyxt-file) &key)
  (unless (typep file 'files:virtual-file)
    (let ((path (files:expand file)))
      (unless (files:nil-pathname-p path)
        (log:info "Loading ~s." path)
        (if *run-from-repl-p*
            (call-next-method)
            (handler-case (call-next-method)
              (error (c)
                (log:info "Failed to load ~s: ~a" path c)
                (handler-case (let ((backup (files:backup path)))
                                (log:info "Erroring file backed up at ~s." backup))
                  (error (c)
                    (log:info "Failed to back up file: ~a" c)
                    nil))
                nil)))))))

(defmethod files:write-file :around ((profile nyxt-profile) (file nyxt-file) &key &allow-other-keys)
  (if *run-from-repl-p*
      (call-next-method)
      (handler-case (call-next-method)
        (error (c)
          (log:info "Failed to save ~s: ~a" (files:expand file) c)
          nil))))

(defmethod files:serialize ((profile nyxt-profile) (file nyxt-lisp-file) stream &key)
  ;; We need to make sure current package is :nyxt so that symbols are printed
  ;; with consistent namespaces.
  (let ((*package* (find-package :nyxt))
        (*print-length* nil))
    (s-serialization:serialize-sexp (files:content file) stream)))

(defmethod files:deserialize ((profile nyxt-profile) (file nyxt-lisp-file) raw-content &key)
  ;; We need to make sure current package is :nyxt so that symbols are printed
  ;; with consistent namespaces.
  (let ((*package* (find-package :nyxt)))
    (s-serialization:deserialize-sexp raw-content)))

(defmethod prompter:object-attributes ((file files:file) (source prompter:source))
  `(("Path" ,(uiop:native-namestring (files:expand file)))      ; TODO: Trim if too long?
    ("Exists?" ,(if (uiop:file-exists-p (uiop:ensure-pathname (files:expand file)))
                    "yes"
                    "no"))
    ("Type" ,(string (sera:class-name-of file)))
    ("Name" ,(files:name file))))

(define-class user-file-source (prompter:source)
  ((prompter:name "User files")
   (prompter:active-attributes-keys '("Path" "Exists?" "Type" "Name"))
   (prompter:constructor (let ((path-map (make-hash-table :test 'equal)))
                           (dolist (file (files:all-files))
                             (sera:and-let* ((nyxt-file-p file)
                                             (editable? (editable-p file))
                                             (full-path (files:expand file)))
                               (when (and (funcall (alex:disjoin #'nyxt-subpackage-p #'nyxt-user-subpackage-p)
                                                   (symbol-package (sera:class-name-of file)))
                                          (not (uiop:directory-pathname-p full-path)))
                                 (setf (gethash full-path path-map) file))))
                           (alexandria:hash-table-values path-map)))))

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
  ((files:base-path (xdg-download-dir))
   (files:name "downloads"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always 'profile-name)
(defun profile-name (profile-class)
  (string-downcase
   (sera:drop-suffix "-PROFILE" (symbol-name (class-name profile-class)))))

(export-always 'list-profile-classes)
(defun list-profile-classes ()
  (cons (find-class 'nyxt:nyxt-profile)
        (mopu:subclasses 'nyxt:nyxt-profile)))

(export-always 'find-profile-class)
(defun find-profile-class (name)
  "Return the `nyxt:nyxt-profile' subclass whose name is NAME-profile."
  (find (string-downcase name)
        (list-profile-classes)
        :test 'string=
        :key 'profile-name))
