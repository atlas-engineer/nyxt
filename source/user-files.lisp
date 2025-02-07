;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class nyxt-profile (files:profile)
  ((files:name "nyxt"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "With the default profile all data is persisted to the
standard locations."))

(defvar *nyxt-profile* (make-instance 'nyxt-profile))

(define-class nyxt-file (files:gpg-file)
  ((files:profile *nyxt-profile*)
   (files:on-external-modification 'files:reload)
   (editable-p
    t
    :type boolean
    :documentation "Whether the file can be edited using a text editor.
It's not always the case, take the socket for instance."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "All Nyxt files.
By default, a file that fails to be loaded is automatically backed up.
If the file is modified externally, Nyxt automatically reloads it."))

(define-class nyxt-remote-file (nyxt-file files:remote-file)
  ()
  (:export-class-name-p t)
  (:documentation "A `files:remote-file' with specialized methods."))

(defmethod files:fetch ((profile nyxt-profile) (file nyxt-remote-file) &key)
  (dex:get (files:url file)))

(define-class nyxt-data-directory (files:data-file nyxt-file)
  ((files:base-path #p""))
  (:export-class-name-p t)
  (:documentation "Directory for Nyxt data (history, bookmarks etc.) files."))

(define-class nyxt-temporary-directory (files:data-file nyxt-file)
  ((files:base-path #p""))
  (:export-class-name-p t)
  (:documentation "File for a /tmp/`profile'-name/ directory."))

(defmethod files:resolve ((profile nyxt-profile) (path nyxt-temporary-directory))
  "Expand all data paths inside a temporary directory."
  (uiop:ensure-pathname
   (uiop:merge-pathnames* (files:name profile) (uiop:temporary-directory))
   :ensure-directory t))

(define-class nyxt-lisp-file (files:gpg-lisp-file nyxt-file)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Nyxt Lisp files."))

(defmethod files:resolve ((profile files:profile) (file nyxt-file))
  (sera:path-join (uiop:ensure-directory-pathname (files:name profile))
                  (call-next-method)))

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
  `(("Path" ,(uiop:native-namestring (files:expand file)) (:width 3))
    ("Exists?" ,(if (uiop:file-exists-p (uiop:ensure-pathname (files:expand file)))
                    "yes"
                    "no")
               (:width 1))
    ("Type" ,(string (sera:class-name-of file)) (:width 1))
    ("Name" ,(files:name file) (:width 2))))

(define-class user-file-source (prompter:source)
  ((prompter:name "User files")
   (prompter:active-attributes-keys
    '("Path" "Exists?" "Type" "Name")
    :accessor nil)
   (prompter:constructor (let ((path-map (make-hash-table :test 'equal)))
                           (dolist (file (files:all-files))
                             (and-let* ((file)
                                        ((editable-p file))
                                        (full-path (files:expand file)))
                               (when (and (funcall (alex:disjoin #'nyxt-subpackage-p #'nyxt-user-subpackage-p)
                                                   (symbol-package (sera:class-name-of file)))
                                          (not (uiop:directory-pathname-p full-path)))
                                 (setf (gethash full-path path-map) file))))
                           (alex:hash-table-values path-map)))))

(export-always 'xdg-download-dir)
(defun xdg-download-dir ()
  "Get the directory for user downloads.
Tries hard to find the XDG directory or at least ~/Downloads one."
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
  (:export-class-name-p t))
