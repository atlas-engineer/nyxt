;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class application-profile (nfiles:profile)
  ((nfiles:name "nyxt"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "We define our own 'default profile' (instead of using
`nfiles:profile' directly) so that we can specialize the methods."))

(export-always '*global-profile*)
(defvar *global-profile* (make-instance 'application-profile)
  "The profile to use in the absence of buffers and on browser-less variables.")

(define-class nyxt-file (nfiles:file)
  ((nfiles:profile *global-profile*)
   (editable-p
    t
    :type boolean
    :documentation "If the file can be editted using a text editor.
It's not always the case, take the socket for instance."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "All Nyxt files."))

(define-class nyxt-lisp-file (nyxt-file nfiles:lisp-file)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Nyxt Lisp files."))

(define-class nosave-profile (nfiles:read-only-profile application-profile)
  ((nfiles:name "nosave"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "We define our own 'default profile' (instead of using
`nfiles:profile' directly) so that we can specialize the methods."))

(defmethod nfiles:resolve ((profile application-profile) (file nyxt-file))
  "Prefix FILE base-path with PROFILE's `nfiles:name'."
  (unless (or (uiop:absolute-pathname-p (nfiles:base-path file))
              (string=
               (nfiles:name profile)
               (second (pathname-directory (uiop:ensure-directory-pathname
                                            (nfiles:base-path file))))))
    (setf (slot-value file 'nfiles:base-path)
          (sera:path-join
           (uiop:ensure-directory-pathname (nfiles:name profile))
           (nfiles:base-path file))))
  (call-next-method))

(defmethod nfiles:serialize ((profile application-profile) (file nyxt-lisp-file) stream &key)
  ;; TODO: Error handling!
  ;; We need to make sure current package is :nyxt so that symbols are printed
  ;; with consistent namespaces.
  (let ((*package* (find-package :nyxt))
        (*print-length* nil))
    (s-serialization:serialize-sexp (nfiles:content file) stream)))

(defmethod nfiles:deserialize ((profile application-profile) (file nyxt-lisp-file) raw-content &key)
  ;; We need to make sure current package is :nyxt so that symbols are printed
  ;; with consistent namespaces.
  (let ((*package* (find-package :nyxt)))
    (s-serialization:deserialize-sexp raw-content)))

(defmethod prompter:object-attributes ((file nfiles:file))
  `(("Path" ,(nfiles:expand file))      ; TODO: Trim if too long?
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

(define-class cookies-file (nfiles:data-file nyxt-file)
  ((nfiles:base-path "cookies.txt")
   (nfiles:name "cookies"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class standard-output-file (nfiles:data-file nyxt-file)
  ((nfiles:base-path "standard-output.txt")
   (nfiles:name "standard-output"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class error-output-file (nfiles:data-file nyxt-file)
  ((nfiles:base-path "error-output.txt")
   (nfiles:name "error-output"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class css-cache-directory (nfiles:data-file nyxt-file)
  ((nfiles:base-path "style-mode-css-cache/")
   (nfiles:name "mode-css-cache"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:resolve ((profile application-profile) (file css-cache-directory))
  ;; TODO: Can we be more dynamic and reuse CLOS more?
  (serapeum:path-join
   (nfiles:base-path file)
   (name profile)
   (uiop:xdg-data-home (call-next-method))))

(define-class inputs-file (nfiles:data-file nyxt-lisp-file)
  ((nfiles:base-path "inputs")
   (nfiles:name "inputs"))
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

(define-class downloads-directory (nfiles:file)
  ((nfiles:base-path (xdg-download-dir))
   (nfiles:name "downloads"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))


(export-always 'current-profile)
(defun current-profile ()
  "If `%buffer' is non-nil, return its data-profile.
Return `*global-profile*' otherwise."
  ;; TODO: %BUFFER is not defined yet. Move %BUFFER there?
  ;; `current-profile' may be called during startup when no window exists,
  ;; which means `current-buffer' neither.  But `current-buffer' calls
  ;; `current-window' which calls `ffi-window-active' and may result in a thread
  ;; dead-lock.  To prevent this, we look for the last active window without
  ;; relying on the renderer.
  (if (and *browser* (slot-value *browser* 'last-active-window))
      (let ((buffer (or (current-buffer (slot-value *browser* 'last-active-window))
                        (make-instance 'user-buffer))))
        (or (and buffer (profile buffer))
            *global-profile*))
      *global-profile*))

(-> find-file-name-path (string) (or string null))
(defun find-file-name-path (ref)
  "Return the value of the REF found in `*options*'s `:with-file'.
Example: when passed command line option --with-file foo=bar,
\(find-file-name-path \"foo\") returns \"bar\"."
  (second
   (assoc ref
          (loop for (opt value . nil) on *options*
                when (eq opt :with-path)
                  collect value)
          :test #'string=)))
