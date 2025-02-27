;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nasdf)

(export-always 'nasdf-system)
(defclass nasdf-system (system) ()
  (:documentation "Extended ASDF system.
It enables features such as:
- Togglable logical-pathnames depending on NASDF_USE_LOGICAL_PATHS.
- Executable dependencies are made immutable for ASDF to prevent accidental reloads."))
(import 'nasdf-system :asdf-user)

(defmethod perform :before ((o image-op) (c nasdf-system))
  "Perform some last minute tweaks to the final image.

- Register immutable systems to prevent compiled images from
trying to recompile the application and its dependencies.
See `:*immutable-systems*'.

- If on SBCL, include `sb-sprof', the statistical profiler, since it's one of
the few modules that's not automatically included in the image."
  #+sbcl
  (require :sb-sprof)
  (map () 'register-immutable-system (already-loaded-systems)))

(defun set-new-translation (host logical-directory
                            root-directory
                            &optional (translated-directory (string-downcase (substitute #\/ #\; logical-directory))))
  "Add default translations for LOGICAL-DIRECTORY (e.g. \"foo;bar;\") in HOST.
Default translations:
- FASL files are expanded as usual with `apply-output-translations' (should default to the ASDF cache).
- Other files are expanded to their absolute location.

This effectively makes the logical pathname behave as if it had been a physical
pathname."
  (let* ((logical-directory (if (string-suffix-p logical-directory ";")
                                logical-directory
                                (strcat logical-directory ";")))
         (logical-path (strcat host ":" logical-directory "**;*.*.*"))
         (logical-fasl-path (strcat host ":" logical-directory "**;*.fasl.*"))
         (path-translation (ensure-pathname
                            (subpathname* root-directory
                                          translated-directory)
                            :ensure-directory t
                            :wilden t))
         (fasl-translation (ensure-pathname
                            (apply-output-translations
                             (subpathname* root-directory
                                           translated-directory))
                            :wilden t)))
    (if (ignore-errors (logical-pathname-translations host))
        (flet ((set-alist (key value)
                 (let ((pair (assoc key (logical-pathname-translations host)
                                    :key #'namestring
                                    :test #'string-equal)))
                   (if pair
                       (setf (rest pair) (list value))
                       (push (list key value)
                             (logical-pathname-translations host))))))
          (set-alist logical-path path-translation)
          (set-alist logical-fasl-path fasl-translation)
          ;; Return this for consistency:
          (list (list logical-fasl-path fasl-translation)
                (list logical-path path-translation)))
        (setf (logical-pathname-translations host)
              ;; WARNING: fasl path must come first as it's more specific.
              (list (list logical-fasl-path fasl-translation)
                    (list logical-path path-translation))))))

(defun logical-word-or-lose (word)      ; From  `sb-impl::logical-word-or-lose'.
  (declare (string word))
  (when (string= word "")
    (error 'namestring-parse-error
           :complaint "Attempted to treat invalid logical hostname ~
                       as a logical host:~%  ~S"
           :args (list word)
           :namestring word :offset 0))
  (let ((word (string-upcase word)))
    (dotimes (i (length word))
      (let ((ch (schar word i)))
        (unless (and (typep ch 'standard-char)
                     (or (alpha-char-p ch) (digit-char-p ch) (char= ch #\-)))
          (error 'namestring-parse-error
                 :complaint "logical namestring character which ~
                             is not alphanumeric or hyphen:~%  ~S"
                 :args (list ch)
                 :namestring word :offset i))))
    (coerce word 'string)))

(defun parse-logical-pathname (pathname)
  "Return two values:
- the host;
- the directory."
  (let* ((name (namestring pathname))
         (pos (position #\: name)))
    (when pos
      (let ((host (subseq name 0 (position #\: name))))
        (when (ignore-errors (logical-word-or-lose host))
          (values host
                  (subseq name (1+ (position #\: name)))))))))

(defmethod component-pathname ((system nasdf-system))
  "If NASDF_USE_LOGICAL_PATHS environment variable is set, use logical path source
location, otherwise use the translated path.

Tools such as Emacs (SLIME and SLY) may fail to make use of logical paths, say,
to go to the compilation error location."
  (let ((path (call-next-method)))
    (when path
      (let ((final-path (let ((host (parse-logical-pathname path)))
                          (if host
                              (progn
                                (set-new-translation host
                                                     (subseq (namestring path) (1+ (length host)))
                                                     (system-source-directory system))
                                ;; The #p reader macro expands to logical
                                ;; pathnames only if the host is already
                                ;; defined, which may not be the case at this
                                ;; point, so we remake the pathname.
                                (make-pathname :defaults path))
                              path))))
        (if (env-true-p "NASDF_USE_LOGICAL_PATHS")
            final-path
            (translate-logical-pathname final-path))))))

(defclass nyxt-renderer-system (system) ()
  (:documentation "Specialized systems for Nyxt with renderer dependency.
The renderer is configured from NYXT_RENDERER or `*nyxt-renderer*'."))
(import 'nyxt-renderer-system :asdf-user)

(export '*nyxt-renderer*)
(defvar *nyxt-renderer* (or (getenv "NYXT_RENDERER")
                            "gi-gtk"))

(defmethod component-depends-on ((o prepare-op) (c nyxt-renderer-system))
  `((load-op ,(format nil "nyxt/~a-application" *nyxt-renderer*))
    ,@(call-next-method)))
