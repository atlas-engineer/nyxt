;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

(export-always 'nyxt-system)
(defclass nyxt-system (asdf:system) ()
  (:documentation "Specialized systems for Nyxt.
Every Nyxt system should use this class or a subclass.
It enables features such as:
- Togglable logical-pathnames depending on NYXT_USE_LOGICAL_PATHS.
- Togglable executable compression with NYXT_COMPRESS.
- Executable dependencies are made immutable for ASDF to prevent accidental reloads."))
;; TODO: This is how `prove' does it, not very clean.
;; Alternatively, we could switch package in nyxt.asd, but this seems cumbersome too.
(import 'nyxt-system  :asdf-user)

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c nyxt-system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression (when (getenv "NYXT_COMPRESS")
                                  (or (parse-integer (getenv "NYXT_COMPRESS")
                                                     :junk-allowed t)
                                      (string-equal "T" (getenv "NYXT_COMPRESS"))))))

(defmethod asdf:perform :before ((o asdf:image-op) (c nyxt-system))
  "Perform some last minute tweaks to the final image.

- Register immutable systems to prevent compiled images of Nyxt from
trying to recompile Nyxt and its dependencies.
See `asdf::*immutable-systems*'.

- If on SBCL, include `sb-sprof', the statistical profiler, since it's one of
the few modules that's not automatically included in the image."
  #+sbcl
  (require :sb-sprof)
  (map () 'asdf:register-immutable-system (asdf:already-loaded-systems)))

(defun set-new-translation (host logical-directory
                            root-directory
                            &optional (translated-directory (string-downcase (substitute #\/ #\; logical-directory))))
  "Add default translations for LOGICAL-DIRECTORY (e.g. \"foo;bar;\") in HOST.
Default translations:
- FASL files are expanded as usual with `asdf:apply-output-translations' (should default to the ASDF cache).
- Other files are expanded to their absolute location.

This effectively makes the logical pathname behave as if it had been a physical
pathname."
  (let* ((logical-directory (if (uiop:string-suffix-p logical-directory ";")
                                logical-directory
                                (uiop:strcat logical-directory ";")))
         (logical-path (uiop:strcat host ":" logical-directory "**;*.*.*"))
         (logical-fasl-path (uiop:strcat host ":" logical-directory "**;*.fasl.*"))
         (path-translation (uiop:ensure-pathname
                            (uiop:subpathname* root-directory
                                               translated-directory)
                            :ensure-directory t
                            :wilden t))
         (fasl-translation (uiop:ensure-pathname
                            (asdf:apply-output-translations
                             (uiop:subpathname* root-directory
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

(defun logical-word-or-lose (word) ; From  `sb-impl::logical-word-or-lose'.
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

;; Both `nasdf:component-pathname' and `asdf:component-pathname' work, but it seems more semantically correct to specialize `asdf:component-pathname' for this.
(defmethod asdf:component-pathname ((system nyxt-system))
  "If NYXT_USE_LOGICAL_PATHS environment variable is set, use logical path source
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
                                                     (asdf:system-source-directory system))
                                ;; The #p reader macro expands to logical pathnames only if
                                ;; the host is already defined, which may not be the case at
                                ;; this point, so we remake the pathname.
                                (make-pathname :defaults path))
                              path))))
        (if (env-true-p "NYXT_USE_LOGICAL_PATHS")
            final-path
            (translate-logical-pathname final-path))))))

(defclass nyxt-renderer-system (asdf:system) ()
  (:documentation "Specialized systems for Nyxt with renderer dependency.
The renderer is configured from NYXT_RENDERER or `*nyxt-renderer*'."))
(import 'nyxt-renderer-system  :asdf-user)

(export-always '*nyxt-renderer*)
(defvar *nyxt-renderer* (or (getenv "NYXT_RENDERER")
                            "gi-gtk"))

(defmethod asdf:component-depends-on ((o asdf:prepare-op) (c nyxt-renderer-system))
  `((asdf:load-op ,(format nil "nyxt/~a-application" *nyxt-renderer*))
    ,@(call-next-method)))
