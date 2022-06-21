;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

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

(export-always '*nyxt-libdir*)
(defparameter *nyxt-libdir* (merge-pathnames* "nyxt/" *libdir*))
(export-always '*dest-source-dir*)
(defparameter *dest-source-dir* (path-from-env "NYXT_SOURCE_PATH" (merge-pathnames* "nyxt/" *datadir*)))

(export-always '*chmod-program*)
(defvar *chmod-program* "chmod")
(export-always '*chmod-executable-arg*)
(defvar *chmod-executable-arg* "+x")

(export-always 'make-executable)
(defun make-executable (file)
  ;; TODO: Use iolib/os:file-permissions instead of chmod?  Too verbose?
  (run-program (list *chmod-program* *chmod-executable-arg* (native-namestring file))))

(export-always 'install-file)
(defun install-file (file dest)
  "Like `copy-file' but ensures all parent directories are created if necessary."
  (ensure-all-directories-exist
   (list (directory-namestring dest)))
  (copy-file file dest))

(export-always 'copy-directory)
(defun copy-directory (source destination &key verbose-p)
  "Copy the content (the file tree) of SOURCE to DESTINATION."
  (when verbose-p
    (format *error-output* "~&;; Copy ~s/* inside ~s.~%" source destination))
  (collect-sub*directories
   (ensure-directory-pathname source)
   (constantly t)
   t
   (lambda (subdirectory)
     (mapc (lambda (file)
             (unless (member (pathname-type file) '("o" "fasl") :test 'equalp)
               (let ((destination-file
                       (merge-pathnames*
                        (subpathp file (ensure-directory-pathname source))
                        (ensure-pathname destination :truenamize t :ensure-directory t))))
                 (install-file file destination-file))))
           (directory-files subdirectory)))))
