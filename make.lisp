;;; make.lisp --- create binary files for Next
;;;
;;; See documents/README.org for more information on installing the
;;; dependencies necessary to build Next from source
;;;
;;; Please note that this script must be run from the root directory
;;; of the repository.

(defun maybe-load-quicklisp (path)
  (ignore-errors
    (load (merge-pathnames path (user-homedir-pathname)) :if-does-not-exist nil)))

(find-if (function maybe-load-quicklisp)
         '(".quicklisp/setup.lisp" "quicklisp/setup.lisp"))

(require :asdf)

(push "./" asdf:*central-registry*)
(ql:quickload "next/cocoa/application" :silent t)

(defparameter *source-dir* (make-pathname :name nil :type nil :defaults *load-truename*))
(defparameter *build-dir* (merge-pathnames "build/" *source-dir*))

(defvar *bundle-dir*)
(defvar *contents-dir*)
(defvar *resources-dir*)
(defvar *macos-dir*)

(defun build-next (&optional (build-dir *build-dir*))
  (let* ((*build-dir* build-dir)
         (*bundle-dir* (merge-pathnames "Next.app/" *build-dir*))
         (*contents-dir* (merge-pathnames "Contents/" *bundle-dir*))
         (*resources-dir* (merge-pathnames "Resources/" *contents-dir*))
         (*macos-dir* (merge-pathnames "MacOS/" *contents-dir*))
         (*default-pathname-defaults* *source-dir*))
    (ccl::ensure-directories-exist *resources-dir*)
    (ccl::ensure-directories-exist (merge-pathnames "ccl/" *resources-dir*))
    (ccl::ensure-directories-exist *macos-dir*)
    (ccl::copy-file "../assets/Info.plist" (merge-pathnames "Info.plist" *contents-dir*)
                    :if-exists :supersede)
    (ccl::copy-file "../assets/next.icns" (merge-pathnames "next.icns" *resources-dir*)
                    :if-exists :supersede)
    (ccl::copy-file (ccl::kernel-path) (merge-pathnames "next" *macos-dir*)
                    :if-exists :supersede
                    :preserve-attributes t)
    (ccl::save-application (merge-pathnames "ccl/next.image" *resources-dir*)
                           :application-class (find-symbol "COCOA-APPLICATION" "CCL"))))

(build-next)
