;;;; make.lisp --- create binary files for nEXT
;;;;
;;;; See Next/next/README.org for more information on installing the
;;;; dependencies necessary to build nEXT from source
;;;;
;;;; Please note that this script must be run from the directory
;;;; Next/next.

(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(require :asdf)

(push "./" asdf:*central-registry*)
(ql:quickload "next" :silent t)

(defparameter *source-dir* (make-pathname :name nil :type nil
					  :defaults *load-truename*))
(defparameter *build-dir* (merge-pathnames "build/" *source-dir*))

(defvar *bundle-dir*)
(defvar *contents-dir*)
(defvar *resources-dir*)
(defvar *macos-dir*)

(defun build-next (&optional (build-dir *build-dir*))
  (let* ((*build-dir* build-dir)
	 (*bundle-dir* (merge-pathnames "nEXT.app/" *build-dir*))
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
    (copy-file (ccl::kernel-path) (merge-pathnames "nEXT" *macos-dir*)
	       :if-exists :supersede
	       :preserve-attributes t)
    (save-application (merge-pathnames "ccl/nEXT.image" *resources-dir*)
		      :application-class (find-symbol "COCOA-APPLICATION" "CCL"))))

(require 'objc-support)
(load "ccl:mac-ui;cf-utils")
(load "ccl:mac-ui;event-process")
(build-next)
