;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)
;; Packagers are welcome to customize the `defparameter's to suit the host system.

(export-always '*options*)
(defvar *options* '()
  "The list of command line options.")

(defvar *run-from-repl-p* t
  "If non-nil, don't terminate the Lisp process when quitting the browser.
This is useful when the browser is run from a REPL so that quitting does not
close the connection.")

(defvar *restart-on-error* nil
  "Control variable to enable accurate error reporting during startup.
Implementation detail.
For user-facing controls, see `*run-from-repl-p*'.")

(export-always '*open-program*)
(declaim (type (or string null) *open-program*))
(defvar *open-program*
  #+darwin "open"
  #+(and (or linux bsd) (not darwin)) "xdg-open"
  #-(or linux bsd darwin) nil
  "The program to open unsupported files with.")

(export-always '*headless-p*)
(defvar *headless-p* nil
  "If non-nil, don't display anything.
This is convenient for testing purposes or to drive Nyxt programmatically.")

(export-always '*quitting-nyxt-p*)
(defvar *quitting-nyxt-p* nil
  "When non-nil, Nyxt is quitting.")

(export-always '*browser*)
(defvar *browser* nil
  "The entry-point object to a complete instance of Nyxt.
It can be initialized with

  (setf *browser* (make-instance 'browser))

It's possible to run multiple interfaces of Nyxt at the same time.  You can
let-bind *browser* to temporarily switch interface.")


(declaim (type (maybe renderer) *renderer*))
(defparameter *renderer* nil
  ;; TODO: Switching renderer does not seem to work anymore.
  ;; Maybe issue at the library level?
  "The renderer used by Nyxt.
It can be changed between two runs of Nyxt when run from a Lisp REPL.
Example:

  (nyxt:quit)
  (setf nyxt::*renderer* (make-instance 'nyxt/renderer/gtk:gtk-renderer))
  (nyxt:start)")

(export-always '+version+)
(alex:define-constant +version+
    (or (uiop:getenv "NYXT_VERSION")
        (asdf/component:component-version (asdf:find-system :nyxt)))
  :test #'equal
  :documentation "Nyxt version.
Can be overridden via NYXT_VERSION environment variable.")

(defun parse-version (version)
  "Helper to parse VERSION as a string.

Return NIL on error.
Return major version as an integer on pre-releases.
Otherwise, return 3 values:
- major version as an integer,
- minor version as an integer,
- patch version as an integer."
  (ignore-errors
   (if (search "pre-release" version)
       (first (sera:words version))
       (destructuring-bind (&optional major minor patch) (uiop:parse-version version)
         (values major minor patch)))))

(defun version ()
  "Get the version of Nyxt parsed as multiple values.
See `parse-version' for details on the returned values."
  (parse-version +version+))

(multiple-value-bind (major minor patch) (version)
  (flet ((push-feature (string)
           (pushnew (intern (uiop:strcat "NYXT-"
                                         (string-upcase (princ-to-string string)))
                            "KEYWORD")
                    *features*)))
    (when +version+ (push-feature +version+))
    (when major (push-feature major))
    (when minor (push-feature (format nil "~a.~a" major minor)))
    (when patch (push-feature (format nil "~a.~a.~a" major minor patch)))))

(export-always '*static-data*)
(defvar *static-data* (make-hash-table :test 'equal)
  "Static data for usage in Nyxt.")

(defun load-assets (subdirectory read-function)
  (mapcar (lambda (i)
            (setf (gethash (file-namestring i) *static-data*)
                  (funcall read-function i)))
          (uiop:directory-files (asdf:system-relative-pathname :nyxt (format nil "assets/~a/" subdirectory)))))

(load-assets "fonts" #'alex:read-file-into-byte-vector)
(load-assets "glyphs" #'alex:read-file-into-string)

;; Load assets needed for `nyxt.desktop' creation.
#+(and unix (not darwin))
(load-assets "icons" #'alex:read-file-into-byte-vector)
#+(and unix (not darwin))
(setf (gethash "nyxt.desktop" *static-data*)
      (alex:read-file-into-string
       (asdf:system-relative-pathname :nyxt "assets/nyxt.appimage.desktop")))
