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

(export-always '*debug-on-error*)
(defvar *debug-on-error* nil
  "Whether the Nyxt-internal debugger pops up when an error happens.
Allows the user to fix immediate errors in runtime, given enough understanding.")

(defvar *restart-on-error* nil
  "Control variable to enable accurate error reporting during startup.
Implementation detail.
For user-facing controls, see `*run-from-repl-p*' and `*debug-on-error*'.")

(export-always '*open-program*)
(declaim (type (or string null) *open-program*))
(defvar *open-program*
  #+darwin "open"
  #+(or linux bsd) "xdg-open"
  #-(or linux bsd darwin) nil)

(defvar *headless-p* nil
  "If non-nil, don't display anything.
This is convenient for testing purposes or to drive Nyxt programmatically.")

(export-always '*browser*)
(defvar *browser* nil
  "The entry-point object to a complete instance of Nyxt.
It can be initialized with

  (setf *browser* (make-instance 'browser))

It's possible to run multiple interfaces of Nyxt at the same time.  You can
let-bind *browser* to temporarily switch interface.")

(declaim (type hooks:hook-void *after-init-hook*))
(export-always '*after-init-hook*)
(defvar *after-init-hook* (make-instance 'hooks:hook-void)
  "The entry-point object to configure everything in Nyxt.
The hook takes no argument.

This hook is run after the `*browser*' is instantiated and before the
`startup' is run.

A handler can be added with:

  (hooks:add-hook *after-init-hook* 'my-foo-function)")

(declaim (type hooks:hook-void *after-startup-hook*))
(export-always '*after-startup-hook*)
(defvar *after-startup-hook* (make-instance 'hooks:hook-void)
  "Hook run when the browser is started and ready for interaction.
The handlers take no argument.")

(defvar *interactive-p* nil
  "When non-nil, allow prompt buffers during BODY execution.
This is useful to spot potential blocks when non-interactive code (for instance
scripts) tries to invoke the prompt buffer.")

(export-always '*swank-port*)
(defvar *swank-port* 4006
  "The port that Swank will open a new server on (default Emacs SLIME port
is 4005, default set to 4006 in Nyxt to avoid collisions).")

(export-always '*slynk-port*)
(defvar *slynk-port* 4006
  "The port that Slynk will open a new server on (default Emacs Sly port
is 4005, default set to 4006 in Nyxt to avoid collisions).")

(defparameter +renderer+ nil "The renderer used by Nyxt. This value is meant to
be set to a string by the renderer itself. This variable exists to allow for
reporting by users, it does not create any functional differences in the
execution of Nyxt.")

(alex:define-constant +nyxt-critical-dependencies+
    '(:cl-cffi-gtk
      :cl-gobject-introspection
      :cl-webkit2)
  :test #'equal)

(defvar +asdf-build-information+
  `(:version ,(asdf:asdf-version)
    :critical-dependencies ,(mapcar (lambda (s)
                                      (nth-value 2 (asdf:locate-system s)))
                                    +nyxt-critical-dependencies+))
  "Build-time ASDF information.
Don't set this, it would lose its meaning.")

(defvar +guix-build-information+
  (when (sera:resolve-executable "guix")
    `(:version
      ;; `guix describe' is not reliable within `guix environment'.
      ,(fourth (sera:tokens
                (first (sera:lines
                        (uiop:run-program '("guix" "--version") :output :string)))))))
  "Build-time Guix information.
Don't set this, it would lose its meaning.")

(defvar +quicklisp-build-information+
  #+quicklisp
  `(:dist-version ,(ql:dist-version "quicklisp")
    :client-version ,(ql:client-version)
    :local-project-directories ,ql:*local-project-directories*
    :critical-dependencies ,(mapcar #'ql-dist:find-system +nyxt-critical-dependencies+))
  #-quicklisp
  nil
  "Build-time Quicklisp information.
Don't set this, it would lose its meaning.")

(export-always '+version+)
(alex:define-constant +version+
    (or (uiop:getenv "NYXT_VERSION")      ; This is useful for build systems without Git.
        (ignore-errors
         (uiop:with-current-directory ((asdf:system-source-directory :nyxt))
           (uiop:run-program (list "git" "describe" "--always" "--tags")
                             :output '(:string :stripped t))))
        (asdf/component:component-version (asdf:find-system :nyxt)))
  :test #'equal)

(defun version ()
  "Return 5 values:
- MAJOR version as integer,
- MINOR version as integer,
- PATCH version as integer,
- COMMITS as number of commits from the last release,
- and current COMMIT as string.
Return nil on error."
  (ignore-errors
   ;; Pre-releases are falling outside the conventional version values.
   (if (search "pre-release" +version+)
       (parse-integer (first (str:split "-" +version+)))
       (destructuring-bind (version &optional commits commit)
           (str:split "-" +version+)
         (let* ((integer-commits-p (and commits (every #'digit-char-p commits)))
                (commits-number (if integer-commits-p
                                    (parse-integer commits)
                                    0))
                (commit (if integer-commits-p
                            commit
                            commits)))
           (destructuring-bind (&optional major minor patch)
               (uiop:parse-version version)
             (values major minor patch commit commits-number)))))))

(multiple-value-bind (major minor patch commit commits)
    (version)
  (flet ((push-feature (string)
           (pushnew (intern (uiop:strcat "NYXT-" (string-upcase (princ-to-string string))) "KEYWORD") *features*)))
    (when +version+
      (push-feature +version+))
    (when major
      (push-feature major))
    (when minor
      (push-feature (format nil "~a.~a" major minor)))
    (when patch
      (push-feature (format nil "~a.~a.~a" major minor patch)))
    (when commit
      (push-feature (string-upcase commit)))
    (when (and commits (not (zerop commits)))
      (push-feature "UNSTABLE"))))
