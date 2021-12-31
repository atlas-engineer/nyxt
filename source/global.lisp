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

(export-always '*browser*)
(defvar *browser* nil
  "The entry-point object to a complete instance of Nyxt.
It can be initialized with

  (setf *browser* (make-instance 'user-browser))

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

  (hooks:add-hook *after-init-hook*
    (hooks:make-handler-void #'my-foo-function))")

(export-always '*swank-port*)
(defvar *swank-port* 4006
  "The port that Swank will open a new server on (default Emacs SLIME port
is 4005, default set to 4006 in Nyxt to avoid collisions).")

(defparameter +renderer+ nil "The renderer used by Nyxt. This value is meant to
be set to a string by the renderer itself. This variable exists to allow for
reporting by users, it does not create any functional differences in the
execution of Nyxt.")

(export-always '+newline+)
(alex:define-constant +newline+ (string #\newline) :test #'equal)

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
