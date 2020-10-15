;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospama)

(define-class os-package ()
  ((name "")
   (version "")
   (dependencies '())
   (synopsis "")
   (home-page "")
   (licenses '()))
  (:accessor-name-transformer #'class*:name-identity))

(export-always '*manager*)
(defvar *manager* nil
  "The currently selected package manager.")

(defun run-over-packages (argument-method package-list)
  (multiple-value-bind (pre-args post-args)
      (funcall argument-method *manager*)
    (uiop:run-program
     (append
      pre-args
      (mapcar #'name package-list)
      post-args)
     :output '(:string :stripped t) )))

(defun install (package-list)
  (run-over-packages #'install-command package-list))

(defun uninstall (package-list)
  (run-over-packages #'uninstall-command package-list))

(defun list-files (package-list)
  (run-over-packages #'list-files-command package-list))

(defmethod size ((manager (eql t)) package)
  (reduce (alexandria:compose #'+  #'trivial-file-size:file-size-in-octets)
          (list-files (list package))))

(defun show (package-list)              ; TODO: Remove since useless.
  (run-over-packages #'show-command package-list))
