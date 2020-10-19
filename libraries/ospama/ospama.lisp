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
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

(export-always '*manager*)
(defvar *manager* :guix ;; (serapeum:resolve-executable "guix") ; TODO: Set this automatically.
  "The currently selected package manager.")

(defun run-over-packages (argument-method package-list)
  (multiple-value-bind (pre-args post-args)
      (funcall argument-method *manager*)
    (let ((command (append
                    pre-args
                    (mapcar #'name package-list)
                    post-args)))
      (uiop:launch-program command
                           :output :stream
                           :error-output :output))))

(export-always 'list-packages)
(defun list-packages (&optional profile)
  (manager-list-packages *manager* profile))

(export-always 'find-os-package)
(defun find-os-package (name)
  (manager-find-os-package *manager* name))

(export-always 'list-profiles)
(defun list-profiles ()
  (manager-list-profiles *manager*))

(export-always 'install)
(defun install (package-list &optional profile)
  (manager-install *manager* package-list profile))

(defmethod manager-install ((manager t) package-list &optional profile)
  (run-over-packages (lambda (manager) (install-command manager profile)) package-list))

(export-always 'uninstall)
(defun uninstall (package-list &optional profile)
  (manager-uninstall *manager* package-list profile))

(defmethod manager-uninstall ((manager t) package-list &optional profile)
  (run-over-packages (lambda (manager) (uninstall-command manager profile)) package-list))

;; (defmethod list-files ((manager (eql t)) package-list)
;;   (run-over-packages #'list-files-command package-list))

;; (defmethod size ((manager (eql t)) package)
;;   (reduce (alexandria:compose #'+  #'trivial-file-size:file-size-in-octets)
;;           (list-files (list package))))
