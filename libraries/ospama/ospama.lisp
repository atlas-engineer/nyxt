;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospama)

;; TODO: Declare defgenerics.
;; TODO: Add more typing.

(define-class manager ()
  ((path ""
         :type (or string pathname)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

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

(define-class os-package-output ()
  ((name "")
   (parent-package nil
                   :type (or null os-package))
   (path ""
         :type (or string pathname))
   (size 0))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "OS package outputs are meaningful mostly for functional
package managers like Nix or Guix."))

(export-always '*manager*)
(defvar *manager* nil
  "The currently selected package manager.
If unset, it will default to the first `*detected-managers*'.")

(export-always '*detected-managers*)
(defvar *detected-managers* '()
  "The list of keywords corresponding to the detected managers.")

;; TODO: What would be an ideal default location?
(defvar profile-directory (uiop:xdg-data-home "profiles"))
(defvar manifest-directory (uiop:xdg-data-home "manifests"))

(defun detect-manager (name class-sym)
  "Prepend the CLASS-SYM instance to `*detected-managers*' if NAME is an
executable that can be found.
NAME can be also be a path."
  (serapeum:and-let* ((path (serapeum:resolve-executable name)))
    (push (make-instance class-sym :path path)
          *detected-managers*)))

(defun manager ()
  (unless *manager*
    (setf *manager* (first *detected-managers*)))
  *manager*)

(defun run (command)
  "Return a PROCESS-INFO of the command."
  (uiop:launch-program command
                       :output :stream
                       :error-output :output))

(defun run-over-packages (argument-method package-list) ; TODO: This is a bit convoluted.  Simplify?
  "Return a PROCESS-INFO of the command."
  (multiple-value-bind (pre-args post-args)
      (funcall argument-method (manager))
    (let ((command (append
                    pre-args
                    (mapcar #'name package-list)
                    post-args)))
      (run command))))

(export-always 'list-packages)
(defun list-packages (&optional profile)
  (manager-list-packages (manager) profile))

(export-always 'list-package-outputs)
(defun list-package-outputs ()
  (manager-list-package-outputs (manager)))

(export-always 'find-os-package)
(defun find-os-package (name)
  (manager-find-os-package (manager) name))

(export-always 'list-profiles)
(defun list-profiles ()
  (manager-list-profiles (manager)))

(export-always 'list-manifests)
(defun list-manifests ()
  (manager-list-manifests (manager)))

(defmethod manager-list-manifests ((manager manager))
  (uiop:directory-files (uiop:ensure-directory-pathname manifest-directory)))

(export-always 'install)
(defun install (package-list &optional profile)
  (manager-install (manager) package-list profile))

(defmethod manager-install ((manager manager) package-list &optional profile)
  (run-over-packages (lambda (manager) (install-command manager profile)) package-list))

(export-always 'install-manifest)
(defun install-manifest (manifest &optional profile)
  (manager-install-manifest (manager) manifest profile))

(defmethod install-manifest-command ((manager manager)) ; TODO: Define all defgenerics?
  (error "Unspecified manager command"))

(defmethod manager-install-manifest ((manager manager) manifest &optional profile)
  (run (list (lambda (manager) (install-manifest-command manager profile))
             manifest)))

(export-always 'uninstall)
(defun uninstall (package-list &optional profile)
  (manager-uninstall (manager) package-list profile))

(defmethod manager-uninstall ((manager manager) package-list &optional profile)
  (run-over-packages (lambda (manager) (uninstall-command manager profile)) package-list))

(defun list-files-recursively (directory)
  (let ((result '()))
    (uiop:collect-sub*directories
     (uiop:ensure-directory-pathname directory)
     (constantly t) (constantly t)
     (lambda (subdirectory)
       (setf result (nconc result
                           (uiop:directory-files subdirectory)))))
    result))

(export-always 'list-files)
(defun list-files (package-or-output-list)
  (manager-list-files (manager) package-or-output-list))
