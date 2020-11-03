;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospama)

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

(declaim (ftype (function (list) uiop/launch-program::process-info) run))
(defun run (command)
  "Run COMMAND over ARGS in the background.
Use the returned `process-info' to check the output and the process termination."
  (the (values uiop/launch-program::process-info &optional)
       (uiop:launch-program command
                            :output :stream
                            :error-output :output)))
(export-always 'list-packages)
(defun list-packages (&optional profile)
  (manager-list-packages (manager) profile))

(export-always 'manager-list-packages)
(defgeneric manager-list-packages (manager &optional profile)
  (:method ((manager manager) &optional profile)
    (declare (ignorable profile))
    (error "Unspecified manager method"))
  (:documentation "Return the list of packages in PROFILE.
If PROFILE is nil, return all packages."))

(export-always 'list-package-outputs)
(defun list-package-outputs ()
  (manager-list-package-outputs (manager)))

(export-always 'manager-list-package-outputs)
(defgeneric manager-list-package-outputs (manager)
  (:method ((manager manager))
    (error "Unspecified manager method"))
  (:documentation "Return the list of all package outputs.
This only needs to be implemented for package managers that support outputs."))

(export-always 'find-os-package)
(defun find-os-package (name)
  (manager-find-os-package (manager) name))

(export-always 'manager-find-os-package)
(defgeneric manager-find-os-package (manager name)
  (:method ((manager manager) name)
    (error "Unspecified manager method"))
  (:documentation "Return the package matching NAME."))

(export-always 'list-profiles)
(defun list-profiles ()
  (manager-list-profiles (manager)))

(export-always 'manager-list-profiles)
(defgeneric manager-list-profiles (manager)
  (:method ((manager manager))
    (error "Unspecified manager method"))
  (:documentation "Return all profiles as a list of strings."))

(export-always 'list-manifests)
(defun list-manifests ()
  (manager-list-manifests (manager)))

(export-always 'manager-list-manifests)
(defmethod manager-list-manifests ((manager manager))
  (uiop:directory-files (uiop:ensure-directory-pathname manifest-directory)))

(export-always 'install)
(defun install (package-list &optional profile)
  (manager-install (manager) package-list profile))

(export-always 'manager-install)
(defmethod manager-install ((manager manager) package-list &optional profile)
  (run (append (install-command manager profile) (mapcar #'name package-list))))

(export-always 'install-command)
(defgeneric install-command (manager profile)
  (:method ((manager manager) profile)
    (error "Unspecified manager method"))
  (:documentation "Return the package manager command (as a list of string)
to install a manifest."))

(export-always 'install-manifest)
(defun install-manifest (manifest &optional profile)
  (manager-install-manifest (manager) manifest profile))

(export-always 'manager-install-manifest)
(defmethod manager-install-manifest ((manager manager) manifest &optional profile)
  (run (append (install-manifest-command manager profile)
               (list manifest))))

(export-always 'install-manifest-command)
(defgeneric install-manifest-command (manager profile)
  (:method ((manager manager) profile)
    (error "Unspecified manager method"))
  (:documentation "Return the package manager command (as a list of string)
to install a manifest."))

(export-always 'uninstall)
(defun uninstall (package-list &optional profile)
  (manager-uninstall (manager) package-list profile))

(export-always 'manager-uninstall)
(defmethod manager-uninstall ((manager manager) package-list &optional profile)
  (run (append (uninstall-command manager profile)
               (mapcar #'name package-list))))

(export-always 'uninstall-command)
(defgeneric uninstall-command (manager profile)
  (:method ((manager manager) profile)
    (error "Unspecified manager method"))
  (:documentation "Return the package manager command (as a list of string)
to uninstall packages"))

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
