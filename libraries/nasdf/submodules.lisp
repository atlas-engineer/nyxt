;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nasdf)

(export-always '*submodules-directory*)
(defvar *submodules-directory* (or (getenv "NASDF_SUBMODULES_DIR")
                                   "_build")
  "Where to store the Git submodules.
The \"_build\" directory is ignored by ASDF by default, which makes it a useful
destination for developers who have their own dependencies elsewhere and just
want to test the build as would happen on the end user system.")

(export-always '*submodules-jobs*)
(defvar *submodules-jobs* (or (getenv "NASDF_SUBMODULES_JOBS")
                              4)
  "Number of parallel 'git clone' jobs to fetch the Git submodules.
A naive benchmark on a 16 Mbps bandwidth gives us

    1 job:  5m17s
    2 jobs: 3m38s
    4 jobs: 2m51s
    8 jobs: 2m21s")

(export-always 'nasdf-submodule-system)
(defclass nasdf-submodule-system (asdf:system) ()
  (:documentation "This system sole purpose is to fetch the Git submodules found in '.gitmodules' next to the system definition file."))
(import 'nasdf-submodule-system  :asdf-user)

(defmethod asdf:perform ((o asdf:compile-op) (c nasdf-submodule-system))
  (fetch-submodules c))

(defun register-submodules (component)
  ;; Ideally we should avoid writing global, stateful files to the user file
  ;; system.  So instead of writing to the ASDF config file, we register the
  ;; sudmodule directory with CL_SOURCE_REGISTRY.  This locally overrides
  ;; CL_SOURCE_REGISTRY, but it's fine since submodules are only meant for
  ;; non-developers (who probably don't set CL_SOURCE_REGISTRY).
  ;;
  ;; We must set this globally because the information would be lost within a
  ;; Lisp compiler subprocess (e.g. as used by linux-packaging).
  (flet ((ensure-absolute-path (path component)
           (if (absolute-pathname-p path)
               path
               (system-relative-pathname component path))))
    (setf (getenv "CL_SOURCE_REGISTRY")
          (strcat
           (native-namestring
            (ensure-directory-pathname
             (ensure-absolute-path *submodules-directory* component)))
           ;; Double-slash tells ASDF to traverse the tree recursively.
           "/"
           ;; Register this directory so that the system's ASD is included, just in case.
           (inter-directory-separator)
           (native-namestring (system-source-directory component))
           (if (getenv "CL_SOURCE_REGISTRY")
               (strcat (inter-directory-separator) (getenv "CL_SOURCE_REGISTRY"))
               ;; End with an empty string to tell ASDF to inherit configuration.
               (inter-directory-separator)))))
  (clear-configuration)
  (format t "; CL_SOURCE_REGISTRY: ~s~%" (getenv "CL_SOURCE_REGISTRY")))

(export-always 'fetch-submodules)
(defmethod fetch-submodules ((component asdf:component))
  (let ((cmd (list *git-program*
                   "-C" (namestring (system-source-directory component))
                   "submodule" "update" "--init" "--force"
                   "--jobs" (write-to-string *submodules-jobs*))))
    (logger "running ~s" cmd)
    (run-program cmd
                 :ignore-error-status t
                 :output t
                 :error-output t))
  (register-submodules component))
