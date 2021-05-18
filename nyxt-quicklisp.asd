;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; This .asd is separate from nyxt.asd because running `ql:update-dist' and
;; playing with the ASDF registries causes all sorts of issues, such as the
;; nyxt.asd systems not being found afterwards.

(defvar *quicklisp-dir* (or (uiop:getenv "NYXT_QUICKLISP_DIR")
                           "_build/quicklisp-client"))
(defvar *submodules-dir* (or (uiop:getenv "NYXT_SUBMODULES_DIR")
                            "_build/submodules"))

(defsystem "nyxt-quicklisp/submodules"
  :perform (compile-op (o c)
                       (uiop:run-program `("git"
                                           "-C" ,(namestring (system-relative-pathname c ""))
                                           "submodule" "update" "--init" "--force")
                                         :ignore-error-status t
                                         :output t)))

(defun ensure-absolute-path (path component)
  (if (uiop:absolute-pathname-p path)
      path
      (system-relative-pathname component path)))

(defun load-quicklisp (component)
  (handler-bind ((warning #'muffle-warning))
    ;; Quicklisp's setup.lisp cannot be asdf:load-ed, we must use `load' here
    ;; apparently, but ASDF triggers a warning on `load' calls inside ASDF
    ;; operatings.
    (load (format nil "~a/setup.lisp" (ensure-absolute-path *quicklisp-dir* component)))))

(defun register-submodules (component)
  ;; We set `ql:*local-project-directories*' so that Quicklisp prefers the
  ;; submodules to its own version of the packages.
  (setf (symbol-value (read-from-string "ql:*local-project-directories*"))
        (cons
         (uiop:truenamize (uiop:ensure-directory-pathname
                           (ensure-absolute-path *submodules-dir* component)))
         (symbol-value (read-from-string "ql:*local-project-directories*"))))
  ;; Ideally we should avoid writing global, stateful files to the user file
  ;; system.  So instead of writing to the ASDF config file, we register the
  ;; sudmodule directory with CL_SOURCE_REGISTRY.  This locally overrides
  ;; CL_SOURCE_REGISTRY, but it's fine since submodules are only meant for
  ;; non-developers (who probably don't set CL_SOURCE_REGISTRY).
  ;;
  ;; We must set this globally and we can't just use
  ;; `ql:*local-project-directories*' because the information would be lost
  ;; within a Lisp compiler subprocess (e.g. as used by linux-packaging).
  (setf (uiop:getenv "CL_SOURCE_REGISTRY")
        (uiop:strcat
         (namestring
          (uiop:truenamize
           (uiop:ensure-directory-pathname
            (ensure-absolute-path *submodules-dir* component))))
         ;; Double-slash tells ASDF to traverse the tree recursively.
         "/"
         ;; Register this directory so that nyxt.asd is included, just in case.
         (uiop:inter-directory-separator)
         (namestring (uiop:truenamize (uiop:pathname-directory-pathname
                                       (asdf:system-source-file component))))
         (if (uiop:getenv "CL_SOURCE_REGISTRY")
             (uiop:strcat (uiop:inter-directory-separator) (uiop:getenv "CL_SOURCE_REGISTRY"))
             ;; End with an empty string to tell ASDF to inherit configuration.
             (uiop:inter-directory-separator))))
  (asdf:clear-configuration)
  (format t "; CL_SOURCE_REGISTRY: ~s~%" (uiop:getenv "CL_SOURCE_REGISTRY")))

(defsystem "nyxt-quicklisp"
  :depends-on (nyxt-quicklisp/submodules)
  :perform (compile-op (o c)
                       (load-quicklisp c)
                       (register-submodules c)
                       (funcall (read-from-string "ql:update-dist")
                                "quicklisp" :prompt nil)))
