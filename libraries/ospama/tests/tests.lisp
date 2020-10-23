(in-package :cl-user)

(defvar *current-directory* (asdf:system-relative-pathname
                             (asdf:find-system :nyxt/ospama) "libraries/ospama/tests/"))

(when (ospama:manager)
  (prove:run (uiop:resolve-absolute-location
              (list *current-directory* "test-generic.lisp")))
  (when (typep (ospama:manager) 'ospama:guix-manager)
    (prove:run (uiop:resolve-absolute-location
                (list *current-directory* "test-functional.lisp")))))
