;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospm/tests)

(defvar *current-directory* (asdf:system-relative-pathname
                             (asdf:find-system :nyxt/ospm) "libraries/ospm/tests/"))

(when (ospm:manager)
  (prove:run (uiop:resolve-absolute-location
              (list *current-directory* "test-generic.lisp")))
  (when (typep (ospm:manager) 'ospm:guix-manager)
    (prove:run (uiop:resolve-absolute-location
                (list *current-directory* "test-functional.lisp")))))

(prove:finalize)
