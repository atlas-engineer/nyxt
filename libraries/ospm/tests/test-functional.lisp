;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospm/tests)

(prove:plan nil)

;; Tests for functional package managers.

(defvar *test-package-name* "hello")
(defvar *test-package-with-outputs* "sbcl-slynk")
(defvar *test-package-with-outputs-output* "image")

(defvar *test-profile* (uiop:resolve-absolute-location ; TODO: Can we generate a temp dir in Common Lisp?
                        (list (uiop:temporary-directory) "ospm-tests/profile")))
(defvar *test-manifest-file* (uiop:resolve-absolute-location 
                              (list (uiop:temporary-directory) "ospm-tests/manifest.scm")))

(defvar *test-manifest* "(specifications->manifest '(\"hello\"))")

(prove:subtest "Install to temp profile"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)))
  (let ((process-info (ospm:install (ospm:find-os-packages *test-package-name*)
                                      *test-profile*)))
    (uiop:wait-process process-info)
    (prove:is (ospm:name (ospm:parent-package
                            (first (ospm:list-packages *test-profile*))))
              *test-package-name*)
    (setf process-info (ospm:uninstall (list (first (ospm:list-packages *test-profile*)))
                                         *test-profile*))
    (uiop:wait-process process-info)
    ;; TODO: Delete *test-profile* afterwards?
    (prove:is (ospm:list-packages *test-profile*)
              nil
              "final profile is empty")))

(prove:subtest "Install output"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)))
  (let ((process-info (ospm:install (list (find *test-package-with-outputs-output*
                                                  (ospm:outputs (first (ospm:find-os-packages *test-package-with-outputs*)))
                                                  :key #'ospm:name
                                                  :test #'string=))
                                      *test-profile*)))
    (uiop:wait-process process-info)
    (prove:is (ospm:name (first (ospm:list-packages *test-profile*)))
              *test-package-with-outputs-output*)
    ;; TODO: Delete *test-profile* afterwards?
    ))

(defvar *test-package-with-versions* "libpng")
(defvar *test-package-with-versions-version* "1.2.59")

(prove:subtest "Install version"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)))
  (let ((process-info (ospm:install (list (first (ospm:find-os-packages
                                                    *test-package-with-versions*
                                                    :version *test-package-with-versions-version*)))
                                      *test-profile*)))
    (uiop:wait-process process-info)
    (prove:is (ospm:version (ospm:parent-package
                               (first (ospm:list-packages *test-profile*))))
              *test-package-with-versions-version*)
    ;; TODO: Delete *test-profile* afterwards?
    ))

(prove:subtest "Install manifest to temp profile"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)
         (uiop:pathname-directory-pathname *test-manifest-file*)))
  (uiop:with-output-file (f *test-manifest-file* :if-exists :overwrite)
    (write-string *test-manifest* f))
  (let ((process-info (ospm:install-manifest *test-manifest-file*
                                               *test-profile*)))
    (uiop:wait-process process-info)
    ;; TODO: Delete *test-profile* afterwards?
    (prove:is (ospm:name (ospm:parent-package
                            (first (ospm:list-packages *test-profile*))))
              *test-package-name*)))

(prove:subtest "List files"
  (let* ((output-list (ospm:outputs (first (ospm:find-os-packages *test-package-name*))))
         (file-list (ospm:list-files
                     (list (first output-list)))))
    (prove:is (pathname-name (first file-list))
              "hello")
    (prove:is (first (last (pathname-directory (first file-list))))
              "bin")))

(prove:finalize)
