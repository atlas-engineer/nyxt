(in-package :cl-user)

(prove:plan nil)

;; Tests for functional package managers.

(defvar *test-package-name* "hello")
(defvar *test-package-with-outputs* "sbcl-slynk")
(defvar *test-package-with-outputs-output* "image")

(defvar *test-profile* (uiop:resolve-absolute-location ; TODO: Can we generate a temp dir in Common Lisp?
                        (list (uiop:temporary-directory) "ospama-tests/profile")))
(defvar *test-manifest-file* (uiop:resolve-absolute-location 
                              (list (uiop:temporary-directory) "ospama-tests/manifest.scm")))

(defvar *test-manifest* "(specifications->manifest '(\"hello\"))")

(prove:subtest "Install to temp profile"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)))
  (let ((process-info (ospama:install (ospama:find-os-packages *test-package-name*)
                                      *test-profile*)))
    (uiop:wait-process process-info)
    (prove:is (ospama:name (ospama:parent-package
                            (first (ospama:list-packages *test-profile*))))
              *test-package-name*)
    (setf process-info (ospama:uninstall (list (first (ospama:list-packages *test-profile*)))
                                         *test-profile*))
    (uiop:wait-process process-info)
    ;; TODO: Delete *test-profile* afterwards?
    (prove:is (ospama:list-packages *test-profile*)
              nil
              "final profile is empty")))

(prove:subtest "Install output"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)))
  (let ((process-info (ospama:install (list (find *test-package-with-outputs-output*
                                                  (ospama:outputs (first (ospama:find-os-packages *test-package-with-outputs*)))
                                                  :key #'ospama:name
                                                  :test #'string=))
                                      *test-profile*)))
    (uiop:wait-process process-info)
    (prove:is (ospama:name (first (ospama:list-packages *test-profile*)))
              *test-package-with-outputs-output*)
    ;; TODO: Delete *test-profile* afterwards?
    ))

(prove:subtest "Install manifest to temp profile"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)
         (uiop:pathname-directory-pathname *test-manifest-file*)))
  (uiop:with-output-file (f *test-manifest-file* :if-exists :overwrite)
    (write-string *test-manifest* f))
  (let ((process-info (ospama:install-manifest *test-manifest-file*
                                               *test-profile*)))
    (uiop:wait-process process-info)
    ;; TODO: Delete *test-profile* afterwards?
    (prove:is (ospama:name (ospama:parent-package
                            (first (ospama:list-packages *test-profile*))))
              *test-package-name*)))

(prove:subtest "List files"
  (let* ((output-list (ospama:outputs (first (ospama:find-os-packages *test-package-name*))))
         (file-list (ospama:list-files
                     (list (first output-list)))))
    (prove:is (pathname-name (first file-list))
              "hello")
    (prove:is (first (last (pathname-directory (first file-list))))
              "bin")))

(prove:finalize)
