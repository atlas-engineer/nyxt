(in-package :cl-user)

(prove:plan nil)

;; Tests for functional package managers.

(defvar *test-package-name* "hello")
(defvar *test-profile* (uiop:resolve-absolute-location ; TODO: Can we generate a temp dir in Common Lisp?
                        (list (uiop:temporary-directory) "ospama-tests/profile")))
(defvar *test-manifest-file* (uiop:resolve-absolute-location 
                              (list (uiop:temporary-directory) "ospama-tests/manifest.scm")))

(defvar *test-manifest* "(specifications->manifest '(\"hello\"))")

(prove:subtest "Install to temp profile"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-parent-directory-pathname *test-profile*)))
  (let ((process-info (ospama:install (list (ospama:find-os-package *test-package-name*))
                                      *test-profile*)))
    (uiop:wait-process process-info)
    (prove:is (ospama:name (first (ospama:list-packages *test-profile*)))
              *test-package-name*)
    (setf process-info (ospama:uninstall (list (ospama:find-os-package *test-package-name*))
                                         *test-profile*))
    (uiop:wait-process process-info)
    ;; TODO: Delete *test-profile* afterwards?
    (prove:ok (null (ospama:list-packages *test-profile*)))))

(prove:subtest "Install manifest to temp profile"
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-parent-directory-pathname *test-profile*)
         (uiop:pathname-parent-directory-pathname *test-manifest-file*)))
  (uiop:with-output-file (f *test-manifest-file* :if-exists :overwrite)
    (write-string *test-manifest* f))
  (let ((process-info (ospama:install-manifest *test-manifest-file*
                                               *test-profile*)))
    (uiop:wait-process process-info)
    ;; TODO: Delete *test-profile* afterwards?
    (prove:is (ospama:name (first (ospama:list-packages *test-profile*)))
              *test-package-name*)))

(prove:subtest "List files"
  (let* ((output-list (ospama:outputs (ospama:find-os-package *test-package-name*)))
         (file-list (ospama:list-files
                     (list (first output-list)))))
    (prove:is (pathname-name (first file-list))
              "hello")
    (prove:is (first (last (pathname-directory (first file-list))))
              "bin")))

(prove:finalize)
