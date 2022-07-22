;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package ospm/tests
  (:use #:common-lisp #:lisp-unit2)
  (:import-from #:ospm))
(in-package :ospm/tests)

;; Generic

(defvar *test-package-name* "hello")
(defvar *test-complex-package-name* "nyxt")
(defvar *test-multi-version-package-name* "linux-libre")

(define-test package-list ()
  (assert-true (< 0 (length (ospm:list-packages))))
  (assert-typep 'ospm:os-package
                (first (ospm:list-packages))))

;; change name
(define-test test-find-package ()
  (assert-equal *test-package-name*
                (ospm:name (first (ospm:find-os-packages *test-package-name*)))))

(define-test find-multiple-package-versions ()
  (assert-true (<= 2 (length (ospm:find-os-packages *test-multi-version-package-name*)))))

(define-test package-inputs ()
  (let* ((pkg (first (ospm:find-os-packages *test-complex-package-name*)))
         (all-inputs (append
                      (ospm:inputs pkg)
                      (ospm:propagated-inputs pkg)
                      (ospm:native-inputs pkg))))
    (assert-true (mapc #'ospm:find-os-packages all-inputs))))

(define-test list-profiles ()
  (assert-true (uiop:directory-exists-p (first (ospm:list-profiles)))))

;; Tests for functional package managers.

(defvar *test-package-name* "hello")
(defvar *test-package-with-outputs* "sbcl-slynk")
(defvar *test-package-with-outputs-output* "image")

(defvar *test-profile* (uiop:resolve-absolute-location ; TODO: Can we generate a temp dir in Common Lisp?
                        (list (uiop:temporary-directory) "ospm-tests/profile")))
(defvar *test-manifest-file* (uiop:resolve-absolute-location
                              (list (uiop:temporary-directory) "ospm-tests/manifest.scm")))

(defvar *test-manifest* "(specifications->manifest '(\"hello\"))")

(define-test install-to-temp-profile ()
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)))
  (let ((process-info (ospm:install (ospm:find-os-packages *test-package-name*)
                                    *test-profile*)))
    (uiop:wait-process process-info)
    (assert-equal *test-package-name*
                  (ospm:name (ospm:parent-package
                              (first (ospm:list-packages *test-profile*)))))
    (setf process-info (ospm:uninstall (list (first (ospm:list-packages *test-profile*)))
                                       *test-profile*))
    (uiop:wait-process process-info)
    ;; TODO: Delete *test-profile* afterwards?
    ;; final profile is empty
    (assert-false (ospm:list-packages *test-profile*))))

(define-test install-output ()
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)))
  (let ((process-info (ospm:install (list (find *test-package-with-outputs-output*
                                                (ospm:outputs (first (ospm:find-os-packages *test-package-with-outputs*)))
                                                :key #'ospm:name
                                                :test #'string=))
                                    *test-profile*)))
    (uiop:wait-process process-info)
    (assert-equal *test-package-with-outputs-output*
                  (ospm:name (first (ospm:list-packages *test-profile*))))
    ;; TODO: Delete *test-profile* afterwards?
    ))

(defvar *test-package-with-versions* "libpng")
(defvar *test-package-with-versions-version* "1.2.59")

(define-test install-version ()
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)))
  (let ((process-info (ospm:install (list (first (ospm:find-os-packages
                                                  *test-package-with-versions*
                                                  :version *test-package-with-versions-version*)))
                                    *test-profile*)))
    (uiop:wait-process process-info)
    ;; FIXME
    ;; (assert-equal *test-package-with-versions-version*
    ;;               (ospm:version (ospm:parent-package
    ;;                              (first (ospm:list-packages *test-profile*)))))
    ;; TODO: Delete *test-profile* afterwards?
    ))

(define-test install-manifest-to-temp-profile ()
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname *test-profile*)
         (uiop:pathname-directory-pathname *test-manifest-file*)))
  (uiop:with-output-file (f *test-manifest-file* :if-exists :overwrite)
    (write-string *test-manifest* f))
  (let ((process-info (ospm:install-manifest *test-manifest-file*
                                             *test-profile*)))
    (uiop:wait-process process-info)
    ;; TODO: Delete *test-profile* afterwards?
    (assert-equal *test-package-name*
                  (ospm:name (ospm:parent-package
                              (first (ospm:list-packages *test-profile*)))))))

(define-test list-files ()
  (let* ((output-list (ospm:outputs (first (ospm:find-os-packages *test-package-name*))))
         (file-list (ospm:list-files
                     (list (first output-list)))))
    (assert-equal "hello"
                  (pathname-name (first file-list)))
    (assert-equal "bin"
                  (first (last (pathname-directory (first file-list)))))))
