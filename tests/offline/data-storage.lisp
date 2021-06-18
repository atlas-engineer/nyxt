;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(plan nil)

(define-class test-data-path (nyxt:data-path)
  ((nyxt:dirname (uiop:temporary-directory))))

(define-class test-gpg-data-path (test-data-path) ())

(defparameter +basename+ ".nyxt-data-storage")
(defparameter +gpg-basename+ ".nyxt-data-storage.gpg")

(defun basename* (path)
  (first (last (pathname-directory (uiop:ensure-directory-pathname path)))))

(defmethod nyxt:expand-data-path ((profile data-profile) (path test-data-path))
  "Return finalized path for initialization files."
  (if (uiop:emptyp (nyxt:basename path))
      (let ((tmp (uiop:tmpize-pathname (uiop:subpathname* (nyxt:dirname path)
                                                          +basename+))))
        (setf (nyxt:basename path) (basename* tmp))
        (call-next-method))
      (call-next-method)))

(defmethod nyxt:expand-data-path ((profile data-profile) (path test-gpg-data-path))
  "Return finalized path for initialization files."
  (if (uiop:emptyp (nyxt:basename path))
      (let ((tmp (uiop:tmpize-pathname (uiop:subpathname* (nyxt:dirname path)
                                                          +gpg-basename+))))
        (setf (nyxt:basename path) (basename* tmp))
        (call-next-method))
      (call-next-method)))

(defmacro with-test-path (sym &body body)
  `(let ((,sym (make-instance 'test-data-path)))
     (unwind-protect (progn ,@body)
       (uiop:delete-file-if-exists  (nyxt:expand-path ,sym)))))

(defmacro with-test-gpg-path (sym &body body)
  `(let ((,sym (make-instance 'test-gpg-data-path)))
     (unwind-protect (progn ,@body)
       (uiop:delete-file-if-exists  (nyxt:expand-path ,sym)))))

(subtest "Data-path: Read and write"
  (with-test-path data-path
    (let ((tmp-prefix (str:concat (namestring (uiop:temporary-directory))
                                  +basename+)))
      (ok (subseq (nyxt:expand-path data-path) 0 (length tmp-prefix))
          tmp-prefix)
      (nyxt:with-data-file (s data-path :direction :output)
        (write-string "foo" s))
      (nyxt:with-data-file (s data-path)
        (is (read s) 'foo))
      (nyxt:with-data-file (s data-path :direction :input)
        (is (read s) 'foo)))))

(subtest "Data-path: More options"
  (with-test-path data-path
    (nyxt:with-data-file (s data-path :direction :output :element-type :default)
      (write-string "foo" s))
    (nyxt:with-data-file (s data-path)
      (is (read s) 'foo))
    (is-error (nyxt:with-data-file (s data-path :direction :output :if-exists :rename)
                (is (read s) 'foo))
              'error)
    (is-error (nyxt:with-data-file (s data-path :direction :output :if-exists :error)
                (is (read s) 'foo))
              'error)
    (nyxt:with-data-file (s data-path :direction :output :if-exists nil)
      (ok (null s)))))

(subtest "Data-path: Append"
  (with-test-path data-path
    (nyxt:with-data-file (s data-path :direction :output :if-exists :append)
      (write-string "foo" s))
    (nyxt:with-data-file (s data-path :direction :output :if-exists :append)
      (write-string "bar" s))
    (nyxt:with-data-file (s data-path)
      (is (read s) 'foobar))))

(subtest "Data-path: Non-existing file"
  (let ((data-path (make-instance 'test-data-path)))
    (uiop:delete-file-if-exists (nyxt:expand-path data-path))
    (nyxt:with-data-file (s data-path :if-does-not-exist nil)
      (is s nil))
    (nyxt:with-data-file (s data-path :if-does-not-exist nil)
      (is s nil))
    (is-error (nyxt:with-data-file (s data-path :if-does-not-exist :error)
                (is s nil))
              'error)
    (nyxt:with-data-file (s data-path :direction :output
                                      :if-does-not-exist nil)
      (is s nil))))

(subtest "Data-path: Integrity"
  (with-test-path data-path
    (nyxt:with-data-file (s data-path :direction :output)
      (write-string "foo" s))
    (ignore-errors
     (nyxt:with-data-file (s data-path :direction :output)
       (write-string "bar" s)
       (error "Boom!")))
    (nyxt:with-data-file (s data-path)
      (is (read s) 'foo))))

(subtest "Data-path: Preserve permissions"
  (with-test-path data-path
    (setf (iolib/os:file-permissions (nyxt:expand-path data-path))
          '(:user-read :user-write :group-read :other-read))
    (nyxt:with-data-file (s data-path :direction :output)
      (write-string "foo" s))
    (is (iolib/os:file-permissions (nyxt:expand-path data-path))
        '(:user-read :user-write :group-read :other-read))

    (setf (iolib/os:file-permissions (nyxt:expand-path data-path))
          '(:user-read :user-write))
    (nyxt:with-data-file (s data-path :direction :output)
      (write-string "foo" s))
    (is (iolib/os:file-permissions (nyxt:expand-path data-path))
        '(:user-read :user-write))))

;; TODO: Test GPG.
;; The following only works if there is some GnuPG setup, which might not be the
;; case on every system.  Set up a dummy GnuPG recipient?

;; (subtest "Data-path: GPG"
;;   (with-test-gpg-path data-path
;;     (nyxt:with-data-file (s data-path :direction :output)
;;       (write-string "foo" s))
;;     (nyxt:with-data-file (s data-path)
;;       (is (read s) 'foo))))

;; TODO: Test `:direction :io'.
;; TODO: Test that directory gets created.  How do we create a temp directory?

(finalize)
