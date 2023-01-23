;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(defun set-url-blocking (url &optional (buffer (nyxt:current-buffer)))
  (nyxt:buffer-load url)
  (let ((p (lpara:promise)))
    (hooks:once-on (nyxt:buffer-loaded-hook buffer) (_)
      (lpara:fulfill p))
    (lpara:force p)))

(defvar +url1+
  (quri:make-uri-file :path
                      (asdf:system-relative-pathname :nyxt "tests/test-data/hello.html")))
(defvar +url2+
  (quri:make-uri-file :path
                      (asdf:system-relative-pathname :nyxt "tests/test-data/lorem.html")))

(define-test remembrance ()
  (nyxt:start :no-config t :no-auto-config t
              :socket "/tmp/nyxt-test.socket"
              :profile "test")
  (nyxt:enable-modes* 'nyxt/remembrance-mode:remembrance-mode (nyxt:current-buffer))
  (let ((mode (nyxt:find-submode 'nyxt/remembrance-mode:remembrance-mode)))
    (assert-equality 'uiop:pathname-equal
                     (nfiles:join +test-root+
                                  (uiop:xdg-cache-home "nyxt" "remembrance.cache/"))
                     (nfiles:expand (nyxt/remembrance-mode::cache-path mode)))
    (assert-number-equal 0
                         (nyxt/remembrance-mode::cache-size mode))
    (set-url-blocking +url1+)

    (assert-number-equal 1
                         (nyxt/remembrance-mode::cache-size mode))
    (set-url-blocking +url2+)

    ;; TODO: For some reason loading a second URL does not trigger the on-signal-* methods in headless mode.
    ;; WebKitGTK bug?

    ;; (assert-number-equal 2
    ;;                      (nyxt/remembrance-mode::cache-size mode))

    (uiop:delete-directory-tree (nfiles:expand (nyxt/remembrance-mode::cache-path mode))
                                :validate t))
  (nyxt:quit))
