;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(define-class nyxt-user::virtual-test-profile (nofile-profile)
  ((files:name :initform "virtual-test"))
  (:documentation "Test profile that does not read nor write to disk."))

(defun set-url-blocking (url &optional (buffer (nyxt:current-buffer)))
  (nyxt:buffer-load url)
  (let ((p (lpara:promise)))
    (hooks:once-on (nyxt:buffer-loaded-hook buffer) (_)
      (lpara:fulfill p))
    (lpara:force p)))

(defvar +matching-url1+
  (quri:make-uri-file :path
                      (asdf:system-relative-pathname :nyxt "tests/test-data/hello.html")))
(defvar +matching-url2+
  (quri:make-uri-file :path
                      (asdf:system-relative-pathname :nyxt "tests/test-data/bye.html")))
(defvar +non-matching-url1+
  (quri:make-uri-file :path
                      (asdf:system-relative-pathname :nyxt "tests/test-data/lorem.html")))
(defvar +non-matching-url2+
  (quri:make-uri-file :path
                      (asdf:system-relative-pathname :nyxt "tests/test-data/ipsum.html")))

(define-test auto-rules-basic ()
  (with-browser-test ("virtual-test")
    (let ((file (auto-rules-file (current-buffer))))
      (assert-eq 0
                 (length (files:content file)))
      ;; (enable-modes :modes '(nyxt/no-script-mode:no-script-mode))
      (nyxt/no-script-mode:no-script-mode)
      (let ((mode (find-submode 'nyxt/no-script-mode:no-script-mode)))
        ;; TODO: Use `save-non-default-modes-for-future-visits' instead?
        (dolist (url (list +matching-url1+ +matching-url2+))
          (add-modes-to-auto-rules (nyxt::url-infer-match url)
                                   :include (nyxt::rememberable-of (list mode))))
        (assert-eq 2
                   (length (files:content file)))

        ;; Basic match.
        (set-url-blocking +matching-url1+)
        (assert-true (nyxt:url-equal +matching-url1+ (url (current-buffer))))
        (assert-true (enabled-p mode))))))

(define-test auto-rules-remembered-modes ()
  (with-browser-test ("virtual-test")
    (let ((file (auto-rules-file (current-buffer))))
      (assert-eq 0
                 (length (files:content file)))
      (nyxt/no-script-mode:no-script-mode)
      (let ((mode (find-submode 'nyxt/no-script-mode:no-script-mode)))
        (dolist (url (list +matching-url1+ +matching-url2+))
          (add-modes-to-auto-rules (nyxt::url-infer-match url)
                                   :include (nyxt::rememberable-of (list mode))))
        (set-url-blocking +non-matching-url1+)
        (assert-true (nyxt:url-equal +non-matching-url1+ (url (current-buffer))))
        (assert-true (enabled-p mode))
        (nyxt/no-script-mode:no-script-mode)
        (assert-false (enabled-p mode))
        (set-url-blocking +non-matching-url2+)
        (assert-false (enabled-p mode))
        (set-url-blocking +non-matching-url1+)
        (assert-false (enabled-p mode))
        (nyxt/no-script-mode:no-script-mode)
        (set-url-blocking +non-matching-url2+)
        (assert-true (enabled-p mode))
        (nyxt/no-script-mode:no-script-mode)
        (assert-false (enabled-p mode))))))

(define-test auto-rules-navigation-and-restoration ()
  (with-browser-test ("virtual-test")
    (let ((file (auto-rules-file (current-buffer))))
      (assert-eq 0
                 (length (files:content file)))
      (nyxt/no-script-mode:no-script-mode)
      (let ((mode (find-submode 'nyxt/no-script-mode:no-script-mode)))
        (dolist (url (list +matching-url1+ +matching-url2+))
          (add-modes-to-auto-rules (nyxt::url-infer-match url)
                                   :include (nyxt::rememberable-of (list mode))))

        ;; Non-matching -> matching.
        (set-url-blocking +matching-url1+)
        (assert-true (enabled-p mode))

        ;; Restore last active modes.
        (set-url-blocking +non-matching-url1+)
        (assert-false (enabled-p mode))
        (set-url-blocking +non-matching-url2+)
        (assert-false (enabled-p mode))))))

(define-test auto-rules-manual-disabling ()
  (with-browser-test ("virtual-test")
    (let ((file (auto-rules-file (current-buffer))))
      (assert-eq 0
                 (length (files:content file)))
      (nyxt/no-script-mode:no-script-mode)
      (let ((mode (find-submode 'nyxt/no-script-mode:no-script-mode)))
        (dolist (url (list +matching-url1+ +matching-url2+))
          (add-modes-to-auto-rules (nyxt::url-infer-match url)
                                   :include '((nyxt/no-script-mode:no-script-mode))))
        (set-url-blocking +matching-url1+)
        (assert-true (enabled-p mode))
        (nyxt/no-script-mode:no-script-mode)
        (assert-false (enabled-p mode))
        (set-url-blocking +non-matching-url1+)
        (assert-false (enabled-p mode))
        (set-url-blocking +matching-url1+)
        (assert-true (enabled-p mode))
        (set-url-blocking +non-matching-url2+)
        (assert-false (enabled-p mode))))))

(define-test auto-rules-navigate-and-reload ()
  (with-browser-test ("virtual-test")
    (let ((file (auto-rules-file (current-buffer))))
      (assert-eq 0
                 (length (files:content file)))
      (nyxt/no-script-mode:no-script-mode)
      (let ((mode (find-submode 'nyxt/no-script-mode:no-script-mode)))
        (dolist (url (list +matching-url1+ +matching-url2+))
          (save-non-default-modes-for-future-visits :url url))
        (set-url-blocking +non-matching-url1+)
        (assert-false (enabled-p mode))
        (nyxt/no-script-mode:no-script-mode :activate t)
        (assert-true (enabled-p mode))
        (set-url-blocking +non-matching-url2+)
        (assert-true (enabled-p mode))
        (nyxt/no-script-mode:no-script-mode :activate nil)
        (set-url-blocking +non-matching-url2+)
        (assert-false (enabled-p mode))))))
