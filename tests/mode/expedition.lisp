;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-expedition-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/expedition:expedition-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/expedition:expedition-mode buffer)))))

(define-class expedition-test-buffer (modable-buffer)
  ((loaded-urls '())))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sb-package-locks
  (sb-ext:unlock-package :nyxt))

(defmethod ffi-buffer-load ((buffer expedition-test-buffer) url)
  (push url (loaded-urls buffer))
  buffer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sb-package-locks
  (sb-ext:lock-package :nyxt))

(define-test expedition-next-and-previous-stop-at-ends ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'expedition-test-buffer))
         (urls (mapcar #'quri:uri '("https://example.com/a"
                                    "https://example.com/b"
                                    "https://example.com/c")))
         (mode (make-instance 'nyxt/mode/expedition:expedition-mode
                              :buffer buffer
                              :urls urls)))
    (nyxt/mode/expedition:expedition-previous :expedition mode)
    (assert-eq 0 (nyxt/mode/expedition:index mode))
    (assert-false (loaded-urls buffer))
    (nyxt/mode/expedition:expedition-next :expedition mode)
    (assert-eq 1 (nyxt/mode/expedition:index mode))
    (assert-equality #'quri:uri= (second urls) (first (loaded-urls buffer)))
    (nyxt/mode/expedition:expedition-next :expedition mode)
    (assert-eq 2 (nyxt/mode/expedition:index mode))
    (assert-equality #'quri:uri= (third urls) (first (loaded-urls buffer)))
    (nyxt/mode/expedition:expedition-next :expedition mode)
    (assert-eq 2 (nyxt/mode/expedition:index mode))
    (assert-equality #'quri:uri= (third urls) (first (loaded-urls buffer)))
    (nyxt/mode/expedition:expedition-previous :expedition mode)
    (assert-eq 1 (nyxt/mode/expedition:index mode))
    (assert-equality #'quri:uri= (second urls) (first (loaded-urls buffer)))
    (nyxt/mode/expedition:expedition-previous :expedition mode)
    (assert-eq 0 (nyxt/mode/expedition:index mode))
    (nyxt/mode/expedition:expedition-previous :expedition mode)
    (assert-eq 0 (nyxt/mode/expedition:index mode))
    (assert-equality #'quri:uri= (first urls) (first (loaded-urls buffer)))))
