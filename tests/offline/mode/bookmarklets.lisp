;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-bookmarklets-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/bookmarklets:bookmarklets-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/bookmarklets:bookmarklets-mode buffer)))))
