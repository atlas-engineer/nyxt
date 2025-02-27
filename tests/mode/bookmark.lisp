;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-bookmark-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer)))))
