;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-reading-line-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/reading-line-mode:reading-line-mode buffer))
      (assert-true (disable-modes* 'nyxt/reading-line-mode:reading-line-mode buffer)))))
