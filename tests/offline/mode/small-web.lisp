;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-small-web-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/small-web-mode:small-web-mode buffer))
      (assert-true (disable-modes* 'nyxt/small-web-mode:small-web-mode buffer)))))
