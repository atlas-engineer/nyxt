;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-passthrough-mode ()
  (let ((buffer (make-instance 'input-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/passthrough-mode:passthrough-mode buffer))
      (assert-true (disable-modes* 'nyxt/passthrough-mode:passthrough-mode buffer)))))
