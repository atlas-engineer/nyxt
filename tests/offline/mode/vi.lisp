;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-vi-modes ()
  (let ((buffer (make-instance 'input-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/vi-mode:vi-normal-mode buffer))
      (assert-true (disable-modes* 'nyxt/vi-mode:vi-normal-mode buffer))
      (assert-true (enable-modes* 'nyxt/vi-mode:vi-insert-mode buffer))
      (assert-true (disable-modes* 'nyxt/vi-mode:vi-insert-mode buffer)))))
