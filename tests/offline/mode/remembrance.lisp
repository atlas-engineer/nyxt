;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-remembrance-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/remembrance-mode:remembrance-mode buffer))
      (assert-true (disable-modes* 'nyxt/remembrance-mode:remembrance-mode buffer)))))
