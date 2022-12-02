;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-user-script-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/user-script-mode:user-script-mode buffer))
      (assert-true (disable-modes* 'nyxt/user-script-mode:user-script-mode buffer)))))
