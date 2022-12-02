;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-proxy-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/proxy-mode:proxy-mode buffer))
      (assert-true (disable-modes* 'nyxt/proxy-mode:proxy-mode buffer)))))
