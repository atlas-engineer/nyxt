;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-certificate-exception-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/certificate-exception-mode:certificate-exception-mode
                                  buffer))
      (assert-true (disable-modes* 'nyxt/certificate-exception-mode:certificate-exception-mode
                                   buffer)))))
