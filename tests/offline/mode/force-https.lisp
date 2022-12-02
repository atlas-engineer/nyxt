;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-force-https-mode ()
  (let ((buffer (make-instance 'network-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/force-https-mode:force-https-mode buffer))
      (assert-true (disable-modes* 'nyxt/force-https-mode:force-https-mode buffer)))))
