;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-cruise-control-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/cruise-control-mode:cruise-control-mode buffer))
      (assert-true (disable-modes* 'nyxt/cruise-control-mode:cruise-control-mode buffer)))))
