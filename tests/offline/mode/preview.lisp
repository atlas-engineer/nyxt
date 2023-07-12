;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-preview-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/preview:preview-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/preview:preview-mode buffer)))))
