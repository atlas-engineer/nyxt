;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-no-webgl-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/no-webgl:no-webgl-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/no-webgl:no-webgl-mode buffer)))))
