;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-no-sound-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/no-sound:no-sound-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/no-sound:no-sound-mode buffer)))))
