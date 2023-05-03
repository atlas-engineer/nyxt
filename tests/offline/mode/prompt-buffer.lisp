;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-prompt-buffer-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/prompt-buffer:prompt-buffer-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/prompt-buffer:prompt-buffer-mode buffer)))))
