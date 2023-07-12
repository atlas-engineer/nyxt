;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-reduce-tracking-mode ()
  (let ((buffer (make-instance 'network-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/reduce-tracking:reduce-tracking-mode
                                  buffer))
      (assert-true (disable-modes* 'nyxt/mode/reduce-tracking:reduce-tracking-mode
                                   buffer)))))
