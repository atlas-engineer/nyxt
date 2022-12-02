;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-reduce-bandwidth-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/reduce-bandwidth-mode:reduce-bandwidth-mode
                                  buffer))
      (assert-true (disable-modes* 'nyxt/reduce-bandwidth-mode:reduce-bandwidth-mode
                                   buffer)))))
