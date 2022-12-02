;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-annotate-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/annotate-mode:annotate-mode buffer))
      (assert-true (disable-modes* 'nyxt/annotate-mode:annotate-mode buffer)))))
