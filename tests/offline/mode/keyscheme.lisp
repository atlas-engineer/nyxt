;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-keyscheme-mode ()
  (let ((buffer (make-instance 'input-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/keyscheme-mode:keyscheme-mode buffer))
      (assert-true (disable-modes* 'nyxt/keyscheme-mode:keyscheme-mode buffer)))))
