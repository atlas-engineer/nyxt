;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-search-buffer-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/search-buffer-mode:search-buffer-mode buffer))
      (assert-true (disable-modes* 'nyxt/search-buffer-mode:search-buffer-mode buffer)))))
