;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-history-tree-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/history-tree-mode:history-tree-mode buffer))
      (assert-true (disable-modes* 'nyxt/history-tree-mode:history-tree-mode buffer)))))
