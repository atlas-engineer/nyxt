;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

;; TODO repl-mode doesn't like the fact (mode-instance evaluation) returns NIL.
(define-test toggle-repl-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/repl-mode:repl-mode buffer))
      (assert-true (disable-modes* 'nyxt/repl-mode:repl-mode buffer)))))
