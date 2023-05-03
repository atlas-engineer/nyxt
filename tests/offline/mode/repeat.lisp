;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

;; TODO enabling repeat-mode invokes a prompt.  That depends on
;; prompt-buffer-generic-history, which is a browser object.
(define-test toggle-repeat-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/repeat:repeat-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/repeat:repeat-mode buffer)))))
