;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

;; TODO visual-mode relies on hint-mode to be initialized, which accounts for a
;; bad design.
(define-test toggle-visual-mode ()
  (let ((buffer (make-instance 'input-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/visual:visual-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/visual:visual-mode buffer)))))
