;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-record-input-field-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/record-input-field:record-input-field-mode
                                  buffer))
      (assert-true (disable-modes* 'nyxt/mode/record-input-field:record-input-field-mode
                                   buffer)))))
