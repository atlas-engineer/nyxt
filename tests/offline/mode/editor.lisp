;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-editor-mode ()
  (let ((editor-buffer (make-instance 'nyxt/editor-mode:editor-buffer)))
    (with-current-buffer editor-buffer
      (assert-true (enable-modes* 'nyxt/editor-mode:editor-mode editor-buffer))
      (assert-true (disable-modes* 'nyxt/editor-mode:editor-mode editor-buffer))
      (assert-true (enable-modes* 'nyxt/editor-mode:plaintext-editor-mode
                                  editor-buffer))
      (assert-true (disable-modes* 'nyxt/editor-mode:plaintext-editor-mode
                                   editor-buffer)))))
