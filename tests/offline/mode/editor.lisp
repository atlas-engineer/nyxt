;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-editor-mode ()
  (let ((editor-buffer (make-instance 'nyxt/mode/editor:editor-buffer)))
    (with-current-buffer editor-buffer
      (assert-true (enable-modes* 'nyxt/mode/editor:editor-mode editor-buffer))
      (assert-true (disable-modes* 'nyxt/mode/editor:editor-mode editor-buffer))
      (assert-true (enable-modes* 'nyxt/mode/editor:plaintext-editor-mode
                                  editor-buffer))
      (assert-true (disable-modes* 'nyxt/mode/editor:plaintext-editor-mode
                                   editor-buffer)))))
