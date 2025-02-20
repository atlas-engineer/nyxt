;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-bookmark-frequent-visits ()
  (let ((buffer (make-instance 'context-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/bookmark-frequent-visits:bookmark-frequent-visits-mode
                                  buffer))
      (assert-true (disable-modes* 'nyxt/bookmark-frequent-visits:bookmark-frequent-visits-mode
                                    buffer)))))
