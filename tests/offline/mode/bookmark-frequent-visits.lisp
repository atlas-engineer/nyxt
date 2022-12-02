;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

;; TODO bookmark-frequent-visits-mode doesn't handle the fact that the
;; (buffer-history) may return NIL.  I.e., the history-file may exist, but it
;; may be an empty file
(define-test toggle-bookmark-frequent-visits ()
  (let ((buffer (make-instance 'context-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/bookmark-frequent-visits:bookmark-frequent-visits-mode
                                  buffer))
      (assert-true (disable-modes* 'nyxt/bookmark-frequent-visits:bookmark-frequent-visits-mode
                                    buffer)))))
