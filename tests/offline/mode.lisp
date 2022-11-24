;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test enable-modes-args-honored ()
  (let ((*browser* (make-instance 'browser))
        (buffer (make-instance 'modable-buffer))
        (arg-value (random 300)))
    (with-current-buffer buffer
      (setf (url (current-buffer)) (quri:uri "test"))
      (assert-true arg-value
                   (nyxt/repeat-mode:repeat-interval
                    (first (modes (first (enable-modes* 'nyxt/watch-mode:watch-mode
                                                        buffer
                                                        :repeat-interval arg-value)))))))))
