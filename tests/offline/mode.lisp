;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-class network-and-modable-buffer (network-buffer modable-buffer) ())

(define-class context-and-modable-buffer (context-buffer modable-buffer) ())

(define-class input-and-modable-buffer (input-buffer modable-buffer) ())

(define-test enable-modes-args-honored ()
  (let ((*browser* (make-instance 'browser))
        (buffer (make-instance 'modable-buffer))
        (arg-value (random 300)))
    (with-current-buffer buffer
      (setf (url (current-buffer)) (quri:uri "test"))
      (assert-true arg-value
                   (nyxt/mode/repeat:repeat-interval
                    (first (modes (first (enable-modes* 'nyxt/mode/watch:watch-mode
                                                        buffer
                                                        :repeat-interval arg-value)))))))))
