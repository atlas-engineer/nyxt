;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-cruise-control-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/cruise-control:cruise-control-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/cruise-control:cruise-control-mode buffer)))))

(define-test cruise-control-velocity-does-not-start-thread ()
  (let* ((*browser* (make-instance 'browser))
         (mode (make-instance 'nyxt/mode/cruise-control:cruise-control-mode
                              :buffer (make-instance 'modable-buffer))))
    (assert-false (nyxt/mode/process::thread mode))
    (nyxt/mode/cruise-control:velocity-incf :cruise-control mode)
    (assert-eq 1 (nyxt/mode/cruise-control:velocity mode))
    (nyxt/mode/cruise-control:velocity-incf :cruise-control mode)
    (assert-eq 2 (nyxt/mode/cruise-control:velocity mode))
    (nyxt/mode/cruise-control:velocity-decf :cruise-control mode)
    (assert-eq 1 (nyxt/mode/cruise-control:velocity mode))
    (nyxt/mode/cruise-control:velocity-zero :cruise-control mode)
    (assert-eq 0 (nyxt/mode/cruise-control:velocity mode))
    (assert-false (nyxt/mode/process::thread mode))))
