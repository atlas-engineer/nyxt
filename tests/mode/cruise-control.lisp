;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-cruise-control-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (assert-true (enable-modes* 'nyxt/mode/cruise-control:cruise-control-mode buffer))
    (assert-true (disable-modes* 'nyxt/mode/cruise-control:cruise-control-mode buffer))))

(define-test cruise-control-velocity-does-not-start-thread ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'modable-buffer))
         (mode (make-instance 'nyxt/mode/cruise-control:cruise-control-mode
                              :buffer buffer
                              :enabled-p t)))
    (push mode (modes buffer))
    (assert-false (nyxt/mode/process::thread mode))
    (nyxt/mode/cruise-control:velocity-incf mode)
    (assert-eq 1 (nyxt/mode/cruise-control:velocity mode))
    (nyxt/mode/cruise-control:velocity-incf mode)
    (assert-eq 2 (nyxt/mode/cruise-control:velocity mode))
    (nyxt/mode/cruise-control:velocity-decf mode)
    (assert-eq 1 (nyxt/mode/cruise-control:velocity mode))
    (nyxt/mode/cruise-control:velocity-zero mode)
    (assert-eq 0 (nyxt/mode/cruise-control:velocity mode))
    (assert-false (nyxt/mode/process::thread mode))))
