;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test terminal-mode-toggles-keys ()
  (let ((buffer (make-instance 'input-and-modable-buffer)))
    (unwind-protect
         (with-current-buffer buffer
           (enable-modes* 'nyxt/mode/terminal:terminal-mode buffer)
           (assert-true (nyxt/mode/terminal::pty
                         (find-submode 'nyxt/mode/terminal:terminal-mode buffer)))
           (assert-true (forward-input-events-p buffer))
           (nyxt/mode/terminal:toggle-terminal-keys buffer)
           (assert-false (forward-input-events-p buffer))
           (assert-false (nyxt/mode/terminal:keys-to-terminal-p
                          (find-submode 'nyxt/mode/terminal:terminal-mode buffer)))
           (nyxt/mode/terminal:toggle-terminal-keys buffer)
           (assert-true (forward-input-events-p buffer)))
      (disable-modes* 'nyxt/mode/terminal:terminal-mode buffer))))
