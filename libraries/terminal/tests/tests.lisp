;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :terminal/tests
  (:use :cl :lisp-unit2))
(in-package :terminal/tests)

(defun wait-for-text (pty needle)
  (loop for delay below 100
        for text = (terminal:text pty)
        when (search needle text)
          return text
        do (sleep 0.02)))

(define-test echoes-output ()
  (let ((pty (terminal:start '("/bin/echo" "nyxt-terminal-ok"))))
    (unwind-protect
         (assert-true (wait-for-text pty "nyxt-terminal-ok"))
      (terminal:stop pty))))

(define-test writes-input ()
  (let ((pty (terminal:start '("/bin/cat"))))
    (unwind-protect
         (progn
           (terminal:enqueue-input
            pty
            (map '(vector (unsigned-byte 8))
                 #'char-code
                 (format nil "nyxt-terminal-in~%")))
           (assert-true (wait-for-text pty "nyxt-terminal-in")))
      (terminal:stop pty))))
