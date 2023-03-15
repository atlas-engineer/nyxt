;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-repl-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/repl-mode:repl-mode buffer))
      (assert-true (disable-modes* 'nyxt/repl-mode:repl-mode buffer)))))

(define-test evaluate-repl-mode ()
  ;; Needed because most REPL commands reload the buffer.
  (let ((buffer (make-instance 'web-buffer)))
    (with-current-buffer buffer
      (enable-modes* 'nyxt/repl-mode:repl-mode buffer)
      (let ((mode (nyxt:find-submode 'nyxt/repl-mode:repl-mode)))
        (flet ((add-and-eval-cell (cell-class input)
                 (nyxt/repl-mode:add-cell nil cell-class)
                 (setf (nyxt/repl-mode:input (nyxt/repl-mode:current-cell mode))
                       input)
                 (nyxt/repl-mode:evaluate-cell)
                 (bt:join-thread (nyxt/repl-mode::thread (nyxt/repl-mode:current-cell mode)))))
          (add-and-eval-cell 'nyxt/repl-mode:lisp-cell "(exp 5)")
          (assert-equal (list (exp 5))
                        (nyxt/repl-mode:results (nyxt/repl-mode:current-cell mode)))
          (add-and-eval-cell 'nyxt/repl-mode:shell-cell "echo 'Hello!'")
          (assert-equal "Hello!"
                        ;; Because shell may produce some newlines after the output.
                        (string-trim serapeum:whitespace (nyxt/repl-mode:output (nyxt/repl-mode:current-cell mode))))
          ;;; FIXME: Provoking and trying to handle errors makes lisp-unit2
          ;;; choke. Any way to avoid lisp-unit2 debugger?
          ;; (add-and-eval-cell 'nyxt/repl-mode:lisp-cell "\"")
          ;; (assert-typep 'end-of-file (nyxt/repl-mode:raised-condition (nyxt/repl-mode:current-cell mode)))
          ;; (add-and-eval-cell 'nyxt/repl-mode:lisp-cell "(,)")
          ;; (assert-typep 'reader-error (nyxt/repl-mode:raised-condition (nyxt/repl-mode:current-cell mode)))
          ;; (add-and-eval-cell 'nyxt/repl-mode:shell-cell "exit 1")
          ;; (assert-equal 1 (first (nyxt/repl-mode:results (nyxt/repl-mode:current-cell mode))))
          ))
      (disable-modes* 'nyxt/repl-mode:repl-mode buffer))))
