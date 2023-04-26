;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-repl-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/repl:repl-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/repl:repl-mode buffer)))))

(define-test evaluate-repl-mode ()
  ;; Needed because most REPL commands reload the buffer.
  (let ((buffer (make-instance 'web-buffer)))
    (with-current-buffer buffer
      (enable-modes* 'nyxt/mode/repl:repl-mode buffer)
      (let ((mode (nyxt:find-submode 'nyxt/mode/repl:repl-mode)))
        (flet ((add-and-eval-cell (cell-class input)
                 (nyxt/mode/repl:add-cell nil cell-class)
                 (setf (nyxt/mode/repl:input (nyxt/mode/repl:current-cell mode))
                       input)
                 (nyxt/mode/repl:evaluate-cell)
                 (bt:join-thread (nyxt/mode/repl::thread (nyxt/mode/repl:current-cell mode)))))
          (add-and-eval-cell 'nyxt/mode/repl:lisp-cell "(exp 5)")
          (assert-equal (list (exp 5))
                        (nyxt/mode/repl:results (nyxt/mode/repl:current-cell mode)))
          (add-and-eval-cell 'nyxt/mode/repl:shell-cell "echo 'Hello!'")
          (assert-equal "Hello!"
                        ;; Because shell may produce some newlines after the output.
                        (string-trim serapeum:whitespace (nyxt/mode/repl:output (nyxt/mode/repl:current-cell mode))))
          ;;; FIXME: Provoking and trying to handle errors makes lisp-unit2
          ;;; choke. Any way to avoid lisp-unit2 debugger?
          ;; (add-and-eval-cell 'nyxt/mode/repl:lisp-cell "\"")
          ;; (assert-typep 'end-of-file (nyxt/mode/repl:raised-condition (nyxt/mode/repl:current-cell mode)))
          ;; (add-and-eval-cell 'nyxt/mode/repl:lisp-cell "(,)")
          ;; (assert-typep 'reader-error (nyxt/mode/repl:raised-condition (nyxt/mode/repl:current-cell mode)))
          ;; (add-and-eval-cell 'nyxt/mode/repl:shell-cell "exit 1")
          ;; (assert-equal 1 (first (nyxt/mode/repl:results (nyxt/mode/repl:current-cell mode))))
          ))
      (disable-modes* 'nyxt/mode/repl:repl-mode buffer))))
