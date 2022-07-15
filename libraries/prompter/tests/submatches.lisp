;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter/tests)

(define-test submatches-test ()
  (flet ((submatch (input list)
           (let ((source 'prompter:raw-source))
             (mapcar (lambda (suggestion)
                       (let ((res (prompter:submatches suggestion source input)))
                         (when res
                           (prompter:value res))))
                     (prompter::ensure-suggestions-list source list)))))

    (assert-equal '(nil "category" nil)
                  (submatch "cat" '("cstheory" "category" "candidate")))
    (assert-equal '("care" nil nil)
                  (submatch "car" '("care" "ful" "ness")))
    (assert-equal '(nil nil nil)
                  (submatch "careful" '("care" "ful" "ness")))
    (assert-equal '(nil "switch-buffer-next" "switch-buffer" nil)
                  (submatch "swit buf" '("about" "switch-buffer-next" "switch-buffer" "delete-buffer")))))
