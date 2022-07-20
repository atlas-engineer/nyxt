;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter/tests)

(prove:plan nil)

(prove:subtest "Submatches Test"
  (flet ((submatch (input list)
           (let ((source 'prompter:raw-source))
             (mapcar (lambda (suggestion)
                       (let ((res (prompter:submatches suggestion source input)))
                         (when res
                           (prompter:value res))))
                     (prompter::ensure-suggestions-list source list)))))

    (prove:is (submatch "cat" '("cstheory" "category" "candidate"))
              '(nil "category" nil))
    (prove:is (submatch "car" '("care" "ful" "ness"))
              '("care" nil nil))
    (prove:is (submatch "careful" '("care" "ful" "ness"))
              '(nil nil nil))
    (prove:is (submatch "swit buf" '("about" "switch-buffer-next" "switch-buffer" "delete-buffer"))
              '(nil "switch-buffer-next" "switch-buffer" nil))))

(prove:finalize)
