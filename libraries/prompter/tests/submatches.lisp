;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter/tests)

(prove:plan nil)

(prove:subtest "Submatches Test"
  (flet ((submatch (input list)
           (let ((source (make-instance 'prompter:raw-source)))
             (setf (prompter::current-input-downcase-p source)
                   (str:downcasep input))
             (mapcar (lambda (suggestion)
                       (prompter:submatches suggestion source input))
                     (prompter::ensure-suggestions-list source list)))))

    (prove:is (count nil (submatch "cat" '("cstheory" "category" "candidate")))
              2)
    (prove:is (count nil (submatch "car" '("care" "ful" "ness")))
              2)
    (prove:is (count nil (submatch "careful" '("care" "ful" "ness")))
              3)
    (prove:is (count nil (submatch "fun" '("help" "helper" "macro")))
              3)
    (prove:is (count nil (submatch "swit buf" '("about" "switch-buffer-next" "switch-buffer" "delete-buffer")))
              2)))

(prove:finalize)
