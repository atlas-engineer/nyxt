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

    (prove:is (submatch "cat" '("cstheory" "category" "candidate"))
              "category")
    (prove:is (submatch "carefulness" '("care" "ful" "ness"))
              "carefulness")))

;; (prove:subtest "Fuzzy match"
;;   (let ((source (make-instance 'prompter:raw-source)))
;;     (flet ((match (input list)
;;              (setf (prompter::current-input-downcase-p source)
;;                    (str:downcasep input))
;;              (prompter:value
;;               (first (sort (mapcar (lambda (suggestion)
;;                                      (prompter:fuzzy-match suggestion source input))
;;                                    (prompter::ensure-suggestions-list source list))
;;                            #'prompter:score>)))))
;;       ;; (prove:is (match "hel" '("help-mode" "help" "foo-help" "help-foo-bar"))
;;       ;;           "help")
;;       )))

(prove:finalize)

(let* ((raw-src (make-instance 'prompter:raw-source))
       (sugg-list (prompter:ensure-suggestions-list raw-src
                                                    '("no" "yes" "click"))))
  (mapcar (lambda (sugg)
            (prompter:submatches sugg raw-src "n ye cli"))
          sugg-list)
  sugg-list)
