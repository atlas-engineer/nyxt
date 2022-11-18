;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package nyxt/benchmark)

(define-benchmark measure-guix-package-source ()
  "Measure the time needed to create a prompter source for Guix packages.
The build time for the Guix package database is not taken into account."
  (declare (optimize speed))
  (ospm:list-packages)
  (loop repeat 10
        do (with-benchmark-sampling
             (make-instance 'nyxt/os-package-manager-mode::os-package-source))))

(define-benchmark measure-score-suggestion-docstring ()
  "Measure the time needed to match against all Nyxt command docstrings.
Inputs are random character sequences taken from the docstrings."
  (let* ((suggestions (loop for command in nyxt::*command-list*
                            unless (uiop:emptyp (documentation command 'function))
                              collect (documentation command 'function)))
         (inverse-probability 3)
         (inputs (mapcar (lambda (suggestion)
                           (remove-if (lambda (c)
                                        (declare (ignore c))
                                        (> (random inverse-probability) 0))
                                      suggestion))
                         suggestions))
         (sum 0.0))
    (loop repeat 10
          do (with-benchmark-sampling
               (dolist (input inputs)
                 (dolist (suggestion suggestions)
                   (incf sum (prompter::score-suggestion-string input suggestion))))))))
