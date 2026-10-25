;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(unless lparallel:*kernel* (setf lparallel:*kernel*
                                 (lparallel:make-kernel (or (serapeum:count-cpus) 1))))

(define-class test-source (prompter:source)
  ;; The number of suggestions is only relevant to assess the algorithm's
  ;; performance, so it suffices to use a single suggestion for general testing.
  ((prompter:name "Test")
   (prompter:constructor '("suggestion"))))

(defun set-test-source-object-attributes (widths)
  (handler-bind ((warning #'muffle-warning))
    (defmethod prompter:object-attributes ((object string) (source test-source))
      (loop for width in widths
            for i from 1
            collect `(,(str:concat "Attribute-" (princ-to-string i))
                      ,(when width (make-string width))
                      (:width ,width))))))

(define-test uniform-width-attributes ()
  (let ((size (sera:random-in-range 1 11))
        (width (sera:random-in-range 1 11)))
    (set-test-source-object-attributes (make-list size :initial-element width))
    (assert-equal (make-list size :initial-element (/ width (* width size)))
                  (nyxt::attribute-widths (make-instance 'test-source)))))

(define-test fallback-on-unallocated-attributes-widths ()
  "Fallback to uniform width distribution when at least one ratio isn't specified."
  (set-test-source-object-attributes (list 1 2 3 nil))
  (assert-equal (make-list 4 :initial-element 1/4)
                (nyxt::attribute-widths (make-instance 'test-source))))

(define-test preserve-ratios-on-inactive-attributes ()
  "Preserve original proportions between attributes' width on disable."
  (set-test-source-object-attributes (list 1 2 3))
  (let ((source (make-instance 'test-source)))
    (setf (slot-value source 'prompter:active-attributes-keys) '("Attribute-1" "Attribute-2"))
    (assert-equal (list 1/3 2/3)
                  (nyxt::attribute-widths source))
    (setf (slot-value source 'prompter:active-attributes-keys) '("Attribute-2" "Attribute-3"))
    (assert-equal (list 2/5 3/5)
                  (nyxt::attribute-widths source))
    (setf (slot-value source 'prompter:active-attributes-keys) '("Attribute-1" "Attribute-3"))
    (assert-equal (list 1/4 3/4)
                  (nyxt::attribute-widths source))))

(define-test composite-source-lists-top-suggestions-from-each-source ()
  (let* ((alpha-action (lambda (values) (list :alpha values)))
         (beta-action (lambda (values) (list :beta values)))
         (alpha (make-instance 'prompter:source
                               :name "Alpha"
                               :filter nil
                               :filter-preprocessor #'prompter:filter-exact-match
                               :actions-on-return alpha-action
                               :constructor '("a1" "a2" "a3")))
         (beta (make-instance 'prompter:source
                              :name "Beta"
                              :filter nil
                              :filter-preprocessor #'prompter:filter-exact-match
                              :actions-on-return beta-action
                              :constructor '("b1" "b2" "b3")))
         (sources (nyxt::add-composite-source (list alpha beta)))
         (composite (first sources)))
    (assert-equal 1 (prompter:suggestions-per-source composite))
    (setf (prompter:suggestions-per-source composite) 2)
    (let ((prompt (prompter:make :sources sources)))
      (unwind-protect
           (progn
             (assert-eq composite (first (prompter:sources prompt)))
             (assert-true (bordeaux-threads:with-timeout (5)
                            (prompter:all-ready-p prompt)))
             (assert-equal '("a1" "a2" "b1" "b2")
                           (mapcar #'prompter:value (prompter:suggestions composite)))
             (assert-equal '("Alpha" "Alpha" "Beta" "Beta")
                           (mapcar (lambda (suggestion)
                                     (prompter:name (prompter:match-data suggestion)))
                                   (prompter:suggestions composite)))
             (setf (prompter::current-suggestion prompt) (list composite 0))
             (assert-eq alpha-action (first (prompter:actions-on-return prompt)))
             (setf (prompter::current-suggestion prompt) (list composite 2))
             (assert-eq beta-action (first (prompter:actions-on-return prompt)))
             (setf (prompter:input prompt) "a1")
             (assert-true (bordeaux-threads:with-timeout (5)
                            (prompter:all-ready-p prompt)))
             (assert-equal '("a1")
                           (mapcar #'prompter:value (prompter:suggestions composite)))
             (setf (prompter:input prompt) "")
             (assert-true (bordeaux-threads:with-timeout (5)
                            (prompter:all-ready-p prompt)))
             (assert-equal '("a1" "a2" "b1" "b2")
                           (mapcar #'prompter:value (prompter:suggestions composite))))
        (prompter:destroy prompt)))))
