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
