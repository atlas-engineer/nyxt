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

(defun set-test-source-object-attributes (length)
  "Set `prompter:object-attributes' following LENGTH.
There are as many attributes as LENGTH, and each one is increasingly long."
  (defmethod prompter:object-attributes ((object string) (source test-source))
    (loop for i from 1 upto length
          collect (list (str:concat "Attribute" (princ-to-string i)) (str:repeat i object) nil i))))

(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(define-test constant-size-attributes ()
  (let ((n-attributes (random-in-range 1 10)))
    (handler-bind ((warning #'muffle-warning))
      (set-test-source-object-attributes n-attributes))
    (assert-equal (loop for i from 1 upto n-attributes
                        sum i into total
                        collect i into widths
                        finally (return (mapcar (rcurry #'/ total) widths)))
                  (nyxt::attribute-widths (make-instance 'test-source)))))

(define-test dynamic-widths ()
  (set-test-source-object-attributes 2)
  (assert-equal
   (list 1/3 2/3)
   (nyxt::attribute-widths (make-instance 'test-source) :dynamic-p t))
  (set-test-source-object-attributes 3)
  (assert-equal
   (list 1/6 2/6 3/6)
   (nyxt::attribute-widths (make-instance 'test-source) :dynamic-p t))
  (set-test-source-object-attributes 4)
  (assert-equal
   (list 1/8 39/200 117/400 39/100)
   (nyxt::attribute-widths (make-instance 'test-source) :dynamic-p t)))
