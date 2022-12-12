;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(unless lparallel:*kernel* (setf lparallel:*kernel*
                                 (lparallel:make-kernel (or (serapeum:count-cpus) 1))))

(define-class test-source (prompter:source)
  ;; The number of suggestions is only relevant to access the algorithm's
  ;; performance, so it suffices to use a single suggestion for general testing.
  ((prompter:name "Test")
   (prompter:constructor '("foo1"))))

(defun set-test-source-object-attributes (lst)
  "Set `prompter:object-attributes' following LST properties:
Its length LST dictates the number of attributes.
Each element dictates the number of times a suggestion is concatenated."
  (defmethod prompter:object-attributes ((object string) (source test-source))
    (do ((index 0 (1+ index))
         (result '() (cons `(,(str:concat "foo" (princ-to-string index))
                             ,(str:repeat (nth index lst) object))
                           result)))
        ((eq index (length lst)) (nreverse result)))))

;; TODO Add to lisp-unit2.
(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(define-test constant-size-attributes ()
  (let ((n-attributes (random-in-range 1 10)))
    ;; How to shup up the fact that the method is being redefined?
    (set-test-source-object-attributes (make-list n-attributes :initial-element 1))
    (assert-equal (make-list n-attributes :initial-element (/ 1 n-attributes))
                  (nyxt::attribute-widths (make-instance 'test-source)))))

(define-test ratio-between-two-attributes ()
  (let ((ratio (random-in-range 1 4)))
    (set-test-source-object-attributes `(1 ,ratio))
    (assert-equal (list (/ 1 (1+ ratio)) (/ ratio (1+ ratio)))
                  (nyxt::attribute-widths (make-instance 'test-source)))
    ;; Test symmetry.
    (set-test-source-object-attributes `(,ratio 1))
    (assert-equal (list (/ ratio (1+ ratio)) (/ 1 (1+ ratio)))
                  (nyxt::attribute-widths (make-instance 'test-source)))))
