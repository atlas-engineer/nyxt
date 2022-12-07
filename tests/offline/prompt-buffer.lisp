;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(unless lparallel:*kernel* (setf lparallel:*kernel*
                                 (lparallel:make-kernel (or (serapeum:count-cpus) 1))))

(define-class twice-source (prompter:source)
  ((prompter:name "Twice-repeated suggestions")))

(defmethod prompter:object-attributes ((object string) (source twice-source))
  `(("Once" ,object)
    ("Twice" ,(str:concat object object))))

(define-test smart-attribute-widths ()
  (assert-equal
   (list (round (/ 100 3))
         (round (* 2 (/ 100 3))))
   (nyxt::attribute-widths
    (make-instance 'twice-source
                   :constructor (mapcar (curry #'format nil "~R")
                                        (alex:iota 100))))))
