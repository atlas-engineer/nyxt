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
   ;; This is because we split 50% of the width between all the attributes
   ;; evenly (thus 25) and then add the average sizes of attributes relative to
   ;; each other (thus 1*(50/3) and 2*(50/3)---the second attribute is twice as
   ;; big as the first one).
   (list (+ 25 (round (/ 50 3)))
         (+ 25 (round (* 2 (/ 50 3)))))
   (nyxt::attribute-widths
    (make-instance 'twice-source
                   :constructor (mapcar (curry #'format nil "~R")
                                        (alex:iota 100))))))
