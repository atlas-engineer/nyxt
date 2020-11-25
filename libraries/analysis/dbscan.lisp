;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

;;; dbscan.lisp -- implementation of Density-based spatial clustering
;;; of applications with noise (DBSCAN) algorithm

;; Vector distance

(defmethod distance ((vector-1 t) (vector-2 t))
  "Calculate the euclidean distance between two vectors."
  (sqrt (loop for i across vector-1
              for j across vector-2
              sum (expt (- i j) 2))))
