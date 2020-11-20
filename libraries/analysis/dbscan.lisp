;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :text-analysis)

;;; dbscan.lisp -- implementation of Density-based spatial clustering
;;; of applications with noise (DBSCAN) algorithm

;; Vector distance

(defmethod distance ((vector-1 t) (vector-2 t))
  "v = (list 1 0 5)
   w = (list 0 2 4)
   d(v, w) = (sqrt (1-0)^2 + (0-2)^2 + (5-4)^2)"
  )
