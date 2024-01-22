;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(benchmark:define-benchmark-package nyxt/benchmarks
  (:import-from :nyxt))

(unless lparallel:*kernel* (setf lparallel:*kernel*
                                 (lparallel:make-kernel (or (serapeum:count-cpus) 1))))
