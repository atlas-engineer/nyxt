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
