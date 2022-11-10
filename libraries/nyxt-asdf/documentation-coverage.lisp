;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

(defun documentation-coverage (package)
  "Documentation coverage data on PACKAGE.

# Return value

A property list with keys

- :TOTAL, the number of external symbols,
- :DOCUMENTED, the number of documented external symbols,
- :UNDOCUMENTED, the list of undocumented external symbols."
  (let ((total 0)
        (documented 0)
        (undocumented '()))
    (do-external-symbols
        (s (find-package package)
           `(:total ,total
             :documented ,documented
             :undocumented ,undocumented))
      (incf total)
      (if (loop for doc-type in '(function variable structure type
                                  setf compiler-macro method-combination)
                when (documentation s doc-type) return t)
          (incf documented)
          (push s undocumented)))))

(defun pp-documentation-coverage (percentage package undocumented &optional (destination t))
  "Pretty prints documentation coverage data on PACKAGE to DESTINATION.

The PACKAGE parameter is a symbol or a string naming a package.

The PERCENTAGE parameter is the completion percentage of documentation.

The UNDOCUMENTED parameter is a list of undocumented symbols.

DESTINATION may be nil, t, a stream, or a string with a fill pointer."
  (format destination
          "~3D% ~A: ~{~A~^ ~}~%"
          percentage
          package
          undocumented))

(export-always 'write-documentation-coverage)
(defun write-documentation-coverage (packages &optional (destination t))
  "Write a documentation coverage report for the given PACKAGES to DESTINATION.

DESTINATION may be nil, t, a stream, or a string with a fill pointer."
  (loop for (percentage package undocumented)
          in (sort (loop for package in packages
                         for coverage = (documentation-coverage package)
                         for documented = (getf coverage :documented)
                         for total = (getf coverage :total)
                         for undocumented = (getf coverage :undocumented)
                         for percentage = (if (zerop total) 100 (floor (* 100 documented) total))
                         ;; sort undocumented symbols
                         collect (list percentage
                                       (package-name package)
                                       (sort undocumented #'string<)))
                   #'< :key #'car)      ; sort by percentage
        do (pp-documentation-coverage percentage package undocumented destination)))
