;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Dependencies: Nsymbols, Closer MOP, Swank-MOP---all to list slots.

(defun documentation-coverage (package)
  "Documentation coverage data on PACKAGE.

Returns four values:
- The percentage of documented external symbols.
- The total number of symbols
- The number of documented symbols.
- The number of undocumented symbols."
  (loop for s being the external-symbol in (find-package package)
        for package-slots = (apply #'append
                                             (mapcar
                                              (alexandria:compose #'closer-mop:class-slots
                                                                  #'closer-mop:ensure-finalized
                                                                  #'find-class)
                                              (nsymbols:package-classes package :external)))
        count 1 into total
        if (loop for doc-type in '(function variable structure type
                                   setf compiler-macro method-combination)
                 thereis (or (documentation s doc-type)
                             (and (member s package-slots :key #'closer-mop:slot-definition-name)
                                  (swank-mop:slot-definition-documentation
                                   (find s package-slots :key #'closer-mop:slot-definition-name)))))
          count 1 into documented
        else
          collect s into undocumented
        finally (return (values (if (zerop total)
                                    100
                                    (floor (* 100 documented) total))
                                total documented undocumented))))

(defun pp-documentation-coverage (percentage package undocumented &optional (destination t))
  "Pretty prints documentation coverage data on PACKAGE to DESTINATION.

The PACKAGE parameter is a symbol or a string naming a package.

The PERCENTAGE parameter is the completion percentage of documentation.

The UNDOCUMENTED parameter is a list of undocumented symbols.

DESTINATION may be nil, t, a stream, or a string with a fill pointer.

Searches for function, variable, structure, type, setf,
compiler-macro, and method-combination documentation. Class slot
documentation is ignored. Packages that are marked 100% either have
all their symbols documented or they have no exported symbols."
  (format destination
          "~d% ~a: ~{~a~^ ~}~%"
          percentage
          package
          undocumented))

(defun write-documentation-coverage (packages &optional (destination t))
  "Write a documentation coverage report for the given PACKAGES to DESTINATION.

DESTINATION may be nil, t, a stream, or a string with a fill pointer."
  (loop for (percentage package undocumented)
          in (sort (loop for package in packages
                         for (percentage total documented undocumented)
                           = (multiple-value-list (documentation-coverage package))
                         ;; sort undocumented symbols
                         collect (list percentage
                                       (package-name package)
                                       (sort undocumented #'string<)))
                   #'< :key #'first)      ; sort by percentage
        do (pp-documentation-coverage percentage package undocumented destination)))
