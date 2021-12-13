;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/no-search-completion-mode
    (:use :common-lisp :nyxt)
  (:documentation "Disable search engine autocompletion."))
(in-package :nyxt/no-search-completion-mode)

(define-mode no-search-completion-mode ()
  "Disable search engine autocompletion in current buffer."
  ((destructor
    (lambda (mode)
      (setf (search-always-auto-complete-p (buffer mode)) t)))
   (constructor
    (lambda (mode)
      (setf (search-always-auto-complete-p (buffer mode)) nil)))))
