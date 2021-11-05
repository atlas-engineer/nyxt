;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/reduce-tracking-mode
  (:use :common-lisp :nyxt)
  (:documentation "Mode to mitigate fingerprinting."))
(in-package :nyxt/reduce-tracking-mode)

(define-mode reduce-tracking-mode ()
  "Set specific settings in the web view in order to mitigate fingerprinting,
(how third-party trackers attempt to indentify you.

Fingerprinting can be tested with https://panopticlick.eff.org/."
  ((preferred-languages
    '("en_US")
    :type list-of-strings
    :documentation "The list of languages that will be sent as part of the
Accept-Language HTTP header.")
   (destructor
    (lambda (mode)
      (ffi-set-preferred-languages (buffer mode)
                                   (list (first
                                          (str:split
                                           "."
                                           (or (uiop:getenv "LANG") "")))))))
   (constructor
    (lambda (mode)
      (ffi-set-preferred-languages (buffer mode)
                                   (preferred-languages mode))))))
