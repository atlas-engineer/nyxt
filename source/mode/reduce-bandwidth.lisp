;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/reduce-bandwidth-mode
    (:use :common-lisp :nyxt)
  (:documentation "Reduce bandwidth."))
(in-package :nyxt/reduce-bandwidth-mode)

(define-mode reduce-bandwidth-mode ()
  "Reduce bandwidth enabling `no-image-mode', `no-script-mode', and
`no-webgl-mode'."
  ((constructor
    (lambda (mode)
      (nyxt/no-image-mode:no-image-mode
       :activate t
       :buffer (buffer mode))
      (nyxt/no-script-mode:no-script-mode
       :activate t
       :buffer (buffer mode))
      (nyxt/no-webgl-mode:no-webgl-mode
       :activate t
       :buffer (buffer mode))))
   (destructor 
    (lambda (mode)
      (nyxt/no-image-mode:no-image-mode
       :activate nil
       :buffer (buffer mode))
      (nyxt/no-script-mode:no-script-mode
       :activate nil
       :buffer (buffer mode))
      (nyxt/no-webgl-mode:no-webgl-mode
       :activate nil
       :buffer (buffer mode))))))

