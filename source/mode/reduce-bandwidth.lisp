;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/reduce-bandwidth-mode
    (:documentation "Reduce bandwidth."))
(in-package :nyxt/reduce-bandwidth-mode)

(define-mode reduce-bandwidth-mode (nyxt/no-image-mode:no-image-mode
                                    nyxt/no-script-mode:no-script-mode
                                    nyxt/no-webgl-mode:no-webgl-mode)
  "Reduce bandwidth enabling `no-image-mode', `no-script-mode', and
`no-webgl-mode'.")
