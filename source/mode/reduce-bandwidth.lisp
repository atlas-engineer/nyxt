;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/reduce-bandwidth-mode
    (:documentation "Reduce bandwidth."))
(in-package :nyxt/reduce-bandwidth-mode)

(define-mode reduce-bandwidth-mode ()
  "Reduce bandwidth enabling `no-image-mode', `no-script-mode', and
`no-webgl-mode'.")

(defmethod enable ((mode reduce-bandwidth-mode) &key)
  (enable-modes '(no-image-mode no-script-mode no-webgl-mode)))

(defmethod disable ((mode reduce-bandwidth-mode) &key)
  (disable-modes '(no-image-mode no-script-mode no-webgl-mode)))
