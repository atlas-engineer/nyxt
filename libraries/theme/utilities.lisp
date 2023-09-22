;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme)

(serapeum:-> relative-luminance ((or string integer cl-colors2:rgb cl-colors2:hsv))
             real) ;; What's the range?
(defun relative-luminance (color)
  "Compute relative luminance of COLOR."
  ;; See https://www.w3.org/TR/WCAG20/#relativeluminancedef
  (loop with rgb = (cl-colors2:as-rgb color)
        for const in '(0.2126 0.7152 0.0722)
        for rgb-component in (list (cl-colors2:rgb-red (cl-colors2:as-rgb color))
                                   (cl-colors2:rgb-green (cl-colors2:as-rgb color))
                                   (cl-colors2:rgb-blue (cl-colors2:as-rgb color)))
        sum (* const (if (<= rgb-component 0.03928)
                         (/ rgb-component 12.92)
                         (expt (/ (+ rgb-component 0.055) 1.055) 2.4)))))

(serapeum:-> contrast-ratio ((or string integer cl-colors2:rgb cl-colors2:hsv)
                             (or string integer cl-colors2:rgb cl-colors2:hsv))
             ;; 21 is the ratio between black and white.
             (real 0 21))
(export-always 'contrast-ratio)
(defun contrast-ratio (color1 color2)
  "Compute contrast ratio between COLOR1 and COLOR2."
  (let ((ratio (/ (+ (relative-luminance color1) 0.05)
                  (+ (relative-luminance color2) 0.05))))
    (max ratio (/ ratio))))

(serapeum:-> contrasting-color ((or string integer cl-colors2:rgb cl-colors2:hsv)) string)
(export-always 'contrasting-color)
(defun contrasting-color (color)
  "Determine whether black or white best contrasts with COLOR."
  (if (>= (contrast-ratio color "white")
          (contrast-ratio color "black"))
      "white"
      "black"))
