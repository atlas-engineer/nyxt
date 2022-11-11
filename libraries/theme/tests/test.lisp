;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)
(uiop:define-package :theme/tests
  (:use #:common-lisp #:lisp-unit2)
  (:import-from #:theme))
(in-package :theme/tests)

(defvar *theme* (make-instance 'theme:theme
                               :dark-p t
                               :background-color "black"
                               :on-background-color "white"
                               :primary-color "yellow"
                               :on-primary-color "black"
                               :secondary-color "blue"
                               :on-secondary-color "black"
                               :accent-color "magenta"
                               :on-accent-color "black")
  "Dummy theme for testing.")

(define-test basic-css-substitution ()
  (lisp-unit2::assert-string= "a{background-color:black;color:yellow;}"
                  (let ((lass:*pretty* nil))
                    (theme:themed-css *theme*
                      `(a
                        :background-color ,theme:background
                        :color ,theme:primary)))))

(define-test multi-rule/multi-color-substitution ()
  (lisp-unit2::assert-string= "a{background-color:black;color:yellow;}body{background-color:yellow;color:white;}h1{color:magenta;}"
                  (let ((lass:*pretty* nil))
                    (theme:themed-css *theme*
                      `(a
                       :background-color ,theme:background
                       :color ,theme:primary)
                      `(body
                       :background-color ,theme:primary
                       :color ,theme:on-background)
                      `(h1
                       :color ,theme:accent)))))

(define-test irregular-args ()
  (lisp-unit2::assert-string=  "body{background-color:yellow;color:magenta !important;}"
                   (let ((lass:*pretty* nil))
                     (theme:themed-css *theme*
                       `(body
                         :background-color ,theme:primary
                         :color ,theme:accent "!important")))))

(define-test quasi-quoted-form ()
  (lisp-unit2::assert-string= "body{color:black;background-color:yellow;}"
                  (let ((lass:*pretty* nil))
                    (theme:themed-css *theme*
                      `(body
                        :color ,(if (theme:dark-p theme:theme)
                                    theme:background
                                    theme:on-background)
                        :background-color ,theme:primary)))))

(defun hex-to-rgb (hex)
  "Convert HEX to an RGB triple."
  (declare (string hex))
  (let ((hex (if (uiop:string-prefix-p "#" hex) (subseq hex 1) hex)))
    (mapcar (lambda (x) (/ x 255.0))
            (list (parse-integer (subseq hex 0 2) :radix 16)
                  (parse-integer (subseq hex 2 4) :radix 16)
                  (parse-integer (subseq hex 4 6) :radix 16)))))

(defun relative-luminance (hex)
  "Compute relative luminance of HEX."
  ;; See https://www.w3.org/TR/WCAG20-TECHS/G18.html
  (loop for const in '(0.2126 0.7152 0.0722)
        for rgb-component in (hex-to-rgb hex)
        sum (* const (if (<= rgb-component 0.03928)
                         (/ rgb-component 12.92)
                         (expt (/ (+ rgb-component 0.055) 1.055) 2.4)))))

(defun contrast-ratio (hex1 hex2)
  "Compute contrast ratio between HEX1 and HEX2."
  (let ((ratio (/ (+ (relative-luminance hex1) 0.05)
                  (+ (relative-luminance hex2) 0.05))))
    (max ratio (/ ratio))))

(defvar *minimum-contrast-ratio* 7.0
  "The minimum contrast ratio required between color and on-color.")

(define-test contrast-ratio-between-color-and-on-color ()
  (flet ((assert-contrast-ratio (hex)
           (assert-true
            ;; This avoids handling the fact that on-colors are set as color
            ;; strings (i.e. "black" and "white")
            (or (> (contrast-ratio hex "ffffff") *minimum-contrast-ratio*)
                (> (contrast-ratio hex "000000") *minimum-contrast-ratio*)))))
    ;; No need to test the ratio between background and on-background since it's
    ;; trivially close to the largest possible value of 21:1.
    (assert-contrast-ratio (theme:primary-color theme:+light-theme+))
    (assert-contrast-ratio (theme:secondary-color theme:+light-theme+))
    (assert-contrast-ratio (theme:accent-color theme:+light-theme+))
    (assert-contrast-ratio (theme:primary-color theme:+dark-theme+))
    (assert-contrast-ratio (theme:secondary-color theme:+dark-theme+))
    (assert-contrast-ratio (theme:accent-color theme:+dark-theme+))))
