;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)
(uiop:define-package :theme/tests
  (:use :cl :lisp-unit2)
  (:import-from :theme))
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
  (assert-string= "a{background-color:black;color:yellow;}"
                              (let ((lass:*pretty* nil))
                                (theme:themed-css *theme*
                                  `(a
                                    :background-color ,theme:background
                                    :color ,theme:primary)))))

(define-test multi-rule/multi-color-substitution ()
  (assert-string= "a{background-color:black;color:yellow;}body{background-color:yellow;color:white;}h1{color:magenta;}"
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
  (assert-string=  "body{background-color:yellow;color:magenta !important;}"
                               (let ((lass:*pretty* nil))
                                 (theme:themed-css *theme*
                                   `(body
                                     :background-color ,theme:primary
                                     :color ,theme:accent "!important")))))

(define-test quasi-quoted-form ()
  (assert-string= "body{color:black;background-color:yellow;}"
                              (let ((lass:*pretty* nil))
                                (theme:themed-css *theme*
                                  `(body
                                    :color ,(if (theme:dark-p theme:theme)
                                                theme:background
                                                theme:on-background)
                                    :background-color ,theme:primary)))))

(defvar *minimum-contrast-ratio* 7.0
  "The minimum contrast ratio required between color and on-color.")

(define-test contrast-ratio-between-color-and-on-color ()
  (flet ((assert-contrast-ratio (hex)
           (assert-true
            ;; This avoids handling the fact that on-colors are set as color
            ;; strings (i.e. "black" and "white")
            (or (> (theme:contrast-ratio hex "ffffff") *minimum-contrast-ratio*)
                (> (theme:contrast-ratio hex "000000") *minimum-contrast-ratio*)))))
    ;; No need to test the ratio between background and on-background since it's
    ;; trivially close to the largest possible value of 21:1.
    (assert-contrast-ratio (theme:primary-color theme:+light-theme+))
    (assert-contrast-ratio (theme:secondary-color theme:+light-theme+))
    (assert-contrast-ratio (theme:accent-color theme:+light-theme+))
    (assert-contrast-ratio (theme:primary-color theme:+dark-theme+))
    (assert-contrast-ratio (theme:secondary-color theme:+dark-theme+))
    (assert-contrast-ratio (theme:accent-color theme:+dark-theme+))))
