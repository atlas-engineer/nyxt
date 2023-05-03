;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)
(uiop:define-package :theme/tests
  (:use :cl :lisp-unit2)
  (:import-from :theme))
(in-package :theme/tests)

(defvar *theme* (make-instance 'theme:theme
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

(defmethod assert-contrast ((theme theme:theme)
                            &key (min-color-contrast 7.0) (min-color-alt-contrast 4.5))
  (flet ((assert-contrast-ratio (color1 color2 min-contrast)
           (assert-true (>= (theme:contrast-ratio color1 color2)
                            min-contrast))))
    (multiple-value-bind (alt-colors colors)
        (theme:filter-palette (lambda (color) (str:contains? "-ALT-" color))
                              (theme:palette theme))
      (multiple-value-bind (on-colors colors)
          (theme:filter-palette (lambda (color) (str:starts-with? "ON-" color))
                                colors)
        (loop for color in colors
              for on-color in on-colors
              do (assert-contrast-ratio (funcall color theme)
                                        (funcall on-color theme)
                                        min-color-contrast)))
      (multiple-value-bind (on-alt-colors alt-colors)
          (theme:filter-palette (lambda (color) (str:starts-with? "ON-" color))
                                alt-colors)
        (loop for alt-color in alt-colors
              for on-alt-color in on-alt-colors
              do (assert-contrast-ratio (funcall alt-color theme)
                                        (funcall on-alt-color theme)
                                        min-color-alt-contrast))))))

(define-test default-light-theme-contrast ()
  (assert-contrast theme:+light-theme+))

(define-test default-dark-theme-contrast ()
  (assert-contrast theme:+dark-theme+))
