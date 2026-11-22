;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)
(uiop:define-package :theme/tests
  (:use :cl :lisp-unit2)
  (:import-from :theme))
(in-package :theme/tests)

(defvar *theme* (make-instance 'theme:theme
                               :background-color "white"
                               :action-color "#37A8E4")
  "Dummy theme for testing.")

(define-test fallback-colors ()
  (assert-string= "white" (theme:background-color- *theme*))
  (assert-string= "white" (theme:background-color+ *theme*))
  (assert-string= "black" (theme:on-background-color *theme*))
  (assert-false (theme:primary-color+ *theme*))
  (assert-false (theme:primary-color *theme*))
  (assert-false (theme:primary-color- *theme*))
  (assert-false (theme:on-primary-color *theme*)))

(define-test css-substitution ()
  (assert-string= "a{background-color:white;color:black;}h1{color:#37A8E4 !important;}"
                  (let ((lass:*pretty* nil))
                    (theme:themed-css *theme*
                      `(a
                        :background-color ,theme:background-color
                        :color ,theme:on-background-color)
                      `(h1
                        :color ,theme:action-color "!important")))))

(define-test logo-color ()
  (assert-string= "#000000" (theme:logo-color theme:+light-theme+))
  (assert-string= "#FFFFFF" (theme:on-logo-color theme:+light-theme+))
  (assert-string= "#88C0D0" (theme:logo-color theme:+dark-theme+))
  (assert-string= "#FFFFFF" (theme:on-logo-color theme:+dark-theme+))
  (let ((light (make-instance 'theme:theme :background-color "white"))
        (dark (make-instance 'theme:theme
                             :background-color "#2E3440"
                             :action-color "#88C0D0")))
    (assert-string= "black" (theme:logo-color light))
    (assert-string= "#FFFFFF" (theme:on-logo-color light))
    (assert-string= "#88C0D0" (theme:logo-color dark))
    (assert-string= "#FFFFFF" (theme:on-logo-color dark))))

(define-test button-color ()
  (assert-string= "#F1F1F1" (theme:button-color theme:+light-theme+))
  (assert-string= "#E6E6E6" (theme:button-color- theme:+light-theme+))
  (assert-string= "#434C5E" (theme:button-color theme:+dark-theme+))
  (assert-string= "#3B4252" (theme:button-color- theme:+dark-theme+))
  (let ((light (make-instance 'theme:theme :background-color "white"
                                           :background-color- "#ECECEC")))
    (assert-string= "#ECECEC" (theme:button-color light))
    (assert-string= "#ECECEC" (theme:button-color- light))))

(define-test surface-color ()
  (assert-string= "color-mix(in srgb, #ECECEC 88%, transparent)"
                  (theme:surface-color theme:+light-theme+))
  (assert-string= "color-mix(in srgb, #3B4252 88%, transparent)"
                  (theme:surface-color theme:+dark-theme+))
  (let ((theme (make-instance 'theme:theme
                              :background-color "white"
                              :surface-color "#FFFFFF")))
    (assert-string= "#FFFFFF" (theme:surface-color theme))))

(define-test heading-color ()
  (assert-string= "#444444" (theme:heading-color theme:+light-theme+))
  (assert-string= "#5E81AC" (theme:heading-color theme:+dark-theme+))
  (assert-true (>= (theme:contrast-ratio
                    (theme:heading-color theme:+light-theme+)
                    (theme:background-color theme:+light-theme+))
                   4.5))
  (let ((theme (make-instance 'theme:theme
                              :background-color "white"
                              :primary-color "#686868"
                              :primary-color- "#555555")))
    (assert-string= "#555555" (theme:heading-color theme))))
