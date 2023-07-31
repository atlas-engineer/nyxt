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
                               :on-action-color "black")
  "Dummy theme for testing.")

(defvar *contrast-test-theme*
  (make-instance 'theme:theme
                 :text-color "#0c0c0d"
                 :contrast-text-color "#f8f8f8"
                 :background-color "#f8f8f8"
                 :primary-color "dimgray"
                 :secondary-color "gray")
  "Theme to test text and +/- colors contrast.")

(define-test auto-generated-text-color ()
  (assert-equal "#0c0c0d" (theme:on-background-color *contrast-test-theme*))
  (assert-equal "#f8f8f8" (theme:on-primary-color *contrast-test-theme*))
  (assert-equal "#0c0c0d" (theme:on-secondary-color *contrast-test-theme*)))

(define-test auto-generated-plus-minus-colors ()
  (let ((theme *contrast-test-theme*))
    (assert-true (theme:background-color+ theme))
    (assert-true (< (theme::relative-luminance (theme:background-color theme))
                    (theme::relative-luminance (theme:background-color+ theme))))
    (assert-true (theme:background-color- theme))
    (assert-true (> (theme::relative-luminance (theme:background-color theme))
                    (theme::relative-luminance (theme:background-color- theme))))
    (assert-true (theme:text-color+ theme))
    (assert-true (> (theme::relative-luminance (theme:text-color theme))
                    (theme::relative-luminance (theme:text-color+ theme))))
    (assert-true (theme:text-color- theme))
    (assert-true (< (theme::relative-luminance (theme:text-color theme))
                    (theme::relative-luminance (theme:text-color- theme))))))

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
                            &key (min-plus-color-contrast 8.5)
                              (min-color-contrast 6.5)
                              (min-minus-color-contrast 4.5))
  (macrolet ((assert-contrast-ratio (color1 color2 min-contrast)
               `(assert-true (>= (theme:contrast-ratio ,color1 ,color2)
                                 ,min-contrast))))
    ;; Colors and their on-colors
    (assert-contrast-ratio (theme:background-color theme) (theme:on-background-color theme) min-color-contrast)
    (assert-contrast-ratio (theme:primary-color theme) (theme:on-primary-color theme) min-color-contrast)
    (assert-contrast-ratio (theme:secondary-color theme) (theme:on-secondary-color theme) min-color-contrast)
    (assert-contrast-ratio (theme:action-color theme) (theme:on-action-color theme) min-color-contrast)
    (assert-contrast-ratio (theme:highlight-color theme) (theme:on-highlight-color theme) min-color-contrast)
    (assert-contrast-ratio (theme:success-color theme) (theme:on-success-color theme) min-color-contrast)
    (assert-contrast-ratio (theme:warning-color theme) (theme:on-warning-color theme) min-color-contrast)
    (assert-contrast-ratio (theme:codeblock-color theme) (theme:on-codeblock-color theme) min-color-contrast)
    ;; More contrasting colors.
    (assert-contrast-ratio (theme:background-color+ theme) (theme:on-background-color theme) min-plus-color-contrast)
    (assert-contrast-ratio (theme:primary-color+ theme) (theme:on-primary-color theme) min-plus-color-contrast)
    (assert-contrast-ratio (theme:secondary-color+ theme) (theme:on-secondary-color theme) min-plus-color-contrast)
    (assert-contrast-ratio (theme:action-color+ theme) (theme:on-action-color theme) min-plus-color-contrast)
    (assert-contrast-ratio (theme:highlight-color+ theme) (theme:on-highlight-color theme) min-plus-color-contrast)
    (assert-contrast-ratio (theme:success-color+ theme) (theme:on-success-color theme) min-plus-color-contrast)
    (assert-contrast-ratio (theme:warning-color+ theme) (theme:on-warning-color theme) min-plus-color-contrast)
    (assert-contrast-ratio (theme:codeblock-color+ theme) (theme:on-codeblock-color theme) min-plus-color-contrast)
    ;; Less contrasting colors.
    (assert-contrast-ratio (theme:background-color- theme) (theme:on-background-color theme) min-minus-color-contrast)
    (assert-contrast-ratio (theme:primary-color- theme) (theme:on-primary-color theme) min-minus-color-contrast)
    (assert-contrast-ratio (theme:secondary-color- theme) (theme:on-secondary-color theme) min-minus-color-contrast)
    (assert-contrast-ratio (theme:action-color- theme) (theme:on-action-color theme) min-minus-color-contrast)
    (assert-contrast-ratio (theme:highlight-color- theme) (theme:on-highlight-color theme) min-minus-color-contrast)
    (assert-contrast-ratio (theme:success-color- theme) (theme:on-success-color theme) min-minus-color-contrast)
    (assert-contrast-ratio (theme:warning-color- theme) (theme:on-warning-color theme) min-minus-color-contrast)
    (assert-contrast-ratio (theme:codeblock-color- theme) (theme:on-codeblock-color theme) min-minus-color-contrast)))

(define-test default-light-theme-contrast ()
  (assert-contrast theme:+light-theme+))

(define-test default-dark-theme-contrast ()
  (assert-contrast theme:+dark-theme+))

(define-test backwards-compatibility ()
  ;; Deprecated.
  (assert-equal "pink" (theme:action-color (make-instance 'theme:theme :accent-color "pink")))
  ;; Alt colors are minus colors now.
  (assert-equal "#aaaaaa" (theme:background-color- (make-instance 'theme:theme :background-alt-color "#aaaaaa")))
  ;; Ignored.
  (assert-string/= "red" (theme:on-background-color
                          (make-instance 'theme:theme :on-background-alt-color "red")))
  ;; Deprecated AND ignored.
  (assert-string/= "fuchsia"
                   ;; `theme:on-background-color' is not ever related, just a random accessor.
                   (theme:on-background-color
                    (make-instance 'theme:theme :on-background-alt-color "fuchsia"))))
