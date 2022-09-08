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
  (assert-string= "a { background-color: black; color: yellow; }
"
                  (theme:themed-css *theme*
                    (a
                     :background-color theme:background
                     :color theme:primary))))

(define-test multi-rule/multi-color-substitution ()
  (assert-string= "a { background-color: black; color: yellow; }
body { background-color: yellow; color: white; }
h1 { color: magenta; }
"
                  (theme:themed-css *theme*
                    (a
                     :background-color theme:background
                     :color theme:primary)
                    (body
                     :background-color theme:primary
                     :color theme:on-background)
                    (h1
                     :color theme:accent))))

(define-test inline-function-execution ()
  (assert-string=  "body { background-color: yellow; color: magenta !important; }
"
                   (theme:themed-css *theme*
                     (body
                      :background-color theme:primary
                      :color (concatenate 'string theme:accent " !important")))))

(define-test inline-macro/special-form-invocation ()
  (assert-string= "body { color: black; background-color: yellow; }
"
                  (theme:themed-css *theme*
                    (body
                     :color (if (theme:dark-p theme:theme)
                                theme:background
                                theme:on-background)
                     :background-color theme:primary))))
