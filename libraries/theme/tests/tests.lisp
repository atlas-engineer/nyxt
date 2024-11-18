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

(defmethod assert-contrast ((theme theme:theme)
                            &key (min-color+-contrast 8.5)
                              (min-color-contrast 6.5)
                              (min-color--contrast 4.5))
  (macrolet ((assert-contrast-ratio (color1 color2 min-contrast)
               `(assert-true (>= (theme:contrast-ratio ,color1 ,color2)
                                 ,min-contrast))))
    (multiple-value-bind (on-colors regular-colors minus-colors plus-colors)
        (values-list
         (theme:filter-palette (list (alexandria:curry #'uiop:string-prefix-p "ON-")
                                     (alexandria:rcurry #'uiop:string-suffix-p "COLOR")
                                     (alexandria:rcurry #'uiop:string-suffix-p "COLOR-")
                                     (alexandria:rcurry #'uiop:string-suffix-p "COLOR+"))
                               (theme:palette theme)))
      (loop for on-color in on-colors
            for regular-color in regular-colors
            for minus-color in minus-colors
            for plus-color in plus-colors
            do (assert-contrast-ratio (funcall regular-color theme)
                                      (funcall on-color theme)
                                      min-color-contrast)
            do (assert-contrast-ratio (funcall plus-color theme)
                                      (funcall on-color theme)
                                      min-color+-contrast)
            do (assert-contrast-ratio (funcall minus-color theme)
                                      (funcall on-color theme)
                                      min-color--contrast)))))

(define-test default-light-theme-contrast ()
  (assert-contrast theme:+light-theme+))

(define-test default-dark-theme-contrast ()
  (assert-contrast theme:+dark-theme+))
