;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme)

;; TODO It would be possible to set all of the "on-" colors based on their
;; counterparts by computing the contrast ratio.
;; See https://www.w3.org/TR/WCAG20-TECHS/G18.html.

(define-class theme ()
  ((dark-p
    nil
    :documentation "Whether the theme is dark.")
   (background-color
    "white"
    :type string
    :documentation "The background color of the theme.")
   (on-background-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of
`background-color'.  Must contrast with `background-color'.")
   (primary-color
    "#555555"
    :type string
    :documentation "One of the colors applied to surfaces.  Should preferably be
neutral.")
   (on-primary-color
    "white"
    :type string
    :documentation "The color applied to elements appearing in front of
`primary-color'.  Must contrast with `primary'.")
   (secondary-color
    "#A6A6A6"
    :type string
    :documentation "One of the colors applied to surfaces.  Should preferably be
neutral.  Must be chosen such that `on-secondary-color' and `on-primary-color'
are not the same.")
   (on-secondary-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of
`secondary-color'.  Must contrast with `secondary'.")
   (accent-color
    "#37A8E4"
    :type string
    :documentation "The color applied to distinguished elements.  Should stand
out from all of the other theme colors.")
   (on-accent-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of `accent'.
Must contrast with `accent'.")
   (font-family
    "Helvetica Neue, Helvetica"
    :type string
    :documentation "The font family to use by default."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defvar +light-theme+
  (make-instance 'theme))

(defvar +dark-theme+
  (make-instance 'theme
                 :dark-p t
                 :background-color "black"
                 :on-background-color "white"
                 :primary-color "#753C17"
                 :on-primary-color "white"
                 :secondary-color "#D88A52"
                 :on-secondary-color "black"
                 :accent-color "#C69203"
                 :on-accent-color "black"))

(defvar theme nil
  "Dynamic variable that binds `theme' in `themed-css'.")
(defvar background nil
  "Dynamic variable that binds `background-color' of `theme' in `themed-css'.")
(defvar on-background nil
  "Dynamic variable that binds `on-background-color' of `theme' in `themed-css'.")
(defvar primary nil
  "Dynamic variable that binds `primary-color' of `theme' in `themed-css'.")
(defvar on-primary nil
  "Dynamic variable that binds `on-primary-color' of `theme' in `themed-css'.")
(defvar secondary nil
  "Dynamic variable that binds `secondary-color' of `theme' in `themed-css'.")
(defvar on-secondary nil
  "Dynamic variable that binds `on-secondary-color' of `theme' in `themed-css'.")
(defvar accent nil
  "Dynamic variable that binds `accent-color' of `theme' in `themed-css'.")
(defvar on-accent nil
  "Dynamic variable that binds `on-accent-color' of `theme' in `themed-css'.")
(defvar font-family nil
  "Dynamic variable that binds `font-family' of `theme' in `themed-css'.")

(defmacro with-theme (theme &body body)
  "Evaluate body with the theme bindings available."
  `(let* ((theme:theme ,theme)
          (theme:background (background-color theme:theme))
          (theme:on-background (on-background-color theme:theme))
          (theme:primary (primary-color theme:theme))
          (theme:on-primary (on-primary-color theme:theme))
          (theme:secondary (secondary-color theme:theme))
          (theme:on-secondary (on-secondary-color theme:theme))
          (theme:accent (accent-color theme:theme))
          (theme:on-accent (on-accent-color theme:theme))
          (theme:font-family (font-family theme:theme)))
     ,@body))

(defun plist-p (object)
  "Return non-nil if OBJECT is a plist."
  (and (listp object)
       (alexandria:proper-list-p object)
       (evenp (length object))
       (loop :for x :in object :by #'cddr
             :always (keywordp x))))

(defun requote-rule (rule)
  (if (plist-p (rest rule))
      (cons 'list (mapcar (lambda (elem)
                            (typecase elem
                              (symbol (if (equalp "theme" (package-name (symbol-package elem)))
                                          elem
                                          `(quote ,elem)))
                              (atom (if (constantp elem)
                                        elem `(quote ,elem)))
                              (list elem)
                              (t elem)))
                          rule))
      (cons 'list (mapcar (lambda (x) `(quote ,x)) rule))))

(defmacro themed-css (theme &body rules)
  "Generate a CSS styled according to the THEME.

RULES is a list of CL-CSS rules (with one level of nesting removed). There are
special symbols that this macro will substitute for theme elements, if
encountered. See `with-theme' for a list of those.

Any non-atomic s-expression put as value of CSS property will be
evaluated too, so you can put arbitrarily complex expressions as
property values.

Example: color all the paragraph text in accent color if the theme is dark, and
in secondary color otherwise. Use the text color as background color. Make
headings have border of secondary color.

\(themed-css (make-instance 'theme
                            :dark-p t
                            :on-background-color \"red\"
                            :accent-color \"blue\")
           (|h1,h2,h3,h4,h5,h6|
            :border-style \"solid\"
            :border-width \"1px\"
            :border-color theme:secondary)
           (p
            :color (if (theme:dark-p theme:theme) theme:accent theme:secondary)
            :background-color theme:background))"
  `(with-theme ,theme
    (cl-css:css
     (list ,@(mapcar #'requote-rule rules)))))
