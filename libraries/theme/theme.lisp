;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme)

(defvar theme nil
  "A dynamic variable to bind to current `theme' in `themed-css'.")

(defvar background nil
  "A dynamic variable to bind `backround-color' of the current `theme' to in `themed-css'.")

(defvar text nil
  "A dynamic variable to bind `text-color' of the current `theme' to in `themed-css'.")

(defvar primary nil
  "A dynamic variable to bind `primary-color' of the current `theme' to in `themed-css'.")

(defvar secondary nil
  "A dynamic variable to bind `secondary-color' of the current `theme' to in `themed-css'.")

(defvar secondary nil
  "A dynamic variable to bind `secondary-color' of the current `theme' to in `themed-css'.")

(defvar tertiary nil
  "A dynamic variable to bind `tertiary-color' of the current `theme' to in `themed-css'.")

(defvar quaternary nil
  "A dynamic variable to bind `quaternary-color' of the current `theme' to in `themed-css'.")

(defvar accent nil
  "A dynamic variable to bind `accent-color' of the current `theme' to in `themed-css'.")

(defvar font-family nil
  "A dynamic variable to bind `font-family' of the current `theme' to in `themed-css'.")

(define-class theme ()
  ((dark-p
    nil
    :documentation "Whether the theme is dark.")
   (background-color
    "white"
    :type string
    :documentation "The background color of the theme")
   (text-color
    "black"
    :type string
    :documentation "The main color of the text in the theme.
Should contrast with the `background-color'")
   (primary-color
    "gray"
    :type string
    :documentation "The main non-text/interface color.
Should preferably contrast both `background-color' and `text-color'.")
   (secondary-color
    "lightgray"
    :type string
    :documentation "The secondary interface color.
Should contrast with the `text-color'.")
   (tertiary-color
    "darkgray"
    :type string
    :documentation "The tertiary interface color.
Should contrast with the `background-color'.")
   (quaternary-color
    "gainsboro"
    :type string
    :documentation "The quaternary color.
Should strongly contrast with the `text-color'.")
   (accent-color
    "#37a8e4"
    :type string
    :documentation "The color of the accented elements that need attention.
Should contrast with every other color in the theme.")
   (font-family
    "Helvetica Neue, Helvetica"
    :type string
    :documentation "The font family to use by default."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defun regular-css-rule-p (rule)
  (loop for i below (length (rest rule))
        for elem in (rest rule)
        when (and (evenp i) (not (keywordp elem)))
          do (return nil)
        finally (return t)))

(defun requote-rule (rule)
  (if (regular-css-rule-p rule)
      (cons 'list (mapcar (lambda (elem)
                            (typecase elem
                              (symbol (if (equalp "theme" (package-name (symbol-package elem)))
                                          elem `(quote ,elem)))
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
encountered:
- `theme:theme' -- THEME itself.
- `theme:background' -- background color of the THEME.
- `theme:text' -- text color of the THEME.
- `theme:primary' -- primary color of the THEME.
- `theme:secondary' -- secondary color of the THEME.
- `theme:tertiary' -- tertiary color of the THEME.
- `theme:accent' -- accent color of the THEME.
- `theme:font' -- font family of the theme.

Any non-atomic s-expression will be evaluated too, so you can put
arbitrarily complex expressions as property values.

Example: color all the paragraph text in accent color if the theme is dark, and
in secondary color otherwise. Use the text color as background color. Make
headings have border of tertiary color.

\(themed-css (make-instance 'theme
                            :dark-p t
                            :text-color \"red\"
                            :accent-color \"blue\")
           (|h1,h2,h3,h4,h5,h6|
            :border-style \"solid\"
            :border-width \"1px\"
            :border-color theme:tertiary)
           (p
            :color (if (theme:dark-p theme:theme) theme:accent theme:secondary)
            :background-color theme:text))"
  `(let* ((theme:theme ,theme)
          (theme:background (background-color theme:theme))
          (theme:text (text-color theme:theme))
          (theme:primary (primary-color theme:theme))
          (theme:secondary (secondary-color theme:theme))
          (theme:tertiary (tertiary-color theme:theme))
          (theme:quaternary (quaternary-color theme:theme))
          (theme:accent (accent-color theme:theme))
          (theme:font-family (font-family theme:theme)))
     (cl-css:css
      (list ,@(mapcar #'requote-rule rules)))))
