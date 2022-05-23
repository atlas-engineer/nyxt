;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme)

(defvar theme nil
  "A dynamic variable to bind to current `theme' in `themed-css'.")

(defvar background nil
  "A dynamic variable to bind to `background-color' of the current `theme' in `themed-css'.")

(defvar text nil
  "A dynamic variable to bind to `text-color' of the current `theme' in `themed-css'.")

(defvar primary nil
  "A dynamic variable to bind to `primary-color' of the current `theme' in `themed-css'.")

(defvar secondary nil
  "A dynamic variable to bind to `secondary-color' of the current `theme' in `themed-css'.")

(defvar tertiary nil
  "A dynamic variable to bind to `tertiary-color' of the current `theme' in `themed-css'.")

(defvar quaternary nil
  "A dynamic variable to bind to `quaternary-color' of the current `theme' in `themed-css'.")

(defvar accent nil
  "A dynamic variable to bind to `accent-color' of the current `theme' in `themed-css'.")

(defvar font-family nil
  "A dynamic variable to bind to `font-family' of the current `theme' in `themed-css'.")

(define-class theme ()
  ((dark-p
    nil
    :documentation "Whether the theme is dark.")
   (background-color
    "#ffffff"
    :type string
    :documentation "The background color of the theme.")
   (text-color
    "#000000"
    :type string
    :documentation "The main color of the text in the theme.
Should contrast with the `background-color'.")
   (primary-color
    "#555555"
    :type string
    :documentation "The main non-text/interface color.
Should preferably contrast both with `background-color' and `text-color'.")
   (secondary-color
    "#737373"
    :type string
    :documentation "The secondary interface color.
Should contrast with `background-color'.")
   (tertiary-color
    "#8C8C8C"
    :type string
    :documentation "The tertiary interface color.
Should contrast with `text-color'.")
   (quaternary-color
    "#E6E6E6"
    :type string
    :documentation "The quaternary color.
Should strongly contrast with `text-color'.")
   (accent-color
    "#37a8e4"
    :type string
    :documentation "The color of the highlighted elements that need attention.
Should contrast with every other color in the theme.")
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
  (make-instance
           'theme
           :dark-p t
           :background-color "black"
           :text-color "white"
           :accent-color "#FCBA04"
           :primary-color "#AD693E"
           :secondary-color "#DB9665"
           :tertiary-color "#A45C30"
           :quaternary-color "#7D3509"))

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

(defmacro with-theme (theme &body body)
  "Evaluate body with the theme bindings available.

The bindings are:
- `theme:theme' -- THEME itself.
- `theme:background' -- background color of the THEME.
- `theme:text' -- text color of the THEME.
- `theme:primary' -- primary color of the THEME.
- `theme:secondary' -- secondary color of the THEME.
- `theme:tertiary' -- tertiary color of the THEME.
- `theme:accent' -- accent color of the THEME.
- `theme:font' -- font family of the theme."
  `(let* ((theme:theme ,theme)
          (theme:background (background-color theme:theme))
          (theme:text (text-color theme:theme))
          (theme:primary (primary-color theme:theme))
          (theme:secondary (secondary-color theme:theme))
          (theme:tertiary (tertiary-color theme:theme))
          (theme:quaternary (quaternary-color theme:theme))
          (theme:accent (accent-color theme:theme))
          (theme:font-family (font-family theme:theme)))
     ,@body))

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
  `(with-theme ,theme
    (cl-css:css
     (list ,@(mapcar #'requote-rule rules)))))
