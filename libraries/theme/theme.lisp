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
`background-color'.  Must strongly contrast with `background-color'.")
   (primary-color
    "#555555"
    :type string
    :documentation "One of the colors applied to surfaces.  Should contrast with
`background-color' and, preferably, be neutral.")
   (on-primary-color
    "white"
    :type string
    :documentation "The color applied to elements appearing in front of
`primary-color'.  Must strongly contrast with `primary-color'.")
   (secondary-color
    "#A6A6A6"
    :type string
    :documentation "One of the colors applied to surfaces.  Should contrast with
`on-background-color' and, preferably, be neutral.")
   (on-secondary-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of
`secondary-color'.  Must strongly contrast with `secondary-color'.")
   (accent-color
    "#37A8E4"
    :type string
    :documentation "The color applied to distinguished elements.  Should stand
out from all of the other theme colors.")
   (on-accent-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of
`accent-color'.  Must strongly contrast with `accent-color'.")
   (font-family
    "Helvetica Neue, Helvetica"
    :type string
    :documentation "The font family to use by default."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always '+light-theme+)
(defvar +light-theme+
  (make-instance 'theme))

(export-always '+dark-theme+)
(defvar +dark-theme+
  (make-instance 'theme
                 :dark-p t
                 :background-color "black"
                 :on-background-color "white"
                 :primary-color "#D88A52"
                 :on-primary-color "black"
                 :secondary-color "#753C17"
                 :on-secondary-color "white"
                 :accent-color "#FCBA04"
                 :on-accent-color "black"))

(export-always '(theme
                 background
                 on-background
                 primary
                 on-primary
                 secondary
                 on-secondary
                 accent
                 on-accent
                 font-family))
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

(export-always 'with-theme)
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

(export-always 'themed-css)
(defmacro themed-css (theme &body blocks)
  "Generate a CSS styled according to the THEME and BLOCKS.

BLOCKS is a list of LASS blocks.

Any LASS-friendly syntax (including quasi-quotes) works, so you can evaluate
arbitrary code before the blocks are compiled to CSS. For the convenience of
quasi-quoted forms, this macro let-binds the set of symbols/slots for THEME
around the BLOCKS.

Example: color all the paragraph text in accent color if the theme is dark, and
in secondary color otherwise. Use the text color as background color. Make
headings have border of secondary color.

\(themed-css (make-instance 'theme
                            :dark-p t
                            :on-background-color \"red\"
                            :accent-color \"blue\")
           `(|h1,h2,h3,h4,h5,h6|
             :border-style \"solid\"
             :border-width \"1px\"
             :border-color ,theme:secondary)
           `(p
             :color ,(if (theme:dark-p theme:theme) theme:accent theme:secondary)
             :background-color ,theme:background))"
  `(with-theme ,theme
     (lass:compile-and-write
      ;; NOTE: This loop allows to omit quotes for most trivial rules,
      ;; mostly preserving backwards-compatibility.
      ,@(loop for block in blocks
              for first = (first block)
              ;; FIXME: This is not perfect, but it's good enough for
              ;; 99% of cases. Maybe somehow parse selectors with LASS?
              if (or (stringp first)
                     (keywordp first)
                     (eq '* first)
                     (and (symbolp first)
                          (eq :internal (nth-value 1 (find-symbol (symbol-name first)
                                                                  (symbol-package first)))))
                     (and (listp first)
                          (keywordp (first first))))
                collect (cons 'quote (list block))
              else collect block))))
