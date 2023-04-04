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
   (background-alt-color
    "#eeeeee"
    :type string
    :documentation "A nuanced version of `background-color'.")
   (on-background-alt-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of
`background-alt-color'.  Must strongly contrast with `background-alt-color'.")
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
   (primary-alt-color
    "#686868"
    :type string
    :documentation "A nuanced version of `primary-color'.")
   (on-primary-alt-color
    nil
    :type string
    :documentation "The color applied to elements appearing in front of
`primary-alt-color'.  Must strongly contrast with `primary-alt-color'.")
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
   (secondary-alt-color
    "#909090"
    :type string
    :documentation "A nuanced version of `secondary-color'.")
   (on-secondary-alt-color
    nil
    :type string
    :documentation "The color applied to elements appearing in front of
`secondary-alt-color'.  Must strongly contrast with `secondary-alt-color'.")
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
   (accent-alt-color
    "#178DCC"
    :type string
    :documentation "A nuanced version of `accent-color'.")
   (on-accent-alt-color
    nil
    :type string
    :documentation "The color applied to elements appearing in front of
`accent-alt-color'.  Must strongly contrast with `accent-alt-color'.")
   (warning-color
    "#AF1923"
    :type string
    :documentation "The color that communicates errors.")
   (on-warning-color
    nil
    :type string
    :documentation "The color applied to elements appearing in front of
`warning-color'.  Must strongly contrast with `warning-color'.")
   (warning-alt-color
    "#D2232E"
    :type string
    :documentation "A nuanced version of `warning-color'.")
   (on-warning-alt-color
    nil
    :type string
    :documentation "The color applied to elements appearing in front of
`warning-alt-color'.  Must strongly contrast with `warning-alt-color'.")
   (font-family
    "Helvetica Neue, Helvetica"
    :type string
    :documentation "The font family to use by default."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t))

(export-always '+light-theme+)
(defvar +light-theme+
  (make-instance 'theme))

(export-always '+dark-theme+)
(defvar +dark-theme+
  (make-instance 'theme
                 :dark-p t
                 :background-color "black"
                 :on-background-color "white"
                 :background-alt-color "#333333"
                 :on-background-alt-color "white"
                 :primary-color "#E48D4E"
                 :on-primary-color "black"
                 :primary-alt-color "#D7752F"
                 :on-primary-alt-color "black"
                 :secondary-color "#874215"
                 :on-secondary-color "white"
                 :secondary-alt-color "#A55D2F"
                 :on-secondary-alt-color "white"
                 :accent-color "#571FD2"
                 :on-accent-color "white"
                 :accent-alt-color "#763DF2"
                 :on-accent-alt-color "white"
                 :warning-color "#FCBA04"
                 :on-warning-color "black"
                 :warning-alt-color "#FCA904"
                 :on-warning-alt-color "black"))

(export-always '(theme
                 background on-background background-alt on-background-alt
                 primary on-primary primary-alt on-primary-alt
                 secondary on-secondary secondary-alt on-secondary-alt
                 accent on-accent accent-alt on-accent-alt
                 warning on-warning warning-alt on-warning-alt
                 font-family))
(defvar theme nil
  "Dynamic variable that binds `theme' in `themed-css'.")
(defvar background nil
  "Dynamic variable that binds `background-color' of `theme' in `themed-css'.")
(defvar on-background nil
  "Dynamic variable that binds `on-background-color' of `theme' in `themed-css'.")
(defvar background-alt nil
  "Dynamic variable that binds `background-alt-color' of `theme' in `themed-css'.")
(defvar on-background-alt nil
  "Dynamic variable that binds `on-background-alt-color' of `theme' in `themed-css'.")
(defvar primary nil
  "Dynamic variable that binds `primary-color' of `theme' in `themed-css'.")
(defvar on-primary nil
  "Dynamic variable that binds `on-primary-color' of `theme' in `themed-css'.")
(defvar primary-alt nil
  "Dynamic variable that binds `primary-alt-color' of `theme' in `themed-css'.")
(defvar on-primary-alt nil
  "Dynamic variable that binds `on-primary-alt-color' of `theme' in `themed-css'.")
(defvar secondary nil
  "Dynamic variable that binds `secondary-color' of `theme' in `themed-css'.")
(defvar on-secondary nil
  "Dynamic variable that binds `on-secondary-color' of `theme' in `themed-css'.")
(defvar secondary-alt nil
  "Dynamic variable that binds `secondary-alt-color' of `theme' in `themed-css'.")
(defvar on-secondary-alt nil
  "Dynamic variable that binds `on-secondary-alt-color' of `theme' in `themed-css'.")
(defvar accent nil
  "Dynamic variable that binds `accent-color' of `theme' in `themed-css'.")
(defvar on-accent nil
  "Dynamic variable that binds `on-accent-color' of `theme' in `themed-css'.")
(defvar accent-alt nil
  "Dynamic variable that binds `accent-alt-color' of `theme' in `themed-css'.")
(defvar on-accent-alt nil
  "Dynamic variable that binds `on-accent-alt-color' of `theme' in `themed-css'.")
(defvar warning nil
  "Dynamic variable that binds `warning-color' of `theme' in `themed-css'.")
(defvar on-warning nil
  "Dynamic variable that binds `on-warning-color' of `theme' in `themed-css'.")
(defvar warning-alt nil
  "Dynamic variable that binds `warning-alt-color' of `theme' in `themed-css'.")
(defvar on-warning-alt nil
  "Dynamic variable that binds `on-warning-alt-color' of `theme' in `themed-css'.")
(defvar font-family nil
  "Dynamic variable that binds `font-family' of `theme' in `themed-css'.")

(export-always 'with-theme)
(defmacro with-theme (theme &body body)
  "Evaluate body with the theme bindings available."
  `(let* ((theme:theme ,theme)
          (theme:background (background-color theme:theme))
          (theme:on-background (on-background-color theme:theme))
          (theme:background-alt (background-alt-color theme:theme))
          (theme:on-background-alt (on-background-alt-color theme:theme))
          (theme:primary (primary-color theme:theme))
          (theme:on-primary (on-primary-color theme:theme))
          (theme:primary-alt (primary-alt-color theme:theme))
          (theme:on-primary-alt (on-primary-alt-color theme:theme))
          (theme:secondary (secondary-color theme:theme))
          (theme:on-secondary (on-secondary-color theme:theme))
          (theme:secondary-alt (secondary-alt-color theme:theme))
          (theme:on-secondary-alt (on-secondary-alt-color theme:theme))
          (theme:accent (accent-color theme:theme))
          (theme:on-accent (on-accent-color theme:theme))
          (theme:accent-alt (accent-alt-color theme:theme))
          (theme:on-accent-alt (on-accent-alt-color theme:theme))
          (theme:warning (warning-color theme:theme))
          (theme:on-warning (on-warning-color theme:theme))
          (theme:warning-alt (warning-alt-color theme:theme))
          (theme:on-warning-alt (on-warning-alt-color theme:theme))
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

;;; Color contrast utils

(serapeum:-> relative-luminance ((or string integer cl-colors2:rgb cl-colors2:hsv))
             real) ;; What's the range?
(defun relative-luminance (color)
  "Compute relative luminance of COLOR."
  ;; See https://developer.mozilla.org/en-US/docs/Web/Accessibility/Understanding_Colors_and_Luminance#modeling_light_color_and_vision
  (loop with rgb = (cl-colors2:as-rgb color)
        for const in '(0.2126 0.7152 0.0722)
        for rgb-component in (list (cl-colors2:rgb-red (cl-colors2:as-rgb color))
                                   (cl-colors2:rgb-green (cl-colors2:as-rgb color))
                                   (cl-colors2:rgb-blue (cl-colors2:as-rgb color)))
        sum (* const (if (<= rgb-component 0.04045)
                         (/ rgb-component 12.92)
                         (expt (/ (+ rgb-component 0.055) 1.055) 2.4)))))

(serapeum:-> contrast-ratio ((or string integer cl-colors2:rgb cl-colors2:hsv)
                             (or string integer cl-colors2:rgb cl-colors2:hsv))
             ;; 21 is the ratio between black and white.
             (real 0 21))
(export-always 'contrast-ratio)
(defun contrast-ratio (color1 color2)
  "Compute contrast ratio between COLOR1 and COLOR2."
  (let ((ratio (/ (+ (relative-luminance color1) 0.05)
                  (+ (relative-luminance color2) 0.05))))
    (max ratio (/ ratio))))

(serapeum:-> contrasting-color ((or string integer cl-colors2:rgb cl-colors2:hsv)) string)
(export-always 'contrasting-color)
(defun contrasting-color (color)
  "Determine whether black or white best contrasts with COLOR."
  (if (>= (contrast-ratio color "white")
          (contrast-ratio color "black"))
      "white"
      "black"))
