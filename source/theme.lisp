;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class theme ()
  ((dark-p
    nil
    :documentation "Whether the theme is dark.
Dark themes override `nyxt/style-mode:dark-mode' colors too.")
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

(export-always 'themed-css)
(defmacro themed-css (theme &body rules)
  "Generate a CSS styled according to the THEME.

RULES is a list of CL-CSS rules (with one level of nesting removed). There are
special symbols that this macro will substitute for theme elements, if
encountered:
- %BACKGROUND% -- background color of the THEME.
- %TEXT% -- text color of the THEME.
- %PRIMARY% -- primary color of the THEME.
- %SECONDARY% -- secondary color of the THEME.
- %TERTIARY% -- tertiary color of the THEME.
- %ACCENT% -- accent color of the THEME.
- %FONT% -- font family of the theme.
- %IF-DARK -- should be a head of the list, followed by one or two forms. If
   THEME is dark, the whole list is substituted by the first form following
   %IF-DARK. Otherwise, the whole list is substituted by the second form (or
   nothing, if there's no form).
- %EVAL -- should be a head of the list, followed by one unquoted
   form. Evaluates one form following it and inserts the result inline. Inside
   this form, one can refer to %THEME% to get the THEME.

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
            :border-color %tertiary%)
           (p
            :color (%if-dark %accent% %secondary%)
            :background-color %text%))"
  (labels ((substitute-special-theme-symbols (rule)
             (typecase rule
               (symbol
                (if (str:s-member (list "%background%" "%text%" "%primary%" "%secondary%"
                                        "%tertiary%" "%quaternary%" "%accent%" "%font%")
                                  (str:downcase (symbol-name rule)))
                    rule
                    `(quote ,rule)))
               (list
                (if (and (symbolp (first rule))
                         (string-equal (str:downcase (symbol-name (first rule))) "%if-dark"))
                    `(%if-dark ,@(mapcar #'substitute-special-theme-symbols (rest rule)))
                    `(list ,@(mapcar #'substitute-special-theme-symbols rule))))
               (t `(quote ,rule)))))
    `(let ((%theme% ,theme))
       (declare (ignorable %theme%))
       (macrolet ((,(intern "%IF-DARK") (DARK light)
                    (if (dark-p %theme%)
                        dark
                        light))
                  (,(intern "%EVAL") (form)
                    (labels ((parse-theme-aware-form (form)
                               (typecase form
                                 (symbol (if (string-equal "%theme%" (symbol-name form))
                                             %theme%
                                             form))
                                 (list
                                  (mapcar #'parse-theme-aware-form form))
                                 (t form))))
                      (eval (parse-theme-aware-form form)))))
         (symbol-macrolet
             ((,(intern "%BACKGROUND%") (background-color %theme%))
              (,(intern "%TEXT%") (text-color %theme%))
              (,(intern "%PRIMARY%") (primary-color %theme%))
              (,(intern "%SECONDARY%") (secondary-color %theme%))
              (,(intern "%TERTIARY%") (tertiary-color %theme%))
              (,(intern "%QUATERNARY%") (quaternary-color %theme%))
              (,(intern "%ACCENT%") (accent-color %theme%))
              (,(intern "%FONT%") (font-family %theme%)))
           (cl-css:css
            ,(substitute-special-theme-symbols rules)))))))
