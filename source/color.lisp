;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *css-colors*
  '("AliceBlue" "AntiqueWhite" "Aqua" "Aquamarine" "Azure" "Beige" "Bisque"
    "Black" "BlanchedAlmond" "Blue" "BlueViolet" "Brown" "BurlyWood" "CadetBlue"
    "Chartreuse" "Chocolate" "Coral" "CornflowerBlue" "Cornsilk" "Crimson" "Cyan"
    "DarkBlue" "DarkCyan" "DarkGoldenRod" "DarkGray" "DarkGrey" "DarkGreen"
    "DarkKhaki" "DarkMagenta" "DarkOliveGreen" "DarkOrange" "DarkOrchid" "DarkRed"
    "DarkSalmon" "DarkSeaGreen" "DarkSlateBlue" "DarkSlateGray" "DarkSlateGrey"
    "DarkTurquoise" "DarkViolet" "DeepPink" "DeepSkyBlue" "DimGray" "DimGrey"
    "DodgerBlue" "FireBrick" "FloralWhite" "ForestGreen" "Fuchsia" "Gainsboro"
    "GhostWhite" "Gold" "GoldenRod" "Gray" "Grey" "Green" "GreenYellow" "HoneyDew"
    "HotPink" "IndianRed" "Indigo" "Ivory" "Khaki" "Lavender" "LavenderBlush"
    "LawnGreen" "LemonChiffon" "LightBlue" "LightCoral" "LightCyan"
    "LightGoldenRodYellow" "LightGray" "LightGrey" "LightGreen" "LightPink"
    "LightSalmon" "LightSeaGreen" "LightSkyBlue" "LightSlateGray" "LightSlateGrey"
    "LightSteelBlue" "LightYellow" "Lime" "LimeGreen" "Linen" "Magenta" "Maroon"
    "MediumAquaMarine" "MediumBlue" "MediumOrchid" "MediumPurple" "MediumSeaGreen"
    "MediumSlateBlue" "MediumSpringGreen" "MediumTurquoise" "MediumVioletRed"
    "MidnightBlue" "MintCream" "MistyRose" "Moccasin" "NavajoWhite" "Navy" "OldLace"
    "Olive" "OliveDrab" "Orange" "OrangeRed" "Orchid" "PaleGoldenRod" "PaleGreen"
    "PaleTurquoise" "PaleVioletRed" "PapayaWhip" "PeachPuff" "Peru" "Pink" "Plum"
    "PowderBlue" "Purple" "RebeccaPurple" "Red" "RosyBrown" "RoyalBlue"
    "SaddleBrown" "Salmon" "SandyBrown" "SeaGreen" "SeaShell" "Sienna" "Silver"
    "SkyBlue" "SlateBlue" "SlateGray" "SlateGrey" "Snow" "SpringGreen" "SteelBlue"
    "Tan" "Teal" "Thistle" "Tomato" "Turquoise" "Violet" "Wheat" "White"
    "WhiteSmoke" "Yellow" "YellowGreen")
  "All the named CSS colors to construct `color-source' from.")

(defvar copy-actions
  (list (lambda-command copy-as-hex* (colors)
          "Copy the color as hex #XXXXXX string."
          (let ((hex (cl-colors-ng:print-hex (first colors))))
            (ffi-buffer-copy (current-buffer) hex)
            (echo "~s copied to clipboard." hex)))
        (lambda-command copy-as-rgb* (colors)
          "Copy the color as CSS rgb() function string."
          (let ((rgb (cl-colors-ng:print-css-rgb/a (first colors))))
            (ffi-buffer-copy (current-buffer) rgb)
            (echo "Copied ~a to clipboard!" rgb)))
        (lambda-command copy-as-hsl* (colors)
          "Copy the color as CSS hsl() function string."
          (let ((hsl (cl-colors-ng:print-css-hsl (first colors))))
            (ffi-buffer-copy (current-buffer) hsl)
            (echo "Copied ~a to clipboard!" hsl)))))

(export-always 'color-source)
(define-class color-source (prompter:source)
  ((prompter:name "Color")
   (prompter:constructor *css-colors*)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:filter-postprocessor
    (lambda (suggestions source input-color)
      (unless (str:empty? input-color) (sleep 0.2))
      (append (when (ignore-errors (cl-colors-ng:as-rgb input-color))
                (list (make-instance 'prompter:suggestion
                                     :value input-color
                                     :attributes (prompter:object-attributes input-color source))))
            suggestions)))
   (prompter:actions-on-current-suggestion-enabled-p t)
   (prompter:actions-on-return
    (cons #'identity copy-actions)))
  (:documentation "A source for color search and copying.
Allows looking through the colors based on their names, HEX values, and
rgb()/hsl() CSS functions representing them."))

(defmethod prompter:object-attributes ((color string) (source color-source))
  `(("Color" ,color)
    ("HEX" ,(cl-colors-ng:print-hex color))
    ("RGB" ,(cl-colors-ng:print-css-rgb/a color))))

(define-command-global pick-color ()
  "Pick a color and copy it to clipboard.
The current color is previewed in the prompt buffer's input area.

Color can be entered as:
- CSS color name: \"PapayaWhip\" (capitalization is optional.)
- HEX code: \"#37A8E4\".
- HSL and RGB functions inspired by CSS."
  (prompt :prompt "Color"
          :sources (make-instance 'color-source
                                  :actions-on-return copy-actions)))
