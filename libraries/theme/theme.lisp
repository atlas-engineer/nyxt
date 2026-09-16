;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme)

(define-class theme ()
  ((background-color+
    :documentation "More contrasting variation of `background-color'.")
   (background-color
    :documentation "The background color of the theme.")
   (background-color-
    :documentation "Less contrasting variation of `background-color'.")
   (on-background-color
    :documentation "The color for elements/text in front of `background-color'.")
   (primary-color+
    :documentation "More contrasting variation of `primary-color'.")
   (primary-color
    :documentation "Primary UI element color.")
   (primary-color-
    :documentation "Less contrasting variation of `primary-color'.")
   (on-primary-color
    :documentation "The color for elements/text in front of `primary-color'.")
   (secondary-color+
    :documentation "More contrasting variation of `secondary-color'.")
   (secondary-color
    :documentation "Secondary UI element color.")
   (secondary-color-
    :documentation "Less contrasting variation of `secondary-color'.")
   (on-secondary-color
    :documentation "The color for elements/text in front of `secondary-color'.")
   (action-color+
    :documentation "More contrasting variation of `action-color'.")
   (action-color
    :documentation "Color for focused and important elements.")
   (action-color-
    :documentation "Less contrasting variation of `action-color'.")
   (on-action-color
    :documentation "The color for elements/text in front of `action-color'.")
   (highlight-color+
    :documentation "More contrasting variation of `highlight-color'.")
   (highlight-color
    :documentation "The color for elements requiring attention.")
   (highlight-color-
    :documentation "Less contrasting variation of `highlight-color'.")
   (on-highlight-color
    :documentation "The color for elements/text in front of `highlight-color'.")
   (success-color+
    :documentation "More contrasting variation of `success-color'.")
   (success-color
    :documentation "The color to express success.")
   (success-color-
    :documentation "Less contrasting variation of `success-color'.")
   (on-success-color
    :documentation "The color for elements/text in front of `success-color'.")
   (warning-color+
    :documentation "More contrasting variation of `warning-color'.")
   (warning-color
    :documentation "The color to express errors.")
   (warning-color-
    :documentation "Less contrasting variation of `warning-color'.")
   (on-warning-color
    :documentation "The color for elements/text in front of `warning-color'.")
   (font-family
    "Public Sans"
    :documentation "The font family to use by default.")
   (monospace-font-family
    "DejaVu Sans Mono"
    :documentation "The monospace font family to use by default."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t))

(defmethod initialize-instance :after ((theme theme) &key)
  (multiple-value-bind (on-colors regular-colors minus-colors plus-colors)
      (values-list
       (filter-palette (list (alexandria:curry #'uiop:string-prefix-p "ON-")
                             (alexandria:rcurry #'uiop:string-suffix-p "COLOR")
                             (alexandria:rcurry #'uiop:string-suffix-p "COLOR-")
                             (alexandria:rcurry #'uiop:string-suffix-p "COLOR+"))
                       (palette theme)))
    (loop for on-color in on-colors
          for regular-color in regular-colors
          for minus-color in minus-colors
          for plus-color in plus-colors
          do (when (and (not (slot-value theme on-color))
                        (slot-value theme regular-color))
               (setf (slot-value theme on-color)
                     (contrasting-color (slot-value theme regular-color))))
          do (when (and (not (slot-value theme minus-color))
                        (slot-value theme regular-color))
               (setf (slot-value theme minus-color)
                     (slot-value theme regular-color)))
          do (when (and (not (slot-value theme plus-color))
                        (slot-value theme regular-color))
               (setf (slot-value theme plus-color)
                     (slot-value theme regular-color))))))

(export-always 'dark-p)
(defmethod dark-p ((theme theme))
  "Whether the theme is dark."
  (when (string= "white" (contrasting-color (background-color theme))) t))

(export-always 'palette)
(defmethod palette ((theme theme))
  "Return color slots of THEME.

Example that returns the palette's color values:
(mapcar (alexandria:rcurry #'funcall +light-theme+)
        (palette +light-theme+))"
  (serapeum:filter (alexandria:curry #'serapeum:string-contains-p "COLOR")
                   (mopu:direct-slot-names theme)
                   :key #'string))

(export-always 'filter-palette)
(defun filter-palette (preds palette)
  "Partition PALETTE according to PREDS."
  (serapeum:partitions preds palette :key #'string))

(export-always 'with-theme)
(defmacro with-theme (theme-instance &body body)
  "Evaluate BODY with THEME and THEME's slots let-bound."
  `(let ((theme ,theme-instance))
     (with-slots ,(mopu:direct-slot-names 'theme) theme
       ,@body)))

(export-always 'themed-css)
(defmacro themed-css (theme &body forms)
  "Generate CSS via lass FORMS styled according to THEME.

Example:

(themed-css (make-instance 'theme :background-color \"white\")
           `(|h1,h2,h3,h4,h5,h6|
             :border-style \"solid\"
             :border-color ,theme:on-background-color)
           `(p
             :color ,(if (theme:dark-p theme:theme) \"yellow\" \"green\")))"
  `(with-theme ,theme (lass:compile-and-write ,@forms)))

(defun css-variable-name (slot)
  (let ((name (string-downcase (symbol-name slot))))
    (format nil "--nyxt-theme-~a"
            (cond ((uiop:string-suffix-p "+" name)
                   (concatenate 'string (subseq name 0 (1- (length name)))
                                "-plus"))
                  ((uiop:string-suffix-p "-" name)
                   (concatenate 'string (subseq name 0 (1- (length name)))
                                "-minus"))
                  (t name)))))

(defun css-variable-reference (slot theme)
  (let ((name (css-variable-name slot))
        (value (funcall slot theme)))
    (if value
        (format nil "var(~a, ~a)" name value)
        (format nil "var(~a)" name))))

(export-always 'with-theme-css-variables)
(defmacro with-theme-css-variables (theme-instance &body body)
  "Evaluate BODY with THEME slots bound to live CSS custom-property references."
  (let ((slots (mopu:direct-slot-names 'theme)))
    `(let ((theme ,theme-instance))
       (let ,(loop for slot in slots
                   collect `(,slot (css-variable-reference ',slot theme)))
         (declare (ignorable ,@slots))
         ,@body))))

(export-always 'themed-css-variables)
(defmacro themed-css-variables (theme &body forms)
  "Generate CSS with theme values backed by live custom properties."
  `(with-theme-css-variables ,theme
     (lass:compile-and-write ,@forms)))

(export-always 'css-variables)
(defun css-variables (theme)
  "Return inline CSS declarations defining THEME's live custom properties."
  (with-output-to-string (stream)
    (dolist (slot (mopu:direct-slot-names 'theme))
      (let ((value (funcall slot theme)))
        (when value
          (format stream "~a:~a;" (css-variable-name slot) value))))
    (format stream "--nyxt-theme-logo-color:~a;"
            (if (dark-p theme)
                (action-color theme)
                (on-background-color theme)))
    (format stream "--nyxt-theme-color-scheme:~a;color-scheme:~a;"
            (if (dark-p theme) "dark" "light")
            (if (dark-p theme) "dark" "light"))))

(export-always '+light-theme+)
(defvar +light-theme+
  (make-instance 'theme
                 :background-color+ "#FFFFFF"
                 :background-color  "#F8F8F8"
                 :background-color- "#ECECEC"
                 :primary-color+    "#999999"
                 :primary-color     "#686868"
                 :primary-color-    "#555555"
                 :secondary-color+  "#BFBFBF"
                 :secondary-color   "#A6A6A6"
                 :secondary-color-  "#909090"
                 :action-color+     "#72CDFE"
                 :action-color      "#37A8E4"
                 :action-color-     "#178DCC"
                 :highlight-color+  "#FFFA66"
                 :highlight-color   "#FCE304"
                 :highlight-color-  "#FCBA04"
                 :success-color+    "#71FE7D"
                 :success-color     "#8AEA92"
                 :success-color-    "#86D58E"
                 :warning-color+    "#88040D"
                 :warning-color     "#AF1923"
                 :warning-color-    "#D2232E"))

(export-always '+dark-theme+)
(defvar +dark-theme+
  (make-instance 'theme:theme
                 :background-color- "#3B4252"
                 :background-color "#2E3440"
                 :background-color+ "#434C5E"
                 :on-background-color "#E5E9F0"
                 :primary-color- "#5E81AC"
                 :primary-color "#5E81AC"
                 :primary-color+ "#81A1C1"
                 :on-primary-color "#ECEFF4"
                 :secondary-color- "#4C566A"
                 :secondary-color "#4C566A"
                 :secondary-color+ "#5E81AC"
                 :on-secondary-color "#E5E9F0"
                 :action-color- "#88C0D0"
                 :action-color "#88C0D0"
                 :action-color+ "#81A1C1"
                 :on-action-color "#2E3440"
                 :success-color- "#8FBCBB"
                 :success-color "#8FBCBB"
                 :success-color+ "#81A1C1"
                 :on-success-color "#2E3440"
                 :highlight-color- "#B48EAD"
                 :highlight-color "#B48EAD"
                 :highlight-color+ "#D8DEE9"
                 :on-highlight-color "#2E3440"
                 :warning-color- "#EBCB8B"
                 :warning-color "#EBCB8B"
                 :warning-color+ "#D08770"
                 :on-warning-color "#2E3440"))

(export-always '+oled-theme+)
(defvar +oled-theme+
  (make-instance 'theme
                 :background-color+ "#000000"
                 :background-color  "#000000"
                 :background-color- "#111111"
                 :primary-color+    "#FFFFFF"
                 :primary-color     "#CCCCCC"
                 :primary-color-    "#999999"
                 :secondary-color+  "#444444"
                 :secondary-color   "#000000"
                 :secondary-color-  "#222222"
                 :action-color+     "#0099FF"
                 :action-color      "#007ACC"
                 :action-color-     "#005999"
                 :highlight-color+  "#FFFF00"
                 :highlight-color   "#FFCC00"
                 :highlight-color-  "#FF9900"
                 :success-color+    "#00FF88"
                 :success-color     "#00CC66"
                 :success-color-    "#009944"
                 :warning-color+    "#FF6666"
                 :warning-color     "#FF3333"
                 :warning-color-    "#CC0000"
                 :font-family       "TT Fors Trial"
                 :monospace-font-family "Berkeley Mono"))

(export-always '+acme-theme+)
(defvar +acme-theme+
  (make-instance 'theme
                 :background-color+ "#FFFFF4"
                 :background-color  "#FFFFE8"
                 :background-color- "#EFEFD8"
                 :on-background-color "#444444"
                 :primary-color+    "#005555"
                 :primary-color     "#007777"
                 :primary-color-    "#007F7F"
                 :on-primary-color  "#FFFFFF"
                 :secondary-color+  "#A8EFEB"
                 :secondary-color   "#E1FAFF"
                 :secondary-color-  "#EFEFD8"
                 :on-secondary-color "#444444"
                 :action-color+     "#0B3E82"
                 :action-color      "#1054AF"
                 :action-color-     "#2F6FC4"
                 :on-action-color   "#FFFFFF"
                 :highlight-color+  "#E8EB98"
                 :highlight-color   "#F8FCE8"
                 :highlight-color-  "#FFFFE8"
                 :on-highlight-color "#444444"
                 :success-color+    "#004400"
                 :success-color     "#005500"
                 :success-color-    "#006600"
                 :on-success-color  "#FFFFFF"
                 :warning-color+    "#660000"
                 :warning-color     "#880000"
                 :warning-color-    "#AA2222"
                 :on-warning-color  "#FFFFFF"
                 :font-family       "ProFontExtended"
                 :monospace-font-family "ProFontExtended"))

(export-always '+kanagawa-dragon-theme+)
(defvar +kanagawa-dragon-theme+
  (make-instance 'theme
                 :background-color+ "#0D0C0C"
                 :background-color  "#080606"
                 :background-color- "#181616"
                 :on-background-color "#C5C9C5"
                 :primary-color+    "#AFC0C7"
                 :primary-color     "#8BA4B0"
                 :primary-color-    "#6F8995"
                 :on-primary-color  "#080606"
                 :secondary-color+  "#181616"
                 :secondary-color   "#1E3F52"
                 :secondary-color-  "#2D4F67"
                 :on-secondary-color "#C5C9C5"
                 :action-color+     "#98C1BA"
                 :action-color      "#7AA89F"
                 :action-color-     "#658F88"
                 :on-action-color   "#080606"
                 :highlight-color+  "#D8CAA8"
                 :highlight-color   "#C4B28A"
                 :highlight-color-  "#A99975"
                 :on-highlight-color "#080606"
                 :success-color+    "#A8B99A"
                 :success-color     "#8A9A7B"
                 :success-color-    "#728366"
                 :on-success-color  "#080606"
                 :warning-color+    "#DE918A"
                 :warning-color     "#C4746E"
                 :warning-color-    "#AF625D"
                 :on-warning-color  "#080606"
                 :font-family       "ProFontExtended"
                 :monospace-font-family "ProFontExtended"))
