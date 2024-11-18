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

(export-always '+light-theme+)
(defvar +light-theme+
  (make-instance 'theme
                 :background-color+ "#FFFFFF"
                 :background-color  "#F8F8F8"
                 :background-color- "#ECECEC"
                 :primary-color+    "#474747"
                 :primary-color     "#555555"
                 :primary-color-    "#686868"
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
  (make-instance 'theme
                 :background-color+ "#000000"
                 :background-color  "#121212"
                 :background-color- "#333333"
                 :primary-color+    "#EFA671"
                 :primary-color     "#E48D4E"
                 :primary-color-    "#D7752F"
                 :secondary-color+  "#683008"
                 :secondary-color   "#844115"
                 :secondary-color-  "#9F592D"
                 :action-color+     "#481FA2"
                 :action-color      "#571FD2"
                 :action-color-     "#763DF2"
                 :highlight-color+  "#FC83F2"
                 :highlight-color   "#F46DE8"
                 :highlight-color-  "#EA43DD"
                 :success-color+    "#87FCDF"
                 :success-color     "#4CFBCF"
                 :success-color-    "#05F4CD"
                 :warning-color+    "#FFD152"
                 :warning-color     "#FCBA04"
                 :warning-color-    "#FCA904"))
