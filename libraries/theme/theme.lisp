;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme)

;; Copied from Nyxt.
(deftype maybe (&rest types)
  `(or null ,@types))

(define-class theme ()
  ((background-color-
    "#ececec"
    :documentation "Less contrasting variation of `background-color'.")
   (background-color
    "#f8f8f8"
    :documentation "The background color of the theme.")
   (background-color+
    "#ffffff"
    :documentation "More contrasting variation of `background-color'.")
   (on-background-color
    nil
    :documentation "The color for elements/text in front of `background-color'.")
   (primary-color-
    "#686868"
    :documentation "Less contrasting variation of `primary-color'.")
   (primary-color
    "#555555"
    :documentation "Primary UI element color.")
   (primary-color+
    "#474747"
    :documentation "More contrasting variation of `primary-color'.")
   (on-primary-color
    nil
    :documentation "The color for elements/text in front of `primary-color'.")
   (secondary-color-
    "#909090"
    :documentation "Less contrasting variation of `secondary-color'.")
   (secondary-color
    "#a6a6a6"
    :documentation "Secondary UI element & decoration color.")
   (secondary-color+
    "#bfbfbf"
    :documentation "More contrasting variation of `secondary-color'.")
   (on-secondary-color
    nil
    :documentation "The color for elements/text in front of `secondary-color'.")
   (action-color-
    "#178dcc"
    :documentation "Less contrasting variation of `action-color'.")
   (action-color
    "#37a8e4"
    :documentation "Color for focused and important elements.")
   (action-color+
    "#72cdfe"
    :documentation "More contrasting variation of `action-color'.")
   (on-action-color
    nil
    :documentation "The color for elements/text in front of `action-color'.")
   (highlight-color-
    "#fcba04"
    :documentation "Less contrasting variation of `highlight-color'.")
   (highlight-color
    "#fce304"
    :documentation "The color for highlighting elements requiring attention.")
   (highlight-color+
    "#fffa66"
    :documentation "More contrasting variation of `highlight-color'.")
   (on-highlight-color
    nil
    :documentation "The color for elements/text in front of `highlight-color'.")
   (success-color-
    "#86d58e"
    :documentation "Less contrasting variation of `success-color'.")
   (success-color
    "#8aea92"
    :documentation "The color for successful/positive result/action.")
   (success-color+
    "#71fe7d"
    :documentation "More contrasting variation of `success-color'.")
   (on-success-color
    nil
    :documentation "The color for elements/text in front of `success-color'.")
   (warning-color-
    "#d2232e"
    :documentation "Less contrasting variation of `warning-color'.")
   (warning-color
    "#af1923"
    :documentation "The color that communicates errors and potentially dangerous actions.")
   (warning-color+
    "#88040d"
    :documentation "More contrasting variation of `warning-color'.")
   (on-warning-color
    nil
    :documentation "The color for elements/text in front of `warning-color'.")
   (codeblock-color-
    "#e7e7c0"
    :documentation "Less contrasting variation of `codeblock-color'.")
   (codeblock-color
    "#f6f6e8"
    :documentation "The color for code listings.")
   (codeblock-color+
    "#fefff6"
    :documentation "More contrasting variation of `codeblock-color'.")
   (on-codeblock-color
    nil
    :documentation "The color for elements/text in front of `codeblock-color'.")
   (text-color-
    "#19191a"
    :documentation "Less contrasting variation of `text-color'.")
   (text-color
    "#0c0c0d"
    :documentation "The color for most of the element text.")
   (text-color+
    "#000000"
    :documentation "More contrasting variation of `text-color'.")
   (contrast-text-color
    "#ffffff"
    :documentation "The color for elements that don't contrast well with `text-color'.")
   (font-family
    "Public Sans"
    :type string
    :documentation "The font family to use by default."))
  (:automatic-types-p t)
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t))

(defmacro defaccessors (deprecated-name name)
  "Boilerplate to generate old names accessors and return the new values."
  `(progn
     (export-always (quote ,deprecated-name))
     (nclasses:define-generic ,deprecated-name ((theme theme))
       ,(format nil "Auto-generated accessor for deprecated `~a' slot.
Returns the value of `~a' slot instead of the deprecated one."
                deprecated-name name)
       (slot-value theme (quote ,name)))
     ;; Use define-generic too?
     (defmethod (setf ,deprecated-name) (value (theme theme))
       (setf (slot-value theme (quote ,name)) value))))

(defaccessors background-alt-color background-color-)
(defaccessors primary-alt-color primary-color-)
(defaccessors secondary-alt-color secondary-color-)
(defaccessors warning-alt-color warning-color-)
(defaccessors accent-color action-color)
(defaccessors accent-alt-color action-color-)
(defaccessors on-background-alt-color on-background-color)
(defaccessors on-primary-alt-color on-secondary-color)
(defaccessors on-secondary-alt-color on-secondary-color)
(defaccessors on-accent-color on-action-color)
(defaccessors on-accent-alt-color on-action-color)
(defaccessors on-warning-alt-color on-warning-color)

(export-always 'contrasting-text-color)
(defun contrasting-text-color (theme color)
  (cond
    ((not (or (text-color theme)
              (contrast-text-color theme)))
     (contrasting-color color))
    ((< (contrast-ratio color (text-color theme))
        (contrast-ratio color (contrast-text-color theme)))
     (contrast-text-color theme))
    (t (text-color theme))))

(defmethod initialize-instance :after
    ((theme theme)
     &rest keys
     &key
       ;; Completing contrast colors.
       background-color- background-color+
       primary-color- primary-color+
       secondary-color- secondary-color+
       action-color- action-color+
       highlight-color- highlight-color+
       success-color- success-color+
       warning-color- warning-color+
       codeblock-color- codeblock-color+
       text-color- text-color+
       ;; Deprecated names
       background-alt-color
       primary-alt-color
       secondary-alt-color
       warning-alt-color
       ;; Accent renamed to action.
       accent-color
       accent-alt-color
       ;; Ignore these—there are regular on-colors.
       on-background-alt-color on-primary-alt-color
       on-secondary-alt-color on-accent-color
       on-accent-alt-color on-warning-alt-color
     &allow-other-keys)
  (declare (ignorable on-background-alt-color on-primary-alt-color on-secondary-alt-color
                      on-accent-color on-accent-alt-color on-warning-alt-color))
  ;; Set values for colors referred to by deprecated names.
  (loop for (slot-name old-keyword-value)
          in `((background-color- ,background-alt-color)
               (primary-color- ,primary-alt-color)
               (secondary-color- ,secondary-alt-color)
               (action-color- ,accent-alt-color)
               (action-color ,accent-color)
               (warning-color- ,warning-alt-color))
        ;; Only use the deprecated name when the new one is not used.
        when (and old-keyword-value
                  (not (find slot-name keys :key (alexandria:compose #'first #'uiop:ensure-list))))
          do (setf (slot-value theme slot-name) old-keyword-value))
  ;; Set on-colors based on contrast ratios.
  (loop for (color on-color)
          in '((background-color on-background-color)
               (primary-color on-primary-color)
               (secondary-color on-secondary-color)
               (action-color on-action-color)
               (highlight-color on-highlight-color)
               (success-color on-success-color)
               (warning-color on-warning-color)
               (codeblock-color on-codeblock-color))
        ;; Honor init value, if available.
        unless (slot-value theme on-color)
          do (setf (slot-value theme on-color)
                   (contrasting-text-color theme (slot-value theme color))))
  ;; Generate +/- colors if they are not provided.
  (labels ((change-luminance (color direction)
             "Increase/decrease the luminance of COLOR.
DIRECTION is 1/-1 for the increased and decreased luminance respectively."
             (let* ((rgb (cl-colors2:as-rgb color)))
               (cl-colors2:print-hex
                ;; Magic numbers: 0.15 is the average distance between every
                ;; component of the color and color+/- in our light theme.
                (cl-colors2:rgb (max 0 (min 1.0 (+ (* direction 0.15) (cl-colors2:rgb-red rgb))))
                                (max 0 (min 1.0 (+ (* direction 0.15) (cl-colors2:rgb-green rgb))))
                                (max 0 (min 1.0 (+ (* direction 0.15) (cl-colors2:rgb-blue rgb))))))))
           (increase-contrast (base-color)
             (change-luminance
              base-color (if (equal "white" (contrasting-color base-color))
                             -1
                             1)))
           (decrease-contrast (base-color)
             (change-luminance
              base-color (if (equal "white" (contrasting-color base-color))
                             1
                             -1))))
    (loop for (basic less-contrasting-name more-contrasting-name less-contrasting more-contrasting)
            in `((background-color background-color- background-color+ ,background-color- ,background-color+)
                 (primary-color primary-color- primary-color+ ,primary-color- ,primary-color+)
                 (secondary-color secondary-color- secondary-color+ ,secondary-color- ,secondary-color+)
                 (action-color action-color- action-color+ ,action-color- ,action-color+)
                 (highlight-color highlight-color- highlight-color+ ,highlight-color- ,highlight-color+)
                 (success-color success-color- success-color+ ,success-color- ,success-color+)
                 (warning-color warning-color- warning-color+ ,warning-color- ,warning-color+)
                 (codeblock-color codeblock-color- codeblock-color+ ,codeblock-color- ,codeblock-color+)
                 (text-color text-color- text-color+ ,text-color- ,text-color+))
          unless (or less-contrasting
                     (slot-value theme less-contrasting-name))
            do (setf (slot-value theme less-contrasting-name)
                     (decrease-contrast (slot-value theme basic)))
          unless (or more-contrasting
                     (slot-value theme more-contrasting-name))
            do (setf (slot-value theme more-contrasting-name)
                     (increase-contrast (slot-value theme basic))))))

(export-always 'dark-p)
(defmethod dark-p ((theme theme))
  "Whether the theme is dark."
  (when (string= "white" (contrasting-color (background-color theme))) t))

(export-always 'palette)
(defmethod palette ((theme theme))
  "Return all color slots of THEME.

Example that returns values of color palette:
(mapcar (alexandria:rcurry #'funcall +light-theme+)
        (color-palette +light-theme+))"
  (serapeum:filter (alexandria:curry #'str:contains? "COLOR")
                   (mopu:direct-slot-names theme)
                   :key #'string))

(export-always 'filter-palette)
(defun filter-palette (pred palette)
  "Partition PALETTE according to PRED.

Return two values, where the first corresponds to the sequence that meets PRED."
  (serapeum:partition pred palette :key #'string))

(export-always '+light-theme+)
(defvar +light-theme+
  (make-instance 'theme))

(export-always '+dark-theme+)
(defvar +dark-theme+
  (make-instance 'theme
                 :background-color- "#333333"
                 :background-color "#121212"
                 :background-color+ "#000000"
                 :primary-color- "#d7752f"
                 :primary-color "#e48d4e"
                 :primary-color+ "#efa671"
                 :secondary-color- "#9f592d"
                 :secondary-color "#844115"
                 :secondary-color+ "#683008"
                 :action-color- "#763df2"
                 :action-color "#571fd2"
                 :action-color+ "#481fa2"
                 :highlight-color- "#ea43dd"
                 :highlight-color "#f46de8"
                 :highlight-color+ "#fc83f2"
                 :success-color- "#05f4cd"
                 :success-color "#4cfbcf"
                 :success-color+ "#87fcdf"
                 :warning-color- "#fca904"
                 :warning-color "#fcba04"
                 :warning-color+ "#ffd152"
                 :codeblock-color- "#44355a"
                 :codeblock-color "#2e243d"
                 :codeblock-color+ "#221a2d"
                 :text-color- "#dedede"
                 :text-color "#ededed"
                 :text-color+ "#ffffff"
                 :contrast-text-color "#0c0c0d"))

(defmacro def (name accessor-alias &optional deprecated)
  "Shorter theme variable definition."
  `(progn
     (export-always (quote ,name))
     (defvar ,name nil
       ,(let ((*print-case* :downcase))
          (format nil "Dynamic variable that binds `~a' in `themed-css'.
~@[~*Deprecated!~]" accessor-alias deprecated)))))

(def theme theme)
(def background background-color)
(def background+ background-color+)
(def background- background-color-)
(def on-background on-background-color)
(def background-alt background-color- t)
(def on-background-alt on-background-color t)
(def primary primary-color)
(def primary+ primary-color+)
(def primary- primary-color-)
(def on-primary on-primary-color)
(def primary-alt primary-color- t)
(def on-primary-alt on-primary-color t)
(def secondary secondary-color)
(def secondary+ secondary-color+)
(def secondary- secondary-color-)
(def on-secondary on-secondary-color)
(def secondary-alt secondary-color- t)
(def on-secondary-alt on-secondary-color t)
(def accent action-color t)
(def action action-color)
(def action+ action-color+)
(def action- action-color-)
(def on-accent on-action-color t)
(def on-action on-action-color)
(def accent-alt action-color- t)
(def on-accent-alt on-action-color t)
(def warning warning-color)
(def warning+ warning-color+)
(def warning- warning-color-)
(def on-warning on-warning-color)
(def warning-alt warning-color- t)
(def on-warning-alt on-warning-color t)
(def success success-color)
(def success+ success-color+)
(def success- success-color-)
(def on-success on-success-color)
(def highlight highlight-color)
(def highlight+ highlight-color+)
(def highlight- highlight-color-)
(def on-highlight on-highlight-color)
(def codeblock codeblock-color)
(def codeblock+ codeblock-color+)
(def codeblock- codeblock-color-)
(def on-codeblock on-codeblock-color)
(def font-family font-family)
(def text text-color)
(def text+ text-color+)
(def text- text-color-)
(def contrast-text contrast-text-color)

(export-always 'with-theme)
(defmacro with-theme (theme &body body)
  "Evaluate body with the theme bindings available."
  `(let* ((theme:theme ,theme)
          ,@(loop for (alias accessor) in
                  '((theme:background theme:background-color)
                    (theme:background+ theme:background-color+)
                    (theme:background- theme:background-color-)
                    (theme:on-background theme:on-background-color)
                    (theme:background-alt theme:background-color-)
                    (theme:on-background-alt theme:on-background-color)
                    (theme:primary+ theme:primary-color+)
                    (theme:primary theme:primary-color)
                    (theme:primary- theme:primary-color-)
                    (theme:on-primary theme:on-primary-color)
                    (theme:primary-alt theme:primary-color- t)
                    (theme:on-primary-alt theme:on-primary-color)
                    (theme:secondary theme:secondary-color)
                    (theme:secondary+ theme:secondary-color+)
                    (theme:secondary- theme:secondary-color-)
                    (theme:on-secondary theme:on-secondary-color)
                    (theme:secondary-alt theme:secondary-color-)
                    (theme:on-secondary-alt theme:on-secondary-color)
                    (theme:accent theme:action-color)
                    (theme:action theme:action-color)
                    (theme:action+ theme:action-color+)
                    (theme:action- theme:action-color-)
                    (theme:on-accent theme:on-action-color)
                    (theme:on-action theme:on-action-color)
                    (theme:accent-alt theme:action-color-)
                    (theme:on-accent-alt theme:on-action-color)
                    (theme:warning theme:warning-color)
                    (theme:warning+ theme:warning-color+)
                    (theme:warning- theme:warning-color-)
                    (theme:on-warning theme:on-warning-color)
                    (theme:warning-alt theme:warning-color-)
                    (theme:on-warning-alt theme:on-warning-color)
                    (theme:success theme:success-color)
                    (theme:success+ theme:success-color+)
                    (theme:success- theme:success-color-)
                    (theme:on-success theme:on-success-color)
                    (theme:highlight theme:highlight-color)
                    (theme:highlight+ theme:highlight-color+)
                    (theme:highlight- theme:highlight-color-)
                    (theme:on-highlight theme:on-highlight-color)
                    (theme:codeblock theme:codeblock-color)
                    (theme:codeblock+ theme:codeblock-color+)
                    (theme:codeblock- theme:codeblock-color-)
                    (theme:on-codeblock theme:on-codeblock-color)
                    (theme:text theme:text-color)
                    (theme:text+ theme:text-color+)
                    (theme:text- theme:text-color-)
                    (theme:contrast-text theme:contrast-text-color)
                    (theme:font-family theme:font-family))
                  collect `(,alias (,accessor theme:theme))))
     ,@body))

(export-always 'themed-css)
(defmacro themed-css (theme &body blocks)
  "Generate CSS BLOCKS styled according to THEME.

BLOCKS is a list of LASS blocks.

Any LASS-friendly syntax (including quasi-quotes) works, so you can evaluate
arbitrary code before the blocks are compiled to CSS. For the convenience of
quasi-quoted forms, this macro let-binds the set of symbols/slots for THEME
around the BLOCKS.

Example: if the theme is dark, color all paragraphs' text in accent color, or in
secondary color otherwise; color the borders of all headings in secondary color.

\(themed-css (make-instance 'theme :accent-color \"blue\")
           `(|h1,h2,h3,h4,h5,h6|
             :border-style \"solid\"
             :border-width \"1px\"
             :border-color ,theme:secondary)
           `(p
             :color ,(if (theme:dark-p theme:theme) theme:accent theme:secondary)))"
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
