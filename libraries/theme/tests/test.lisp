;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme/tests)

(prove:plan nil)

(defvar *theme* (make-instance
                 'theme:theme
                 :dark-p t
                 :background-color "black"
                 :text-color "white"
                 :accent-color "magenta"
                 :primary-color "yellow"
                 :secondary-color "blue"
                 :tertiary-color "green"
                 :quaternary-color "red")
  "An absolutely crazy theme to test things on.")

(prove:subtest "Basic CSS substitution"
  (prove:is (theme:themed-css *theme*
              (a
               :color theme:primary
               :background-color theme:background))
            "a { color: yellow; background-color: black; }
"))

(prove:subtest "Multi-rule/multi-color substitution"
  (prove:is (theme:themed-css *theme*
              (a
               :color theme:primary
               :background-color theme:background)
              (body
               :color theme:text
               :background-color theme:primary)
              (h1 :color theme:accent))
            "a { color: yellow; background-color: black; }
body { color: white; background-color: yellow; }
h1 { color: magenta; }
"))

(prove:subtest "Inline function execution"
  (prove:is  (theme:themed-css *theme*
               (body
                :color (concatenate 'string theme:accent " !important")
                :background-color theme:primary))
             "body { color: magenta !important; background-color: yellow; }
"))

(prove:subtest "Inline macro/special form invokation"
  (prove:is (theme:themed-css *theme*
              (body
               :color (if (theme:dark-p theme:theme) theme:background theme:text)
               :background-color theme:primary))
            "body { color: black; background-color: yellow; }
"))

(prove:finalize)

