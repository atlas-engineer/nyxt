;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; We use `uiop:define-package' instead of `nyxt:define-package' since this does
;; not depend on Nyxt at all.
(uiop:define-package :nyxt/keyscheme
  (:use :common-lisp)
  (:import-from :serapeum #:export-always #:->)
  (:documentation "Nyxt type specialization for `keymap' bound values."))
(in-package :nyxt) ; In case the package is locked.
(trivial-package-local-nicknames:add-package-local-nickname :keyscheme :nyxt/keyscheme :nyxt)
(in-package :nyxt/keyscheme)

;; Setting `nkeymaps:keymap' bound-value type to something like `function-symbol'
;; is not practical because functions defined in the same file as the keymap are
;; not `fboundp' at compile-time.
;;
;; To overcome this limitation, we can first load the system, then set the type
;; to something more specific and reload the system to catch the type errors.

(export-always 'nyxt-keymap-value)
#+nyxt-debug-keymap
(deftype nyxt-keymap-value ()
  '(or nkeymaps:keymap nyxt:function-symbol nyxt::command))

#-nyxt-debug-keymap
(deftype nyxt-keymap-value ()
  '(or nkeymaps:keymap t))

(export-always 'make-keyscheme)
(defun make-keyscheme (name &rest parents)
  "Return a new `nkeymaps:keyscheme' object of type `nyxt-keymap-value'.
The scheme name inherits from the optional PARENTS, ordered by priority.

Example:

  (defvar emacs (make-keyscheme \"emacs\" cua))

In the above, we define a new scheme name called `emacs' which inherits from the
existing keyscheme `cua'."
  (the (values nkeymaps:keyscheme &optional)
       (make-instance 'nkeymaps:keyscheme
                      :name name
                      :parents parents
                      :bound-type 'nyxt-keymap-value)))

(export-always 'default)
(defvar default (make-keyscheme "default"))
(export-always 'cua)
(defvar cua (make-keyscheme "cua" default))
(export-always 'emacs)
(defvar emacs (make-keyscheme "emacs" default))
(export-always 'vi-normal)
(defvar vi-normal (make-keyscheme "vi-normal" default))
(export-always 'vi-insert)
(defvar vi-insert (make-keyscheme "vi-insert"))
