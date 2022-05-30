;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; We use `uiop:define-package' instead of `nyxt:define-package' since this does
;; not depend on Nyxt at all.
(uiop:define-package :nyxt/scheme
  (:use :common-lisp)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:documentation "Nyxt type specialization for `keymap' bound values."))
(in-package :nyxt) ; In case the package is locked.
(trivial-package-local-nicknames:add-package-local-nickname :scheme :nyxt/scheme :nyxt)
(in-package :nyxt/scheme)

;; Setting `keymap:keymap' bound-value type to something like `function-symbol'
;; is not practical because functions defined in the same file as the keymap are
;; not `fboundp' at compile-time.
;;
;; To overcome this limitation, we can first load the system, then set the type
;; to something more specific and reload the system to catch the type errors.

(export-always 'nyxt-keymap-value)
#+nyxt-debug-keymap
(deftype nyxt-keymap-value ()
  '(or keymap:keymap nyxt:function-symbol nyxt::command))

#-nyxt-debug-keymap
(deftype nyxt-keymap-value ()
  '(or keymap:keymap t))

(export-always 'make-scheme-name)
(defun make-scheme-name (name &rest parents)
  "Return a new `scheme-name' object, with `nyxt-keymap-value' value type.
The scheme name inherits from the optional PARENTS, ordered by priority.

Example:

  (defvar emacs (make-scheme-name \"emacs\" cua))

In the above, we define a new scheme name called `emacs` which inherits from the
existing name `cua`."
  (the (values keymap:scheme-name &optional)
       (make-instance 'keymap:scheme-name
                      :name name
                      :parents parents
                      :bound-type 'nyxt-keymap-value)))

(export-always 'cua)
(defvar cua (make-scheme-name "cua"))
(export-always 'emacs)
(defvar emacs (make-scheme-name "emacs" cua))
(export-always 'vi-normal)
(defvar vi-normal (make-scheme-name "vi-normal" cua emacs))
(export-always 'vi-insert)
(defvar vi-insert (make-scheme-name "vi-insert" cua))
