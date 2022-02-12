;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/help-mode
  (:use :common-lisp :nyxt)
  (:import-from #:serapeum #:export-always)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for help pages"))
(in-package :nyxt/help-mode)

(define-mode help-mode ()
  "Mode for displaying documentation."
  ((rememberable-p nil)
   (inspected-values (tg:make-weak-hash-table :test 'equal)
                     :type hash-table
                     :allocation :class)))

(export-always 'inspected-value)
(defmethod inspected-value ((mode help-mode) id)
  (gethash id (inspected-values mode)))

(defmethod (setf inspected-value) (new-value (mode help-mode) id)
  (setf (gethash id (inspected-values mode)) new-value))
