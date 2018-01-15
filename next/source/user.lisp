;;; user.lisp --- user configuration

(in-package :next)

;; utility functions for getting paths from the xdg directory
;; specification in a namespaced directory for next
(defun xdg-data-home (&optional (rel ""))
  (uiop:xdg-data-home (concatenate 'string "next/" rel)))

(defun xdg-config-home (&optional (rel ""))
  (uiop:xdg-config-home (concatenate 'string "next/" rel)))

