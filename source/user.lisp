;;; user.lisp --- user configuration

(in-package :next)

;; utility functions for getting paths from the xdg directory
;; specification in a namespaced directory for Next
(defun xdg-data-home (&optional (file-name ""))
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "next"))
    (uiop:xdg-data-home))))

(defun xdg-config-home (&optional (file-name ""))
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "next"))
    (uiop:xdg-config-home))))

