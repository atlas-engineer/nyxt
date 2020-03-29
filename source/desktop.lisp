(in-package :next)

(serapeum:export-always 'xdg-data-home)
(defun xdg-data-home (&optional (file-name ""))
  "Return $XDG_DATA_HOME/next as per XDG directory specification.
FILE-NAME is appended to the result."
  (merge-pathnames
   file-name
   (uiop:xdg-data-home "next/")))

(serapeum:export-always 'xdg-config-home)
(defun xdg-config-home (&optional (file-name ""))
  "Return $XDG_CONFIG_HOME/next as per XDG directory specification.
FILE-NAME is appended to the result."
  (merge-pathnames
   file-name
   (uiop:xdg-config-home "next/")))
