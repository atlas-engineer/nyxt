(in-package :next/engine)

(defgeneric cache (type uri))

(defun resolve (uri)
  "Resolve and locally cache URI"
  (cache :uri uri))

