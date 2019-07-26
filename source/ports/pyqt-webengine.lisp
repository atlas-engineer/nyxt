(in-package :next)

(setf (get-default 'port 'name) "next-pyqt-webengine")

(defmethod path ((port port))
  "Calculate the path of the port relative to the application bundle."
  (let* ((path (uiop/image:argv0))
         (path (cl-strings:shorten path (- (length path) 4) :truncate-string ""))
         (path (concatenate 'string path "next-pyqt-webengine")))
    path))
