(in-package :next)

(setf +platform-port-command+ "next-pyqt-webengine")

(defmethod path ((port port))
  "Calculate the path of the port relative to the application bundle."
  (let* ((path (uiop/image:argv0))
         (path (subseq path 0 (-  (length path) 4)))
         (path (concatenate 'string path "next-pyqt-webengine")))
    path))
