;;; darwin-gtk.lisp --- source for standalone Darwin application bundles

(in-package :next)

(deploy:define-hook (:deploy copy-webkit-executables) (directory)
  (let ((webkit-executables (list "WebKitNetworkProcess"
                                  "WebKitPluginProcess"
                                  "WebKitWebProcess"
                                  "jsc"))
        (webkit-executable-path (or (uiop:getenv "WEBKIT_EXEC_PATH")
                                    "/opt/local/libexec/webkit2gtk-4.0/")))
    (loop for executable in webkit-executables do
      (uiop:copy-file (format nil "~a~a" webkit-executable-path executable)
                      (format nil "~a/~a" directory executable)))))
