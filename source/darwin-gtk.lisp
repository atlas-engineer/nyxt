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
      (let ((executable-target (format nil "~a/~a" directory executable)))
        (uiop:copy-file (format nil "~a~a" webkit-executable-path executable)
                        executable-target)
        (shared-library-extract:process-executable executable-target)))))

(deploy:define-hook (:boot set-environment-paths) (directory)
  (unless (uiop:getenv "WEBKIT_EXEC_PATH")
    (setf (uiop:getenv "WEBKIT_EXEC_PATH") (format nil "~a../Resources" directory))
    (format t "WEBKIT_EXEC_PATH ~a ~%" (uiop:getenv "WEBKIT_EXEC_PATH")))
  (unless (uiop:getenv "FONTCONFIG_PATH")
    (setf (uiop:getenv "FONTCONFIG_PATH") "/opt/X11/lib/X11/fontconfig")
    (format t "FONTCONFIG_PATH ~a ~%" (uiop:getenv "FONTCONFIG_PATH"))))
