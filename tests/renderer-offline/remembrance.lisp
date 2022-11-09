;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(defun set-url-blocking (url &optional (buffer (nyxt:current-buffer)))
  (nyxt:buffer-load url)
  (let ((p (lpara:promise)))
    (hooks:once-on (nyxt:buffer-loaded-hook buffer) (_)
      (lpara:fulfill p))
    (lpara:force p)))

(defvar +url1+
  (quri:make-uri-file :path
                      (asdf:system-relative-pathname :nyxt "tests/test-data/hello.html")))
(defvar +url2+
  (quri:make-uri-file :path
                      (asdf:system-relative-pathname :nyxt "tests/test-data/lorem.html")))

(defmacro wait-on-handler (hook args &body body) ; TODO: Move to Nhooks?
  "Like `nhooks:once-on' but block until the handler returns.."
  (alex:with-gensyms (promise)
    (let ((handler-name (gensym "once-on-hook-handler"))
          (args (alexandria:ensure-list args)))
      (alexandria:once-only (hook)
        `(let ((,promise (lpara:promise)))
           (nhooks:add-hook
            ,hook (make-instance 'nhooks:handler
                                 :fn (lambda ,args
                                       (declare (ignorable ,@args))
                                       (nhooks:remove-hook ,hook (quote ,handler-name))
                                       (lpara:fulfill ,promise
                                         (progn ,@body)))
                                 :name (quote ,handler-name)))
           (lpara:force ,promise))))))

(define-test remembrance ()
  (with-headless
    (nyxt:start :no-config t :no-auto-config t
                :socket "/tmp/nyxt-test.socket"
                :profile "test")
    (wait-on-handler nyxt:*after-startup-hook* ()
      (nyxt:enable-modes 'nyxt/remembrance-mode:remembrance-mode (nyxt:current-buffer)))
    (let ((mode (nyxt:find-submode 'nyxt/remembrance-mode:remembrance-mode)))
      (assert-equality 'uiop:pathname-equal
                       (nfiles:join +test-root+
                                    (uiop:xdg-cache-home "nyxt" "remembrance.cache/"))
                       (nfiles:expand (nyxt/remembrance-mode::cache-path mode)))

      (set-url-blocking +url1+)
      (set-url-blocking +url2+)

      (assert-number-equal 2
                           (nyxt/remembrance-mode::cache-size mode)))
    (nyxt:quit)))
