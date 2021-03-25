;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; This file produces two warnings due to redefinitions of
;;;; renderer-thread-p and ffi-initialize.
;;;;
;;;; This is necessary to modify the behavior of renderer-gtk to use
;;;; gobject to launch/manage the main GTK thread. When/if this
;;;; renderer will fully utilize gobject introspection (by taking the
;;;; changes made on the respectively named branch, and reapplying
;;;; them), then these errors will disappear, as there will be no
;;;; reliance on renderer-gtk (and thus, no redefinition).

(in-package :nyxt)

(handler-bind ((warning #'muffle-warning))
  (defun renderer-thread-p ()
    (string= "main thread" (bt:thread-name (bt:current-thread))))
  (defmethod ffi-initialize ((browser gtk-browser) urls startup-timestamp)
    (log:debug "Initializing Gobject-GTK Interface")
    (setf gtk-running-p t)
    (let ((main-thread (bt:make-thread
                        (lambda ()
                          (with-protect ("Error on GTK thread: ~a" :condition)
                            (glib:g-set-prgname "nyxt")
                            (gdk:gdk-set-program-class "Nyxt")
                            (gir:invoke ((gir:ffi "Gtk") 'main))))
                        :name "main thread")))
      (finalize browser urls startup-timestamp)
      (unless *keep-alive*
        (bt:join-thread main-thread)))))
