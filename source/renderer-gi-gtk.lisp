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

(setf +renderer+ "GI-GTK")
(handler-bind ((warning #'muffle-warning))
  (defun renderer-thread-p ()
    (string= "main thread" (bt:thread-name (bt:current-thread))))
  (defmethod ffi-initialize ((browser gtk-browser) urls startup-timestamp)
    "gtk:within-main-loop handles all the GTK initialization. On
    GNU/Linux, Next could hang after 10 minutes if it's not
    used. Conversely, on Darwin, if gtk:within-main-loop is used, no
    drawing happens. Drawing operations on Darwin MUST originate from
    the main thread, which the GTK main loop is not guaranteed to be
    on."
    (log:debug "Initializing GI-GTK Interface")
    (setf gtk-running-p t)
    (flet ((main-func ()
             (with-protect ("Error on GTK thread: ~a" :condition)
               (glib:g-set-prgname "nyxt")
               #+GTK-3-4
               (gdk:gdk-set-program-class "Nyxt")
               (gir:invoke ((gir:ffi "Gtk" "3.0") 'main)))))
          (finalize browser urls startup-timestamp)
          #-darwin
          (let ((main-thread (bt:make-thread #'main-func :name "main thread")))
            (unless *run-from-repl-p*
              (bt:join-thread main-thread)))
          #+darwin
          (main-func))))
