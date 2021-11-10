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
  (let ((renderer-thread-name "Nyxt renderer thread"))
    (defun renderer-thread-p ()
      (string= (bt:thread-name (bt:current-thread))
               #+darwin
               "thread"
               #-darwin
               renderer-thread-name))
    (defmethod ffi-initialize ((browser gtk-browser) urls startup-timestamp)
      "On GNU/Linux we can create a separate thread to launch the GTK
interface. On Darwin, we must run the GTK thread on the main thread."
      (log:debug "Initializing GI-GTK Interface")
      (setf (uiop:getenv "WEBKIT_FORCE_SANDBOX") "0")
      (setf gtk-running-p t)
      (flet ((main-func ()
               (with-protect ("Error on GTK thread: ~a" :condition)
                 (glib:g-set-prgname "nyxt")
                 #+GTK-3-4
                 (gdk:gdk-set-program-class "Nyxt")
                 (gir:invoke ((gir:ffi "Gtk" "3.0") 'main)))))
        (finalize browser urls startup-timestamp)
        #-darwin
        (let ((main-thread (bt:make-thread #'main-func :name renderer-thread-name)))
          (unless *run-from-repl-p*
            (bt:join-thread main-thread)))
        #+darwin
        (main-func)))

    (define-ffi-method ffi-kill-browser ((browser gtk-browser))
      (unless *run-from-repl-p*
        (gir:invoke ((gir:ffi "Gtk" "3.0") 'main-quit))))))
