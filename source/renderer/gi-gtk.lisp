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

(nyxt:define-package :nyxt/renderer/gi-gtk
    (:documentation "GTK renderer leveraging GObject Introspection.
For now it is also partly based on `nyxt/renderer/gtk'."))
(in-package :nyxt/renderer/gi-gtk)

(setf nyxt::+renderer+ "GI-GTK")
(push :nyxt-gi-gtk *features*)
(handler-bind ((warning #'muffle-warning))
  (let ((renderer-thread-name "Nyxt renderer thread"))
    (defmethod nyxt/renderer/gtk::renderer-thread-p ((renderer (eql "GI-GTK")) &optional (thread (bt:current-thread)))
      (string= (bt:thread-name thread)
               #+darwin
               "thread"
               #-darwin
               renderer-thread-name))
    (defmethod ffi-initialize ((browser nyxt/renderer/gtk:gtk-browser) urls startup-timestamp)
      "On GNU/Linux we can create a separate thread to launch the GTK
interface. On Darwin, we must run the GTK thread on the main thread."
      (declare (ignore urls startup-timestamp))
      (log:debug "Initializing GI-GTK Interface")
      (setf (uiop:getenv "WEBKIT_FORCE_SANDBOX") "0")
      ;; TODO: Do not run the GTK loop again when T?
      (setf nyxt/renderer/gtk::gtk-running-p t)
      (flet ((main-func ()
               (with-protect ("Error on GI-GTK thread: ~a" :condition)
                 (glib:g-set-prgname "nyxt")
                 #+GTK-3-4
                 (gdk:gdk-set-program-class "Nyxt")
                 (gir:invoke ((gir:ffi "Gtk" "3.0") 'main)))))
        (call-next-method)
        #-darwin
        (let ((main-thread (bt:make-thread #'main-func :name renderer-thread-name)))
          (unless nyxt::*run-from-repl-p*
            (bt:join-thread main-thread)
            ;; See comment about FreeBSD in gtk.lisp
            (uiop:quit (slot-value browser 'nyxt::exit-code) #+freebsd nil)))
        #+darwin
        (main-func)))

    (nyxt/renderer/gtk:define-ffi-method ffi-kill-browser ((browser nyxt/renderer/gtk:gtk-browser))
      (unless nyxt::*run-from-repl-p*
        (gir:invoke ((gir:ffi "Gtk" "3.0") 'main-quit))))))
