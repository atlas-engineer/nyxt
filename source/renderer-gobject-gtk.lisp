;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun renderer-thread-p ()
  (string= "main thread" (bt:thread-name (bt:current-thread))))

(defmethod ffi-initialize ((browser gtk-browser) urls startup-timestamp)
  (log:info "Initializing Gobject-GTK Interface")
  (bt:make-thread (lambda ()
                    (gdk:gdk-set-program-class "nyxt")
                    (gir:invoke ((gir:ffi "Gtk") 'main)))
                  :name "main thread")
  (finalize browser urls startup-timestamp))
