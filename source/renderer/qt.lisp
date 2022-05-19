;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class qt-browser ()
  ((application :accessor application))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod ffi-initialize ((browser qt-browser) urls startup-timestamp)
  (log:debug "Initializing Qt Interface")
  (flet ((initialize ()
           (setf (application browser)
                 (qt:new-q-application 1 (cffi:foreign-alloc :string
                                                             :initial-contents (list "Nyxt")
                                                             :null-terminated-p t)))
           (finalize browser urls startup-timestamp)
           (qt:application-exec (application browser))))
    #+sbcl
    (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
      (log:debug "SBCL initialization")
      (initialize))
    #-sbcl
    (initialize)))

(defmethod ffi-kill-browser ((browser qt-browser))
  (qt:application-quit (application browser)))

(define-class qt-window ()
  ((qt-object)
   (box-layout)
   (minibuffer-view))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class qt-buffer ()
  ((qt-object))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod initialize-instance :after ((window qt-window) &key)
  (with-slots (id qt-object box-layout active-buffer minibuffer-view) window
    (setf id (get-unique-identifier *browser*))
    (setf qt-object (qt:new-q-widget))
    (setf box-layout (qt:new-qv-box-layout))
    (setf active-buffer (make-instance 'buffer))
    (setf minibuffer-view (qt:new-q-web-engine-view))
    ;; Add views to window, configure window widget
    (qt:widget-set-layout qt-object box-layout)
    (qt:layout-add-widget box-layout (qt-object active-buffer))
    (qt:layout-add-widget box-layout minibuffer-view)
    (qt:layout-set-contents-margins box-layout 0 0 0 0)
    (qt:layout-set-spacing box-layout 0)
    (ffi-window-set-prompt-buffer-height window 16)
    (qt:widget-resize qt-object 1024 768)
    (qt:widget-install-key-press-filter
     (qt-object window)
     (lambda (event)
       (on-signal-key-press-event window event)))
    (qt:widget-show qt-object)))

(defmethod printable-p ((window qt-window) event)
  "Return the printable value of EVENT."
  (match (qt:key-string event)
    (" " "space")
    ("-" "hyphen")
    (_ (qt:key-string event))))

(defmethod on-signal-key-press-event ((sender qt-window) event)
  (when (qt:key-string event)
    (alex:appendf (key-stack sender)
                  (list (keymap:make-key :code (qt:key-code event)
                                         :value (string-downcase (qt:key-string event))
                                         :modifiers (qt:modifiers event)
                                         :status :pressed)))
    (funcall (input-dispatcher sender)
             event (active-buffer sender)
             sender (printable-p sender event))))

(defmethod on-signal-destroy ((window qt-window))
  (window-delete window))

(defmethod ffi-window-delete ((window qt-window))
  "Delete a window object and remove it from the hash of windows.")

(defmethod ffi-window-fullscreen ((window qt-window))
  (qt:widget-show-full-screen (qt-object window)))

(defmethod ffi-window-unfullscreen ((window qt-window))
  (qt:widget-show-normal (qt-object window)))

(defmethod initialize-instance :after ((buffer qt-buffer) &key)
  (hooks:run-hook (buffer-before-make-hook *browser*) buffer)
  (setf (id buffer) (get-unique-identifier *browser*))
  (setf (qt-object buffer) (qt:new-q-web-engine-view))
  (qt:load-started-listener-connect
   (qt-object buffer)
   (lambda ()
     (on-signal-load-committed buffer (quri:uri (qt:web-engine-view-url (qt-object buffer))))))
  (qt:load-finished-listener-connect
   (qt-object buffer)
   (lambda ()
     (on-signal-load-finished buffer (quri:uri (qt:web-engine-view-url (qt-object buffer)))))))

(defmethod ffi-window-make ((browser qt-browser))
  "Make a window."
  (make-instance 'window))

(defmethod ffi-window-to-foreground ((window qt-window))
  "Show window in foreground."
  (qt:widget-present (qt-object window)))

(defmethod ffi-window-set-title ((window qt-window) title)
  "Set the title for a window."
  (qt:widget-set-window-title (qt-object window) title)
  title)

(defmethod ffi-window-active ((browser qt-browser))
  "Return the window object for the current window."
  (setf (slot-value browser 'last-active-window)
        (or (find-if #'qt:widget-is-active-window (window-list) :key #'qt-object)
            (slot-value browser 'last-active-window))))

(defmethod ffi-window-set-buffer ((window qt-window) (buffer qt-buffer))
  "Set BROWSER's WINDOW buffer to BUFFER."
  (qt:widget-set-parent (qt-object (active-buffer window)) (cffi:null-pointer))
  (qt:layout-insert-widget (box-layout window) 0 (qt-object buffer))
  (qt:widget-show (qt-object buffer))
  (setf (active-buffer window) buffer))

(defmethod ffi-window-set-prompt-buffer-height ((window qt-window) height)
  (qt:widget-set-fixed-height (minibuffer-view window) height))

(defmethod ffi-buffer-make ((browser qt-browser))
  (make-instance 'buffer))

(defmethod ffi-buffer-delete ((buffer qt-buffer)))

(defmethod ffi-buffer-load ((buffer qt-buffer) url)
  (qt:web-engine-view-load (qt-object buffer) (quri:render-uri url)))

(defmethod ffi-buffer-evaluate-javascript-async ((buffer qt-buffer) javascript)
  (qt:web-engine-page-run-javascript (qt:web-engine-view-page (qt-object buffer)) javascript %callback))

(defmethod ffi-prompt-buffer-evaluate-javascript-async ((window qt-window) javascript)
  (qt:web-engine-page-run-javascript (qt:web-engine-view-page (minibuffer-view window)) javascript %callback))

(defmethod ffi-generate-input-event ((window qt-window) event))

(defmethod ffi-generated-input-event-p ((window qt-window) event))

(defmethod ffi-within-renderer-thread ((browser qt-browser) thunk)
  (declare (ignore browser))
  ;; TODO: Test this!
  (funcall thunk))

(defun set-renderer ()
  (define-user-class window (qt-window))
  (define-user-class buffer (qt-buffer))
  (define-user-class browser (qt-browser)))

(set-renderer)
