;;;; gtk.lisp --- gtk interface
(in-package :interface)

(defparameter *next-interface* nil)
(defparameter *character-conversion-table* (make-hash-table :test 'equalp))

(defclass next-interface ()
  ((window :accessor window :initarg :window)
   (container-view :accessor container-view :initarg :container-view)
   (minibuffer-view :accessor minibuffer-view :initarg :minibuffer-view)))

(defclass minibuffer-view ()
  ((container-view :accessor container-view :initarg :container-view)
   (input-entry :accessor input-entry :initarg :input-entry)
   (completion-model :accessor completion-model :initarg :completion-model)
   (completion-view :accessor completion-view :initarg :completion-view)
   (completion-function :accessor completion-function)))

(defmethod completions-clear ((self minibuffer-view))
  (gtk:gtk-list-store-set (completion-model self)
                          (gtk:gtk-list-store-clear
                           (completion-model self))))

(defmethod completions-add ((self minibuffer-view) elements)
  (loop for element in elements do
       (gtk:gtk-list-store-set
        (completion-model self)
        (gtk:gtk-list-store-append (completion-model self))
        (write-to-string element))))

(defmethod get-input ((self minibuffer-view))
  (gtk:gtk-entry-text (input-entry self)))

(defmethod process-set-completions ((self minibuffer-view))
  "Process and set completions for the minibuffer"
  (with-slots (completion-function completion-model completion-viw) self
    (when (completion-function self)
      (completions-clear self)
      (let ((completions (funcall completion-function (get-input self))))
        (when completions
          (completions-add self completions)
          (highlight-completion self 0))))))

(defmethod highlight-completion ((self minibuffer-view) index)
  (gtk:gtk-tree-view-set-cursor
           (completion-view self)
           (gtk:gtk-tree-path-new-from-string (write-to-string index))))

(defun initialize ()
  (setf (gethash #\Return *character-conversion-table*) "RETURN")
  (setf (gethash #\- *character-conversion-table*) "HYPHEN"))

(defun start ()
  (gtk:within-main-loop
   (let* ((window
           (make-instance 'gtk:gtk-window
                          :type :toplevel
                          :title "nEXT"
                          :default-width 250
                          :border-width 0))
          (button
           (make-instance 'gtk:gtk-button
                          :label "Browser View Placeholder"))
          (entry
           (make-instance 'gtk:gtk-entry
                          :text "Minibuffer Input"))
          (root-box
           (make-instance 'gtk:gtk-box
                          :orientation :vertical
                          :spacing 0))
          (minibuffer-box
           (make-instance 'gtk:gtk-box
                          :orientation :vertical
                          :spacing 0))
          (model
           (make-instance 'gtk:gtk-list-store
                          :column-types '("gchararray")))
          (list-view
           (make-instance 'gtk:gtk-tree-view
                          :model model))
          (renderer (gtk:gtk-cell-renderer-text-new))
          (column (gtk:gtk-tree-view-column-new-with-attributes
                   "Name" renderer "text" 0)))
     (gtk:gtk-tree-view-append-column list-view column)
     (gtk:gtk-list-store-set model (gtk:gtk-list-store-append model) "Element 1")
     (gobject:g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (gtk:leave-gtk-main)))
     (gtk:gtk-box-pack-start root-box button)
     (gtk:gtk-box-pack-start root-box minibuffer-box :expand nil)
     (gtk:gtk-box-pack-start minibuffer-box entry :expand nil)
     (gtk:gtk-box-pack-start minibuffer-box list-view :expand nil)
     (gtk:gtk-container-add window root-box)
     (setf *next-interface* (make-instance
                             'next-interface
                             :window window
                             :minibuffer-view (make-instance
                                               'minibuffer-view
                                               :container-view minibuffer-box
                                               :input-entry entry
                                               :completion-model model
                                               :completion-view list-view)))
     (gobject:g-signal-connect
      window "key_press_event"
      (lambda (window event)
        (declare (ignore window))
        (process-event event)))
     (gobject:g-signal-connect
      entry "changed"
      (lambda (widget)
        (declare (ignore widget))
        (process-set-completions (minibuffer-view *next-interface*))))
     (gtk:gtk-widget-show-all window)
     (minibuffer-hide))))

(defun kill ()
  (quit))

(defun copy ())
(defun paste ())
(defun cut ())

(defun process-event (event)
  (let* ((modifier-state (gdk:gdk-event-key-state event))
         (character (gdk:gdk-keyval-to-unicode (gdk:gdk-event-key-keyval event)))
         (mapped-character (gethash character *character-conversion-table* character)))
    (unless (equalp character #\Null)
      (next:push-key-chord
       (member :control-mask modifier-state :test #'equalp)
       (member :mod1-mask modifier-state :test #'equalp)
       (member :super-mask modifier-state :test #'equalp)
       (string mapped-character)))))

(defun set-visible-view (view)
  (declare (ignore view)))

(defun delete-view (view)
  (declare (ignore view)))

(defun make-web-view ())

(defun web-view-set-url (view url)
  (declare (ignore view url)))

(defun web-view-set-url-loaded-callback (view function)
  (declare (ignore view function)))

(defun web-view-get-url (view)
  (declare (ignore view))
  "url.com")

(defun web-view-execute (view script)
  (declare (ignore view script))
  "{\"hello\": \"world\"}")

(defun make-minibuffer ()
  (container-view (minibuffer-view *next-interface*)))

(defun minibuffer-show ()
  (gtk:gtk-widget-show (container-view (minibuffer-view *next-interface*)))
  (gtk:gtk-widget-grab-focus (input-entry (minibuffer-view *next-interface*))))

(defun minibuffer-hide ()
  (gtk:gtk-widget-hide (container-view (minibuffer-view *next-interface*))))

(defun minibuffer-set-input (input)
  (setf (gtk:gtk-entry-text (input-entry (minibuffer-view *next-interface*))) input))

(defun minibuffer-get-input ()
  (gtk:gtk-entry-text (input-entry (minibuffer-view *next-interface*))))

(defun minibuffer-get-input-complete ()
  (with-slots (completion-model completion-view) (minibuffer-view *next-interface*)
    (when (gtk:gtk-tree-model-get-iter
           completion-model
           (gtk:gtk-tree-view-get-cursor
            completion-view))
      (gtk:gtk-tree-model-get-value
       completion-model
       (gtk:gtk-tree-model-get-iter
        completion-model
        (gtk:gtk-tree-view-get-cursor
         completion-view))
       0))))

(defun minibuffer-select-next ()
  (gtk:gtk-tree-view-set-cursor
   (completion-view (minibuffer-view *next-interface*))
   (gtk:gtk-tree-path-next
    (gtk:gtk-tree-view-get-cursor
     (completion-view (minibuffer-view *next-interface*))))))

(defun minibuffer-select-previous ()
  (gtk:gtk-tree-view-set-cursor
   (completion-view (minibuffer-view *next-interface*))
   (gtk:gtk-tree-path-prev
    (gtk:gtk-tree-view-get-cursor
     (completion-view (minibuffer-view *next-interface*))))))

(defun minibuffer-set-completion-function (function)
  (setf (completion-function (minibuffer-view *next-interface*)) function))
