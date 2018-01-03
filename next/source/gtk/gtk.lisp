;;;; gtk.lisp --- gtk interface
(in-package :interface)

(defparameter *cookie-path-dir* "~/.next.d/")
(defparameter *cookie-type* :webkit-cookie-persistent-storage-text)
(defparameter *cookie-accept-policy* :webkit-cookie-policy-accept-always)

(defparameter *next-interface* nil)
(defparameter *character-conversion-table* (make-hash-table :test 'equalp))

(defclass next-interface ()
  ((window :accessor window :initarg :window)
   (container-view :accessor container-view :initarg :container-view)
   (minibuffer-view :accessor minibuffer-view :initarg :minibuffer-view)))

(defclass minibuffer-view ()
  ((container-view :accessor container-view :initarg :container-view)
   (input-entry :accessor input-entry :initarg :input-entry)
   (completions :accessor completions)
   (completion-model :accessor completion-model :initarg :completion-model)
   (completion-view :accessor completion-view :initarg :completion-view)
   (completion-function :accessor completion-function)))

(defmethod completions-clear ((self minibuffer-view))
  (gtk:gtk-list-store-clear
   (completion-model self)))

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
          (setf (completions self) completions)
          (highlight-completion self 0))))))

(defmethod highlight-completion ((self minibuffer-view) index)
  (gtk:gtk-tree-view-set-cursor
           (completion-view self)
           (gtk:gtk-tree-path-new-from-string (write-to-string index))))

(defvar *within-main-nesting* nil)

(defmacro synchronous-within-main (&body b)
  (let ((promise (gensym "PROMISE"))
	(thunk (gensym "THUNK")))
  `(let ((,thunk (lambda () ,@b)))
     (format *error-output* "S-W-M~%")
     (if (or
	  *within-main-nesting*
	  (string= (bt:thread-name (bt:current-thread))
		   "cl-cffi-gtk main thread"))
	 (funcall ,thunk)
	 (let ((*within-main-nesting* t)
	       (,promise (lparallel:promise)))
	   (gtk:within-main-loop
	     (lparallel:fulfill ,promise
	       (funcall ,thunk)))
	   (lparallel:force ,promise))))))

(defun initialize ()
  (setf (gethash #\Return *character-conversion-table*) "RETURN")
  (setf (gethash #\- *character-conversion-table*) "HYPHEN"))

(defun start ()
  (synchronous-within-main
   (let* ((window
           (make-instance 'gtk:gtk-window
                          :type :toplevel
                          :title "nEXT"
                          :default-width 1024
                          :default-height 768
                          :border-width 0))
          (entry
           (make-instance 'gtk:gtk-entry
                          :text "Minibuffer Input"))
          (root-box
           (make-instance 'gtk:gtk-box
                          :orientation :vertical
                          :spacing 0))
          (container-box
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
     (gobject:g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (gtk:leave-gtk-main)))
     (gtk:gtk-box-pack-start root-box container-box)
     (gtk:gtk-box-pack-start root-box minibuffer-box :expand nil)
     (gtk:gtk-box-pack-start minibuffer-box entry :expand nil)
     (gtk:gtk-box-pack-start minibuffer-box list-view :expand nil)
     (gtk:gtk-container-add window root-box)
     (setf *next-interface* (make-instance
                             'next-interface
                             :window window
                             :container-view container-box
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
  (synchronous-within-main
    (gtk:gtk-container-foreach
     (container-view *next-interface*)
     (lambda (widget) (gtk:gtk-container-remove (container-view *next-interface*) widget)))
    (gtk:gtk-box-pack-start (container-view *next-interface*) view)
    (gtk:gtk-widget-show view)))

(defun delete-view (view)
  (declare (ignore view)))

(defun make-default-context ()
  (let* ((ctx (cl-webkit2:webkit-web-context-get-default))
         (cm  (cl-webkit2:webkit-web-context-get-cookie-manager ctx)))
    (cl-webkit2:webkit-cookie-manager-set-accept-policy
     cm *cookie-accept-policy*)
    (cl-webkit2:webkit-cookie-manager-set-persistent-storage
     cm (namestring (merge-pathnames "cookies" *cookie-path-dir*)) *cookie-type*)
    ctx))

(defun make-web-view ()
  (synchronous-within-main
    (let* ((ctx (make-default-context))
	   (wv (make-instance 'webkit2:webkit-web-view :context ctx)))
      wv)))

(defun web-view-set-url (view url)
  (webkit2:webkit-web-view-load-uri view url))

(defun web-view-set-url-loaded-callback (view function)
  (declare (ignore view function)))

(defun web-view-get-url (view)
  (webkit2:webkit-web-view-uri view))

(defun web-view-execute (view script)
  (let ((np (cffi:null-pointer)))
    (webkit2:webkit-web-view-run-javascript view script np np np))
  "{\"hello\": \"world\"}")

(defun make-minibuffer ()
  (container-view (minibuffer-view *next-interface*)))

(defun minibuffer-show ()
  (gtk:gtk-widget-show (container-view (minibuffer-view *next-interface*)))
  (gtk:gtk-widget-grab-focus (input-entry (minibuffer-view *next-interface*)))
  (process-set-completions (minibuffer-view *next-interface*)))

(defun minibuffer-hide ()
  (completions-clear (minibuffer-view *next-interface*))
  (gtk:gtk-widget-hide (container-view (minibuffer-view *next-interface*))))

(defun minibuffer-set-input (input)
  (setf (gtk:gtk-entry-text (input-entry (minibuffer-view *next-interface*))) input))

(defun minibuffer-get-input ()
  (gtk:gtk-entry-text (input-entry (minibuffer-view *next-interface*))))

(defun minibuffer-get-input-complete ()
  (with-slots (completion-view completions) (minibuffer-view *next-interface*)
    (let ((path (gtk:gtk-tree-path-to-string
                 (gtk:gtk-tree-view-get-cursor completion-view))))
      (if path
          (nth (parse-integer path) completions)
          nil))))

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
