;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(defparameter *window* (qnew "QWidget" "windowTitle" "nEXT"))
(defparameter *layout* (qnew "QGridLayout"))

(setf *mini-buffer* (generate-new-buffer "mini-buffer" (minibuffer-mode)))
(setf *active-buffer* (generate-new-buffer "default" (document-mode)))

;; Used by QT to capture key presses
(qadd-event-filter nil |QEvent.KeyPress| 'key-press)
(qadd-event-filter nil |QEvent.KeyRelease| 'key-release)

(defun start ()
  ;; remove margins around root widgets
  (|setSpacing| *layout* 0)
  (|setContentsMargins| *layout* 0 0 0 0)
   ;; row, column, rowspan, colspan
  (|addWidget| *layout* (buffer-view *active-buffer*) 0 0 1 1)
  (|addWidget| *layout* (buffer-view *mini-buffer*)   1 0 1 1)
  
  (|setLayout| *window* *layout*)
  (|show| *window*))

;; start nEXT
(start)

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)
