;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(defparameter *window* (qnew "QWidget" "windowTitle" "nEXT"))
(defparameter *root-layout* (qnew "QGridLayout"))
(defparameter *stack-layout* (qnew "QStackedLayout"))

(setf *minibuffer* (generate-new-buffer "mini-buffer" (minibuffer-mode) nil))
(setf *active-buffer* (generate-new-buffer "default" (document-mode)))

;; Used by QT to capture key presses
(qadd-event-filter nil |QEvent.KeyPress| 'key-press)
(qadd-event-filter nil |QEvent.KeyRelease| 'key-release)

(defun start ()
  ;; remove margins around root widgets
  (|setSpacing| *root-layout* 0)
  (|setContentsMargins| *root-layout* 0 0 0 0)
   ;; row, column, rowspan, colspan
  (|addLayout| *root-layout* *stack-layout*              0 0 1 1)
  (|addWidget| *root-layout* (buffer-view *minibuffer*)  1 0 1 1)
  
  (|hide| (buffer-view *minibuffer*))
  (|setLayout| *window* *root-layout*)
  (|show| *window*))

;; start nEXT
(start)

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)
