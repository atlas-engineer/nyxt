;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(defparameter *window* nil)
(defparameter *root-layout* nil)
(defparameter *stack-layout* nil)

;; Used by QT to capture key presses
(qadd-event-filter nil |QEvent.KeyPress| 'key-press)
(qadd-event-filter nil |QEvent.KeyRelease| 'key-release)

(defun start ()
  (setf *window* (qnew "QWidget" "windowTitle" "nEXT")
         *root-layout* (qnew "QGridLayout")
         *stack-layout* (qnew "QStackedLayout"))
  (setf *minibuffer-prompt* (qnew "QLabel" "text" "input:")
        *minibuffer-input* (qnew "QLineEdit")
        *minibuffer-completion-model* (qnew "QStringListModel")
        *minibuffer-completion* (qnew "QListView")
        *minibuffer* (generate-new-buffer "minibuffer" (minibuffer-mode) nil))
  (setf *active-buffer* (generate-new-buffer "default" (document-mode)))
  (setf *history-tree* (generate-new-buffer "history-tree" (tree-history-mode)))

  (initialize-keycodes)
  (initialize-keymap)

  (define-key minibuffer-mode-map (kbd "Return") #'return-input)
  (define-key minibuffer-mode-map (kbd "C-g") #'cancel-input)
  (define-key minibuffer-mode-map (kbd "Escape") #'cancel-input)

  (define-key global-map (kbd "C-x b") (:input-complete switch-buffer buffer-complete))
  (define-key global-map (kbd "C-x k") (:input-complete delete-buffer buffer-complete))

  (define-key document-mode-map (kbd "S-t") #'history-tree-show)
  (define-key document-mode-map (kbd "S-f")
    (:input-complete history-forwards-query history-fowards-query-complete))
  (define-key document-mode-map (kbd "S-b") #'history-backwards)
  (define-key document-mode-map (kbd "C-f") #'history-forwards)
  (define-key document-mode-map (kbd "C-b") #'history-backwards)
  (define-key document-mode-map (kbd "C-p") #'scroll-up)
  (define-key document-mode-map (kbd "C-n") #'scroll-down)
  (define-key document-mode-map (kbd "C-l") (:input set-url))
  (define-key global-map (kbd "S-l") (:input set-url-new-buffer))

  (define-key global-map (kbd "S-s k")
    (:input-complete bookmark-delete bookmark-complete))
  (define-key document-mode-map (kbd "S-s o")
    (:input-complete set-url bookmark-complete))
  (define-key document-mode-map (kbd "S-s s") #'bookmark-current-page)

  ;; remove margins around root widgets
  (|setSpacing| *root-layout* 0)
  (|setContentsMargins| *root-layout* 0 0 0 0)
   ;; arguments for grid layout: row, column, rowspan, colspan
  (|addLayout| *root-layout* *stack-layout*              0 0 1 1)
  (|addWidget| *root-layout* (buffer-view *minibuffer*)  1 0 1 1)

  (|hide| (buffer-view *minibuffer*))
  (|setLayout| *window* *root-layout*)
  (|show| *window*))
