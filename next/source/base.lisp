;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(defun start ()
  (ensure-directories-exist (uiop:physicalize-pathname #P"~/.next.d/"))
  (initialize-bookmark-db)
  (initialize-default-key-bindings)
  (interface:initialize)
  (interface:start)
  ;; create the default buffers
  (setf *minibuffer* (generate-new-buffer "minibuffer" (minibuffer-mode) nil))
  (set-visible-active-buffer (generate-new-buffer "default" (document-mode)))
  ;; load the user configuration if it exists
  (load "~/.next.d/init.lisp" :if-does-not-exist nil))

(defun initialize-default-key-bindings ()
  (define-key global-map (kbd "C-x C-c") #'interface:kill)
  (define-key minibuffer-mode-map (kbd "Return") #'return-input)
  (define-key minibuffer-mode-map (kbd "C-g") #'cancel-input)
  (define-key minibuffer-mode-map (kbd "Escape") #'cancel-input)
  (define-key global-map (kbd "C-x b") (:input-complete switch-buffer buffer-complete))
  (define-key global-map (kbd "C-x k") (:input-complete delete-buffer buffer-complete))
  (define-key document-mode-map (kbd "S-f") (:input-complete history-forwards-query history-fowards-query-complete))
  (define-key document-mode-map (kbd "S-b") #'history-backwards)
  (define-key document-mode-map (kbd "C-f") #'history-forwards)
  (define-key document-mode-map (kbd "C-b") #'history-backwards)
  (define-key document-mode-map (kbd "C-p") #'scroll-up)
  (define-key document-mode-map (kbd "C-n") #'scroll-down)
  (define-key document-mode-map (kbd "C-l") (:input set-url))
  (define-key global-map (kbd "C-o") (:input set-url-new-buffer))
  (define-key global-map (kbd "S-s k") (:input-complete bookmark-delete bookmark-complete))
  (define-key document-mode-map (kbd "S-s o") (:input-complete set-url bookmark-complete))
  (define-key document-mode-map (kbd "S-s s") #'bookmark-current-page))
