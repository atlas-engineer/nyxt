;;; vi.lisp --- VI-style bindings.

(in-package :next)

(define-mode vi-normal-mode ()
    "Mode for displaying documentation."
    ((keymap-schemes
      :initform
      (let ((map (make-keymap)))
        (define-key
          "i" 'vi-insert-mode
          :keymap map)
        (list :vi-normal map))))
  (let ((active-buffer (active-buffer *interface*)))
    (vi-insert-mode (first (modes active-buffer)) :activate nil)
    (setf (current-keymap-scheme active-buffer) :vi-normal)
    (echo (minibuffer *interface*) "VI normal mode")))

;; TODO: Move ESCAPE binding to the override map.
(define-mode vi-insert-mode ()
    "Mode for displaying documentation."
    ((keymap-schemes
      :initform
      (let ((map (make-keymap)))
        (define-key "ESCAPE" 'vi-normal-mode
          :keymap map)
        (list :vi-insert map))))
  (let ((active-buffer (active-buffer *interface*)))
    (vi-normal-mode (first (modes (active-buffer *interface*))) :activate nil)
    (setf (current-keymap-scheme active-buffer) :vi-insert)
    (echo (minibuffer *interface*) "VI insert mode")))
