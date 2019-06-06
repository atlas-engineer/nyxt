;;; vi.lisp --- VI-style bindings.

(in-package :next)

;; TODO: Instead of defining bindings here, define bindings schemes in the
;; respective modes.  This should only set the scheme of the buffer.

(define-mode vi-normal-mode ()
    "Mode for displaying documentation."
    ((keymap
      :initform
      (let ((map (make-keymap)))
        (define-key "j" 'scroll-down
          "k" 'scroll-up
          "i" 'vi-insert-mode
          :keymap map)
        map)))
  (vi-insert-mode (first (modes (active-buffer *interface*))) :activate nil)
  (echo (minibuffer *interface*) "VI normal mode"))

(define-mode vi-insert-mode ()
    "Mode for displaying documentation."
    ((keymap
      :initform
      (let ((map (make-keymap)))
        (define-key "ESCAPE" 'vi-normal-mode
          :keymap map)
        map)))
  (vi-normal-mode (first  (modes (active-buffer *interface*))) :activate nil)
  (echo (minibuffer *interface*) "VI insert mode"))
