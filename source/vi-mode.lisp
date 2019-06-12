;;; vi.lisp --- VI-style bindings.

(in-package :next)

(define-mode vi-normal-mode ()
    "Enable VI-style modal bindings (normal mode)"
    ((keymap-schemes
      :initform
      (let ((map (make-keymap)))
        (define-key
          "i" 'vi-insert-mode
          "Button1" 'vi-button1
          :keymap map)
        (list :vi-normal map)))
     (destructor
      :initform
      (lambda (mode)
        (setf (current-keymap-scheme (buffer mode))
              (get-default 'buffer 'current-keymap-scheme))
        (setf (forward-input-events (buffer mode))
              (get-default 'buffer 'current-keymap-scheme))
        (echo "VI normal mode disabled."))))
  (let ((active-buffer (buffer %mode)))
    (vi-insert-mode %mode :activate nil :buffer active-buffer)
    (setf (current-keymap-scheme active-buffer) :vi-normal)
    (setf (forward-input-events active-buffer) nil)
    (echo "VI normal mode.")))

;; TODO: Move ESCAPE binding to the override map?
(define-mode vi-insert-mode ()
    "Enable VI-style modal bindings (insert mode)"
    ((keymap-schemes
      :initform
      (let ((map (make-keymap)))
        (define-key :keymap map
          "Escape" 'vi-normal-mode
          "Button1" 'vi-button1)
        (list :vi-insert map)))
     (destructor
      :initform
      (lambda (mode)
        (setf (current-keymap-scheme (buffer mode))
              (get-default 'buffer 'current-keymap-scheme))
        (echo "VI insert mode disabled."))))
  (let ((active-buffer (buffer %mode)))
    (vi-normal-mode %mode :activate nil :buffer active-buffer)
    (setf (current-keymap-scheme active-buffer) :vi-insert)
    (echo "VI insert mode.")))

(define-parenscript %clicked-in-input? ()
  (ps:chain document active-element tag-name))

(define-command vi-button1 (root-mode)
  "Enable to VI insert mode when focus is on an input element on the web page."
  (%%generate-input-event *interface*
                          (%%window-active *interface*)
                          (first (last-key-chords
                                  (buffer root-mode))))
  (%clicked-in-input?
   :callback (lambda (response)
               (cond
                 ((and (string= response "INPUT")
                       (mode-active? (buffer root-mode) 'vi-normal-mode))
                  (vi-insert-mode root-mode))
                 ((and (not (string= response "INPUT"))
                       (mode-active? (buffer root-mode) 'vi-insert-mode))
                  (vi-normal-mode root-mode))))))
