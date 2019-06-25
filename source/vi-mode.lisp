;;; vi.lisp --- VI-style bindings.

(in-package :next)

(define-mode vi-normal-mode ()
    "Enable VI-style modal bindings (normal mode)"
    ((keymap-schemes
      :initform
      (let ((map (make-keymap)))
        (define-key
          "i" 'vi-insert-mode
          "button1" 'vi-button1
          :keymap map)
        (list :vi-normal map)))
     (destructor
      :initform
      (lambda (mode)
        (setf (current-keymap-scheme (buffer mode))
              (get-default 'buffer 'current-keymap-scheme))
        (setf (forward-input-events (buffer mode))
              (get-default 'buffer 'current-keymap-scheme)))))
  (let ((active-buffer (buffer %mode)))
    (vi-insert-mode %mode :activate nil :buffer active-buffer)
    (setf (current-keymap-scheme active-buffer) :vi-normal)
    (setf (forward-input-events active-buffer) nil)))

;; TODO: Move ESCAPE binding to the override map?
(define-mode vi-insert-mode ()
    "Enable VI-style modal bindings (insert mode)"
    ((keymap-schemes
      :initform
      (let ((map (make-keymap)))
        (define-key :keymap map
          "ESCAPE" 'vi-normal-mode
          "button1" 'vi-button1)
        (list :vi-insert map)))
     (destructor
      :initform
      (lambda (mode)
        (setf (current-keymap-scheme (buffer mode))
              (get-default 'buffer 'current-keymap-scheme)))))
  (let ((active-buffer (buffer %mode)))
    (vi-normal-mode %mode :activate nil :buffer active-buffer)
    (setf (current-keymap-scheme active-buffer) :vi-insert)))

(define-parenscript %clicked-in-input? ()
  (ps:chain document active-element tag-name))

(define-command vi-button1 (root-mode)
  "Enable to VI insert mode when focus is on an input element on the web page."
  (rpc-generate-input-event *interface*
                          (rpc-window-active *interface*)
                          (first (last-key-chords
                                  (buffer root-mode))))
  (%clicked-in-input?
   :callback (lambda (response)
               (cond
                 ((and (string= response "INPUT")
                       (find-mode (buffer root-mode) 'vi-normal-mode))
                  (vi-insert-mode root-mode))
                 ((and (not (string= response "INPUT"))
                       (find-mode (buffer root-mode) 'vi-insert-mode))
                  (vi-normal-mode root-mode))))))
