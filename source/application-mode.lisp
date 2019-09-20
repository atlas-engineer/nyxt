;;; application-mode.lisp --- major mode for "web applications"
;;; has no history or bindings to avoid interference with "web applications"'

(in-package :next)

;; TODO: Finish it!  We can move the active modes to a list of backed up modes,
;; and have one "escape key" bound to a function that would restore the backup modes.

(define-mode application-mode ()
    "Mode that forwards all keys to the platform port."
  ())
