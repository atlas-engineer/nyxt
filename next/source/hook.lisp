;;; hook.lisp --- functions for adding and removing hooks

(in-package :next)

(defun add-hook (hook-name function function-designator))

(defun run-hook (hook-name))

(defun remove-hook ((hook-name function-designator)))
