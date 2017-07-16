#-eql5
(error "Please use the EQL5 executable (see README)")

(require :cmp)

#+msvc
(setf c::*compile-in-constants* t)

;; load all lisp files for compilation
(defparameter *lisp-files*
  (list "base" "qt" "keymap")
  "All Lisp files of the application.")

(dolist (f *lisp-files*)
  (let ((file (format nil "lisp/~A" f)))
    (load file)
    (compile-file file :system-p t)))

(c:build-static-library "next"
                        :lisp-files (mapcar (lambda (file)
                                              (format nil "lisp/~A.~A" file #+msvc "obj" #-msvc "o"))
                                            *lisp-files*)
			:init-name "ini_app"
                        :epilogue-code '(next:start))

(eql:qq)
