#-eql5
(error "Please use the EQL5 executable (see README)")
(require :cmp)
(load "dependencies")
(push "./" asdf:*central-registry*)

;; build static library 
(asdf:make-build "next"
                 :monolithic t
                 :type :static-library
                 :move-here "./")

;; rename static library
(let ((lib-name #+msvc "next.lib"
                #-msvc "libnext.a"))
  (when (probe-file lib-name)
    (delete-file lib-name))
  (rename-file (x:cc "next--all-systems"
		     #+msvc ".lib"
		     #-msvc ".a")
               lib-name))

;; execute qmake
(ext:run-program "qmake" nil)

;; execute make
(ext:run-program "make" nil)


;;;; Cleanup Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete tmp dir to force recompilation
(uiop:delete-directory-tree
 (merge-pathnames (make-pathname
		   :directory '(:relative "tmp"))
		  (asdf:system-source-directory :next))
 :validate T
 :if-does-not-exist :ignore)
;; delete libnext.a
(uiop:delete-file-if-exists
 (format nil "~Alibnext.a" (asdf:system-source-directory :next)))
;; delete Makefile
(uiop:delete-file-if-exists
 (format nil "~AMakefile" (asdf:system-source-directory :next)))

(qquit)
