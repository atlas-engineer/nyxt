;;;; make.lisp --- create binary files for nEXT

;;;; Pre-requisties:
;;;; - dylibbundler (available via ports)
;;;; - ecl (available at the official website)
;;;; - eql (available at the official repository)
;;;;
;;;; See Next/next/README.org for more information on installing the
;;;; dependencies necessary to build nEXT from source
;;;;
;;;; Please note that this script must be run from the directory
;;;; Next/next. It may be necessary to modify the paths or commands in
;;;; the "Bundle OSX Dependencies" List.

#-eql5
(error "Please use the EQL5 executable (see README)")
(require :cmp)
(push "./" asdf:*central-registry*)
(ql:quickload "next")

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
(ext:run-program "qmake" nil :output t)

;; execute make
(ext:run-program "make" nil :output t)


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

;;;; Prompt the User to Bundle Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format *query-io* "[0] Quit [1] Bundle OSX Dependencies: ")
(force-output *query-io*)
(let ((input (read-line *query-io*)))
  (cond ((equalp input "0") )
	((equalp input "1")
	 (progn
	   (ext:run-program
	    "dylibbundler"
	    '("-cd"
	      "-b"
	      "-x"
	      "./next.app/Contents/MacOS/next"
	      "-d"
	      "./next.app/Contents/libs") :input t :output t)
	   
	   (ext:run-program
	    "macdeployqt"
	    '("./next.app/"
	      "-libpath=/usr/local/lib") :output t)

	   (ext:run-program
	    "install_name_tool"
	    '("-change"
	      "@libdir@/libecl.16.1.dylib"
	      "@executable_path/../libs/libecl.16.1.dylib"
	      "./next.app/Contents/MacOS/next") :output t)))))

(qquit)
