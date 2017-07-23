#-eql5
(error "Please use the EQL5 executable (see README)")

(require :cmp)

(load "dependencies")

(push "./" asdf:*central-registry*)

(asdf:make-build "next"
                 :monolithic t
                 :type :static-library
                 :move-here "./")

(let ((lib-name #+msvc "next.lib"
                #-msvc "libnext.a"))
  (when (probe-file lib-name)
    (delete-file lib-name))
  (rename-file (x:cc "next--all-systems"
                      #+msvc ".lib"
                      #-msvc ".a")
               lib-name))
