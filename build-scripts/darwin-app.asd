;; The following system is used to create standalone (portable)
;; application bundles for Darwin with all dependencies included
;; (intended for distribution)- otherwise, the Makefile may be
;; utilized by package managers and others compiling from source
(asdf:defsystem :next/darwin/gtk-application
  :defsystem-depends-on (:deploy)
  :depends-on (:next/gtk)
  :build-operation "osx-app-deploy-op"
  :build-pathname "Next"
  :entry-point "next:entry-point"
  :pathname "../source/"
  :components ((:file "darwin-gtk")))
