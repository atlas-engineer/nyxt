;; The following system is used to create standalone (portable)
;; application bundles for Darwin with all dependencies included
;; (intended for distribution)- otherwise, the Makefile may be
;; utilized by package managers and others compiling from source
(asdf:defsystem :nyxt/darwin/gtk-application
  :defsystem-depends-on (:deploy)
  :depends-on (:nyxt/gtk)
  :build-operation "osx-app-deploy-op"
  :build-pathname "Nyxt"
  :entry-point "nyxt:entry-point"
  :pathname "../source/"
  :components ((:file "darwin-gtk")))
