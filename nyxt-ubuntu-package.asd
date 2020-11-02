(defsystem "nyxt-ubuntu-package"
  :defsystem-depends-on ("linux-packaging")
  :class "linux-packaging:deb"
  :build-operation "linux-packaging:build-op"
  :depends-on ("nyxt/gtk")
  :package-name "nyxt"
  :version "2" ; Pre-release 3
  :author "Atlas Engineer LLC"
  :homepage "https://nyxt.atlas.engineer"
  :description "Extensible web-browser in Common Lisp"
  :license "BSD 3-Clause"
  :additional-dependencies ("glib-networking"
                            "gsettings-desktop-schemas"
                            "xclip"
                            "enchant"
                            ;; TODO: Remove notify-osd?
                            "notify-osd")
  :additional-files (("assets/nyxt.desktop" . "usr/share/applications/")
                     ("assets/nyxt_16x16.png" . #p"usr/share/icons/hicolor/16x16/apps/nyxt.png")
                     ("assets/nyxt_32x32.png" . #p"usr/share/icons/hicolor/32x32/apps/nyxt.png")
                     ("assets/nyxt_128x128.png" . #p"usr/share/icons/hicolor/128x128/apps/nyxt.png")
                     ("assets/nyxt_256x256.png" . #p"usr/share/icons/hicolor/256x256/apps/nyxt.png")
                     ("assets/nyxt_512x512.png" . #p"usr/share/icons/hicolor/512x512/apps/nyxt.png"))
  :build-pathname "nyxt"
  :entry-point "nyxt:entry-point")
