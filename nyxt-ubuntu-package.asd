;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Can we move this file to build-scripts?  Looks like linux-packaging
;; fails to find the produced binary then.
(defsystem "nyxt-ubuntu-package"
  :defsystem-depends-on ("linux-packaging")
  :class "linux-packaging:deb"
  :build-operation "linux-packaging:build-op"
  :depends-on (nyxt/gi-gtk)
  :package-name "nyxt"
  :version #.(asdf:system-version (asdf:find-system :nyxt))
  :author #.(asdf:system-author (asdf:find-system :nyxt))
  :homepage #.(asdf:system-homepage (asdf:find-system :nyxt))
  :description #.(asdf:system-description (asdf:find-system :nyxt))
  :license #.(asdf:system-license (asdf:find-system :nyxt))
  :additional-dependencies ("libfixposix-dev" ; Because we need libfixposix.so, not just libfixposix.so.3.
                            "libwebkit2gtk-4.0-dev" ; Same.
                            "glib-networking"
                            "gsettings-desktop-schemas"
                            "xclip"
                            "enchant-2")
  :additional-files (("assets/nyxt.desktop" . "usr/share/applications/")
                     ("assets/nyxt_16x16.png" . #p"usr/share/icons/hicolor/16x16/apps/nyxt.png")
                     ("assets/nyxt_32x32.png" . #p"usr/share/icons/hicolor/32x32/apps/nyxt.png")
                     ("assets/nyxt_128x128.png" . #p"usr/share/icons/hicolor/128x128/apps/nyxt.png")
                     ("assets/nyxt_256x256.png" . #p"usr/share/icons/hicolor/256x256/apps/nyxt.png")
                     ("assets/nyxt_512x512.png" . #p"usr/share/icons/hicolor/512x512/apps/nyxt.png"))
  :build-pathname "nyxt"
  :entry-point "nyxt:entry-point")
