;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :gtk)

(defcfun ("gtk_clipboard_wait_for_text" gtk-clipboard-wait-for-text)
  (g-string :free-from-foreign t)
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-clipboard-wait-for-text)
