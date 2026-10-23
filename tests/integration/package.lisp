;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/tests/integration
  (:use :lisp-unit2)
  (:documentation "Integration tests that browse through Nyxt's own UI.
The suite starts the Electron renderer and drives the prompt buffer, the page,
and the status bar. It is separate from the unit tests in :nyxt/tests."))

#+sb-package-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package :nyxt/tests/integration))

(in-package :nyxt/tests/integration)

(defvar *data-directory*
  (truename
   (ensure-directories-exist
    (uiop:merge-pathnames*
     (format nil "nyxt-integration/~a/" (get-universal-time))
     (uiop:temporary-directory))))
  "Directory that receives every file this suite asks Nyxt to read or write.
Truenamed so it still matches paths after Nyxt resolves symlinks.")

(defvar *socket-path*
  (namestring (uiop:merge-pathnames* "nyxt.socket" *data-directory*)))

(defvar *electron-data-directory*
  (namestring (ensure-directories-exist
               (uiop:merge-pathnames* #p"electron/" *data-directory*))))

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt:history-file))
  (uiop:merge-pathnames* #p"history.lisp" *data-directory*))

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt:log-file))
  (uiop:merge-pathnames* #p"nyxt.log" *data-directory*))

(defmethod files:resolve ((profile nyxt:nyxt-profile)
                          (file nyxt/mode/bookmark:bookmarks-file))
  (uiop:merge-pathnames* #p"bookmarks.lisp" *data-directory*))

(dolist (name '("history.lisp" "bookmarks.lisp" "nyxt.log"))
  (with-open-file (stream (uiop:merge-pathnames* name *data-directory*)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (when (member name '("history.lisp" "bookmarks.lisp") :test #'string=)
      (write-string "()" stream))))
