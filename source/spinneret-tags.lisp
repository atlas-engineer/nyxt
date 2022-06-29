;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :spinneret)

(deftag :mayberaw (body attrs &key &allow-other-keys)
  attrs
  `(:raw (if (nyxt:html-string-p (progn ,@body))
             (progn ,@body)
             (escape-string (progn ,@body)))))

(deftag :nstyle (body attrs &key &allow-other-keys)
  `(:style ,@attrs (:raw ,@body)))

(deftag :nscript (body attrs &key &allow-other-keys)
  `(:script ,@attrs (:raw ,@body)))

(spinneret:deftag :nxref (symbol attr &key &allow-other-keys)
  `(:a :href (nyxt:javascript-url
              (ps:ps (nyxt/ps:lisp-eval
                      (:title "describe-any")
                      ;; Not defined yet:
                      (funcall (nyxt:resolve-symbol :describe-any :function)
                               (princ-to-string ,@symbol)))))
       (:code ,@attr ,@symbol)))
