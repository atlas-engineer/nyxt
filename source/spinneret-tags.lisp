;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :spinneret)

(deftag :maybe-raw (body attrs &key &allow-other-keys)
  (once-only (body)
    attrs ; to suppress the unused variable error
    `(:raw (if (nyxt:html-string-p ,body)
               ,body
               (escape-string ,body)))))
