;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

(export-always '*nyxt-renderer*)
(defvar *nyxt-renderer* (or (getenv "NYXT_RENDERER")
                            "gi-gtk"))
