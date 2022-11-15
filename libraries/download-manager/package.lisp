;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package :download-manager
  (:use :cl)
  (:export #:init
           #:*notifications*
           #:default-download-directory
           #:download
           #:resolved-url
           #:requested-url
           #:header
           #:file
           #:filename
           #:bytes-fetched
           #:bytes-total
           #:progress
           #:finished-p
           #:last-update-speed
           #:cache
           #:resolve))
