;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/blocker
  (:documentation "Package for `blocker-mode', which blocks requests for hosts.
The mode stores the names in `hosts' and publishes them through
`ffi-buffer-blocked-hosts'. The renderer matches the request host against that
list."))
(in-package :nyxt/mode/blocker)

(define-mode blocker-mode ()
  "Block requests to the hosts in `hosts'.
The request host is matched exactly, ignoring case, so \"example.com\" does not
block \"ads.example.com\".

Example:

\(define-configuration nyxt/mode/blocker:blocker-mode
  ((nyxt/mode/blocker:hosts '(\"platform.twitter.com\"
                              \"syndication.twitter.com\"))))"
  ((hosts
    '()
    :type (list-of string)
    :documentation "Host names to block.
Each entry is one host, such as \"ads.example\". A port, a trailing dot, and
surrounding brackets are ignored.")))

(defmethod (setf hosts) :after (hosts (mode blocker-mode))
  (when (and (enabled-p mode) (buffer mode))
    (setf (ffi-buffer-blocked-hosts (buffer mode)) hosts)))

(defmethod enable ((mode blocker-mode) &key)
  (setf (ffi-buffer-blocked-hosts (buffer mode)) (hosts mode)))

(defmethod disable ((mode blocker-mode) &key)
  (setf (ffi-buffer-blocked-hosts (buffer mode)) nil))
