;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/reduce-tracking
  (:documentation "Package for `reduce-tracking-mode' to mitigate fingerprinting."))
(in-package :nyxt/mode/reduce-tracking)

(define-mode reduce-tracking-mode ()
  "Set specific settings in the web view in order to mitigate fingerprinting
(how third-party trackers attempt to identify you).

Fingerprinting can be tested at https://panopticlick.eff.org/."
  ((preferred-languages
    '("en_US")
    :type (list-of string)
    :documentation "The list of languages that will be sent as part of the
Accept-Language HTTP header.")
   (preferred-user-agent
    ;; Check https://techblog.willshouse.com/2012/01/03/most-common-user-agents
    ;; and https://www.whatismybrowser.com/guides/the-latest-user-agent/safari
    ;; occasionally and refresh when necessary.
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_1) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.1 Safari/605.1.15"
    :type string
    :documentation "The user agent to set when enabling `reduce-tracking-mode'.
It's Safari on macOS by default, because this way we break fewer websites while
still being less noticeable in the crowd.")
   ;; Taken from https://github.com/brave/brave-core/blob/master/browser/net/brave_site_hacks_network_delegate_helper.cc#L31
   ;; Also see https://gitlab.com/ClearURLs/rules/-/blob/master/data.min.json
   (query-tracking-parameters
    '("fbclid" "gclid" "msclkid" "mc_eid"
      "dclid"
      "oly_anon_id" "oly_enc_id"
      "_openstat"
      "vero_conv" "vero_id"
      "wickedid"
      "yclid"
      "__s"
      "rb_clickid"
      "s_cid"
      "ml_subscriber" "ml_subscriber_hash"
      "twclid"
      "gbraid" "wbraid"
      "_hsenc" "__hssc" "__hstc" "__hsfp" "hsCtaTracking"
      "oft_id" "oft_k" "oft_lk" "oft_d" "oft_c" "oft_ck" "oft_ids"
      "oft_sk"
      "igshid")
    :type (list-of string)
    :documentation "The list of query parameters to clean from the URLs.")
   (old-user-agent
    nil
    :type (or null string)
    :export nil
    :documentation "The User Agent the browser had before enabling this mode.")
   (old-timezone
    (uiop:getenv "TZ")
    :export nil
    :documentation "The timezone the system had before enabling this mode.")))

(defun strip-tracking-parameters (request-data)
  (let ((mode (find-submode 'reduce-tracking-mode)))
    (when (and mode (not (uiop:emptyp (quri:uri-query (url request-data)))))
      (setf (quri:uri-query-params (url request-data))
            (remove-if (rcurry #'member (query-tracking-parameters mode)
                                          :test #'string-equal)
                       (quri:url-decode-params (quri:uri-query (url request-data)) :lenient t)
                       :key #'first)))
    request-data))

(defmethod enable ((mode reduce-tracking-mode) &key)
  (setf (old-timezone mode) (uiop:getenv "TZ")
        (uiop:getenv "TZ") "UTC")
  (setf (old-user-agent mode) (ffi-buffer-user-agent (buffer mode))
        (ffi-buffer-user-agent (buffer mode)) (preferred-user-agent mode))
  (setf (ffi-preferred-languages (buffer mode)) (preferred-languages mode))
  (setf (ffi-tracking-prevention (buffer mode)) t)
  (hooks:add-hook
   (request-resource-hook (buffer mode))
   #'strip-tracking-parameters))

(defmethod disable ((mode reduce-tracking-mode) &key)
  (when (old-timezone mode)
    (setf (uiop:getenv "TZ") (old-timezone mode)))
  (setf (ffi-buffer-user-agent (buffer mode)) (old-user-agent mode))
  (setf (ffi-preferred-languages (buffer mode))
        (list (first
               (str:split
                "."
                (or (uiop:getenv "LANG") "")))))
  (setf (ffi-tracking-prevention (buffer mode)) nil)
  (hooks:remove-hook (request-resource-hook (buffer mode))
                     #'strip-tracking-parameters))
