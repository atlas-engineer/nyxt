;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Add support for adding files.

;; TODO: Handle IPNS as well.

;; Test data:
;; - https://ipfs.io/ipfs/Qme7ss3ARVgxv6rXqVPiikMJ8u2NLgmgszg13pYrDKEoiu
;; - ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html
;; - ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/I/m/Van_Gogh_Museum_Amsterdam.jpg
;; - ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/I/m/Van_Gogh_-_Bildnis_der_Mutter_des_K%C3%BCnstlers.jpeg

;; REFERENCE:
;;  https://docs.ipfs.tech/reference/kubo/rpc/#getting-started
;;
;; https://www.reddit.com/r/ipfs/comments/63ev6h/list_of_ipfs_websites/
;; https://github.com/brave/brave-browser/issues/10220
;; https://github.com/ipfs/in-web-browsers
;; https://github.com/ipfs-shipyard/is-ipfs

(nyxt:define-package :nyxt/ipfs-mode
    (:documentation "Mode for IPFS page browsing."))
(in-package :nyxt/ipfs-mode)

(define-mode ipfs-mode ()
  "Handle ipfs:// URLs."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (daemon
    nil
    :allocation :class
    :documentation "Daemon UIOP process.
There can be only one per Nyxt instance.")
   (program
    "ipfs"
    :type types:pathname-designator
    :documentation "Name or path of the IPFS executable.
See also the `arguments' slot.")
   (arguments
    '()
    :type (cons string *)
    :documentation "Arguments passed to the daemon when starting.
See also the `program' slot.")
   (daemon-timeout
    30
    :type alex:non-negative-float
    :documentation "Time in seconds to wait for the daemon to start.
After this time has elapsed, signal an error.")
   (gateway
    (quri:uri "https://dweb.link")
    :type quri:uri
    :documentation "Fallback remote node when local daemon is not running.
Redirection example:

    ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html

to

    https://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq.ipfs.dweb.link/wiki/Vincent_van_Gogh.html"))
  (:toggler-command-p nil))

(defmethod daemon-running-p ((mode ipfs-mode)) ; Unused?  Useful for debugging though.
  (= 0
     (nth 2
          (uiop:run-program (list (program mode) "diag" "cmds")
                            :ignore-error-status t))))

(defmethod start-daemon ((mode ipfs-mode))
  "Wait until IPFS daemon is started.
Return immediately if already started."
  (sera:synchronized ((daemon mode))
    (when (and (not (daemon mode))
               (sera:resolve-executable (program mode)))
      (let ((p-i (uiop:launch-program (append (list (program mode) "daemon")
                                              (arguments mode))
                                      :output :stream
                                      :error-output :stream)))
        (handler-case
            (progn
              (bt:with-timeout ((daemon-timeout mode))
                (loop
                  (unless (uiop:process-alive-p p-i)
                    (error "Already started?"))
                  (when (and (uiop:process-alive-p p-i)
                             (string= "Daemon is ready" (read-line (uiop:process-info-output p-i))))
                    (return 'ready))))
              (setf (daemon mode) p-i))
          (bt:timeout ()
            (uiop:terminate-process p-i :urgent t)
            (error "IPFS daemon timed out before it could start:~&~a"
                   (uiop:slurp-stream-string (uiop:process-info-output p-i))))
          (t ()
            (uiop:terminate-process p-i :urgent t)
            (error "IPFS daemon failed to start:~&~a"
                   (uiop:slurp-stream-string (uiop:process-info-error-output p-i)))))))))

(defmethod quit-daemon ((mode ipfs-mode))
  (when (daemon mode)
    (uiop:terminate-process (daemon mode))
    (setf (daemon mode) nil)))

(defun ipfs-request-p (request-data)
  (and (toplevel-p request-data)
       (or (str:s-member '("ipfs" "ipns") (quri:uri-scheme (url request-data)))
           ;; TODO: It's in the response header.
           ;; Test: https://en.wikipedia-on-ipfs.org
           ;; See https://webkitgtk.org/reference/webkit2gtk/stable/method.URIResponse.get_http_headers.html.
           (str:starts-with-p "x-ipfs-path" (mime-type request-data)))))

(defmethod enable ((mode ipfs-mode) &key)
  (hooks:add-hook
   (pre-request-hook (buffer mode))
   (make-instance
    'hooks:handler
    :fn (lambda (request-data)
          (unless (ipfs-request-p request-data)
            (disable-modes 'ipfs-mode (buffer mode)))
          request-data)
    :name 'ipfs-mode-disable)))

(defmethod disable ((mode ipfs-mode) &key)
  ;; TODO: Terminate daemon here?  Maybe not, since it's an expensive operation.
  ;; But when then?  In (before-exit-hook *browser*)?
  (hooks:remove-hook
   (pre-request-hook (buffer mode))
   'ipfs-mode-disable))

(defmethod url->gateway-url ((mode ipfs-mode) url)
  "Turn IPFS URL into an HTTP URL on MODE's `gateway' URL."
  (let ((cid (quri:uri-domain url)))
    (quri:copy-uri url
                   :scheme "https"
                   :port (quri.port:scheme-default-port "https")
                   :host (uiop:strcat cid ".ipfs." (quri:uri-host (gateway mode))))))

(defmethod fetch ((mode ipfs-mode) url)
  ;; TODO: Fallback to public gateway.
  (start-daemon mode)
  (if (daemon mode)
      (let ((mime (or (mimes:mime (pathname (quri:uri-path url)))
                      "text/html;charset=utf8")))
        ;; WARNING: Need byte-vector here because strings CL encoding may confuse the browser.
        (values (flex:string-to-octets (ipfs:cat (nyxt::schemeless-url url))) ; TODO: Export?
                mime))
      (let* ((new-url (url->gateway-url mode url))
             (redirect-html
               (spinneret:with-html-string
                 (:head
                  (:title "Redirect")
                  (:style (style (buffer mode))))
                 (:body
                  (:h1 "Redirecting...")
                  (:p
                   (:a url)
                   " to "
                   (:a new-url))))))
        (buffer-load new-url :buffer (buffer mode))
        (values redirect-html "text/html;charset=utf8"))))

(define-internal-scheme "ipfs"
    (lambda (url-string buffer)
      ;; FIXME: This better become a default auto-mode rule.
      (enable-modes '(ipfs-mode) buffer)
      (fetch (find-submode 'ipfs-mode buffer) (quri:uri url-string))))
