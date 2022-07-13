;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/xmpp-mode
  (:documentation "XMPP support mode with a configurable chat-like communication."))
(in-package :nyxt/xmpp-mode)

(define-mode xmpp-mode ()
  "A mode for XMPP chats management."
  ((rememberable-p nil)
   (keyscheme-map
    (define-keyscheme-map "xmpp-mode" ()
      keyscheme:default
      (list
       "C-return" 'send-message)
      keyscheme:emacs
      (list
       "C-c C-c" 'send-message)))
   (style (theme:themed-css (theme *browser*)
            (* :font-family "monospace,monospace")
            (body
             :background-color theme:background)
            (textarea
             :border-width "4px"
             :border-color theme:primary
             :border-style "solid"
             :border-radius "0"
             :color theme:on-primary
             :width "100%"
             :position "absolute"
             :bottom "1em"
             :left "0")
            (.chat
             :display "flex"
             :flex-direction "column-reverse"
             :min-width "100%")
            (.message
             :margin "0.5em"
             :padding "0.5em")
            (.presence
             :margin "0.5em"
             :padding "0.2em")
            (.incoming
             :background-color theme:primary
             :color theme:on-primary
             :align-self "flex-start")
            (.outbound
             :background-color theme:secondary
             :color theme:on-secondary
             :align-self "flex-end")))
   (host
    nil
    :documentation "The hostname of the XMPP server.")
   (jid-part
    nil
    :documentation "The JID part of the username corresponding to the `host'.")
   (username
    nil
    :documentation "The username part of the JID.")
   (password nil)
   (auth-type
    nil
    :documentation "Auth type.
One of :PLAIN, :SASL-PLAIN, :DIGEST-MD5, :SASL-DIGEST-MD5.")
   (connection
    nil
    :type (maybe xmpp:connection)
    :reader nil
    :writer t
    :documentation "The currently established XMPP connection.")
   (receive-thread
    nil
    :type (maybe bt:thread)
    :export nil
    :documentation "Thread to receive XMPP messages on.")
   (recipient
    :type string
    :documentation "The JID of the person the current chat happens with.")
   (messages
    '()
    :type list
    :documentation "The history of all the incoming and outbound messages associated with the current `connection'."))
  (:toggler-command-p nil))

(defmethod enable ((mode xmpp-mode) &key &allow-other-keys)
  (setf (receive-thread mode)
        (run-thread "XMPP receiver thread"
          (xmpp:receive-stanza-loop
           (connection mode)
           :stanza-callback (lambda (stanza connection &key dom-repr)
                              (declare (ignore dom-repr))
                              (flet ((get-events ()
                                       (cl-xmpp::dom-to-event connection (cl-xmpp::parse-result connection stanza))))
                                (let ((events (handler-case
                                                  (get-events)
                                                (error ()
                                                  (xmpp-reconnect mode)
                                                  (ignore-errors (get-events))))))
                                  (dolist (event (alex:ensure-list events))
                                    (push event (messages mode)))
                                  (reload-buffers (list (buffer mode)))
                                  events)))))))

(defmethod connection ((mode xmpp-mode))
  (or (slot-value mode 'connection)
      (progn
        (xmpp-connect mode)
        (connection mode))))

(define-command send-message ()
  "Send the inputted message to the person the chat happens with."
  (let ((mode (find-submode 'xmpp-mode))
        (message-body (peval (ps:@ (nyxt/ps:qs document "#new") value))))
    (flet ((send-message ()
             (xmpp:message (connection mode) (recipient mode) message-body)))
      (handler-case
          (send-message)
        (error ()
          (xmpp-reconnect mode)
          (ignore-errors (send-message)))))
    (push (make-instance 'xmpp:message
                         :to (recipient mode)
                         :from (xmpp:username (connection mode))
                         :body message-body)
          (messages mode))
    (reload-buffers (list (buffer mode)))))

(define-command send-presence ()
  "Send the presence signal to the person chat happens with."
  (let ((mode (find-submode 'xmpp-mode)))
    (xmpp:presence (connection mode) :to (recipient mode))
    (push (make-instance 'xmpp:presence
                         :to (recipient mode)
                         :from (xmpp:username (connection mode)))
          (messages mode))
    (reload-buffers (list (buffer mode)))))

(define-class jabber-servers-source (prompter:source)
  ((prompter:name "Jabber servers (from Jabberes.org)")
   (prompter:constructor
    (delete
     nil
     (mapcar (lambda (raw-server)
               (let ((server (rest (car raw-server))))
                 (when (equal (getf server :|offline|) "no")
                   (getf server :|jid|))))
             (rest (s-xml:parse-xml-string
                    (dex:get "https://www.jabberes.org/servers/servers.xml"))))))))

(define-command xmpp-connect (&optional (mode (find-submode 'xmpp-mode))
                              (hostname (prompt1
                                         :prompt "XMPP server hostname"
                                         :sources (list (make-instance 'prompter:raw-source)
                                                        (make-instance 'jabber-servers-source))))
                              (jid-part (if-confirm ("Does the server have matching hostname and JID part?")
                                                    hostname
                                                    (prompt1
                                                     :prompt "XMPP server JID pard"
                                                     :sources (list (make-instance 'prompter:raw-source)))))
                              ;; FIXME: This should better have a default value
                              ;; of prompt, but we need to connect to the server
                              ;; first.
                              username password auth-type)
  "Connect to the chosen XMPP server."
  (let* ((connection (xmpp:connect-tls :hostname hostname :jid-domain-part jid-part)))
    (setf (connection mode) connection)
    (let* ((username (or username
                         (prompt1
                          :prompt (format nil "Your username at ~a" hostname)
                          :sources (list (make-instance 'prompter:raw-source)))))
           (password (or password
                         (prompt1
                          :prompt "Password"
                          :invisible-input-p t
                          :sources (list (make-instance 'prompter:raw-source)))))
           (auth-type (or auth-type
                          (prompt1
                           :prompt "Authorization type"
                           :sources (list (make-instance
                                           'prompter:source
                                           :name "Auth types"
                                           :constructor (list :plain :sasl-plain :digest-md5 :sasl-digest-md5)))))))
      (prog1
          (xmpp:auth (connection mode) username password "" :mechanism auth-type)
        (setf (host mode) hostname
              (jid-part mode) jid-part
              (username mode) username
              (password mode) password
              (auth-type mode) auth-type)))))

(define-command xmpp-disconnect (&optional (mode (find-submode 'xmpp-mode)))
  "Disconnect the MODE from the current server.

Leaves any other MODE state (`host', `username' etc.) intact to allow
`xmpp-reconnect'."
  (xmpp:disconnect (connection mode))
  (setf (connection mode) nil))

(define-command xmpp-reconnect (&optional (mode (find-submode 'xmpp-mode)))
  "Reconnect the MODE to the current server."
  (xmpp-disconnect mode)
  (xmpp-connect
   mode (host mode) (jid-part mode)
   (username mode) (password mode) (auth-type mode)))

(defgeneric event->html (event mode)
  (:method (event (mode xmpp-mode))
    (echo-warning "event->html not implemented for ~a ~a"
                  (class-of event) event))
  (:method ((event xmpp:presence) (mode xmpp-mode))
    (spinneret:with-html-string
      (:span :class (if (string= (xmpp:from event) (xmpp:username (connection mode)))
                        "presence outbound"
                        "presence incoming")
             (:i (xmpp:from event) " present"))))
  (:method ((event xmpp:message) (mode xmpp-mode))
    (spinneret:with-html-string
      (:div :class (if (string= (xmpp:from event) (xmpp:username (connection mode)))
                       "message outbound"
                       "message incoming")
            (xmpp:body event))))
  (:documentation "Translate the EVENT into the HTML string properly rendering it."))

(define-internal-scheme "xmpp"
    (lambda (url buffer)
      (enable-modes '(xmpp-mode) buffer)
      (let* ((mode (find-submode 'xmpp-mode buffer)))
        (setf (recipient mode) (quri:uri-path (nyxt::ensure-url url)))
        (unless (connection mode)
          (xmpp-connect mode))
        (values
         (spinneret:with-html-string
           (:head
            (:style (style buffer))
            (:style (style mode)))
           (:body
            (:div
             :class "chat"
             (dolist (message (messages mode))
               (:raw (event->html message mode))))
            (:textarea
             :id "new"
             :placeholder (format nil "Put your message to ~a here" (recipient mode)))))
         "text/html;charset=utf8"))))
