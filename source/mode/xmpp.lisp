;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/xmpp-mode
  (:documentation "Visual mode."))
(in-package :nyxt/xmpp-mode)

(define-mode xmpp-mode ()
  "A mode for XMPP chats management."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "repl"
      scheme:cua
      (list
       "C-return" 'send-message)
      scheme:emacs
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
             :color theme:background
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
             :background-color theme:text
             :color theme:background
             :align-self "flex-start")
            (.outbound
             :background-color theme:quaternary
             :color theme:text
             :align-self "flex-end")))
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
                              (let ((events (cl-xmpp::dom-to-event connection (cl-xmpp::parse-result connection stanza))))
                                (dolist (event (alex:ensure-list events))
                                  (push event (messages mode)))
                                (reload-buffers (list (buffer mode)))
                                events))))))

(defmethod connection ((mode xmpp-mode))
  (or (slot-value mode 'connection)
      (progn
        (xmpp-connect mode)
        (connection mode))))

(define-command send-message ()
  "Send the inputted message to the person the chat happens with."
  (let ((mode (find-submode 'xmpp-mode))
        (message-body (peval (ps:@ (nyxt/ps:qs document "#new") value))))
    (xmpp:message (connection mode) (recipient mode) message-body)
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

(define-command xmpp-connect (&optional (mode (find-submode 'xmpp-mode)))
  "Connect to the chosen XMPP server."
  (let* ((hostname (prompt1
                     :prompt "XMPP server hostname"
                     :sources (list (make-instance 'prompter:raw-source))))
         (jid (if-confirm ("Does the server have matching hostname and JID part?")
                          hostname
                          (prompt1
                            :prompt "XMPP server JID pard"
                            :sources (list (make-instance 'prompter:raw-source)))))
         (connection (xmpp:connect-tls :hostname hostname :jid-domain-part jid)))
    (setf (connection mode) connection)
    (let* ((username (prompt1
                       :prompt (format nil "Your username at ~a" hostname)
                       :sources (list (make-instance 'prompter:raw-source))))
           (password (prompt1
                       :prompt "Password"
                       :invisible-input-p t
                       :sources (list (make-instance 'prompter:raw-source))))
           (auth-type (prompt1
                        :prompt "Authorization type"
                        :sources (list (make-instance
                                        'prompter:source
                                        :name "Auth types"
                                        :constructor (list :plain :sasl-plain :digest-md5 :sasl-digest-md5))))))
      (xmpp:auth (connection mode) username password "" :mechanism auth-type))))

(defmethod xmpp:handle ((connection xmpp:connection) (message xmpp:message))
  (let ((mode (find-submode 'xmpp-mode)))
    (push message (messages mode))
    (reload-buffers (list (buffer mode))))
  message)

(defmethod xmpp:handle ((connection xmpp:connection) object)
  (echo "Got ~a of type ~a." object (type-of object))
  object)

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
               (typecase message
                 (xmpp:presence
                  (:span :class (if (string= (xmpp:from message) (xmpp:username (connection mode)))
                                    "presence outbound"
                                    "presence incoming")
                         (:i "present")))
                 (xmpp:message
                  (:div :class (if (string= (xmpp:from message) (xmpp:username (connection mode)))
                                   "message outbound"
                                   "message incoming")
                        (xmpp:body message))))))
            (:textarea
             :id "new"
             :placeholder (format nil "Put your message to ~a here" (recipient mode)))))
         "text/html;charset=utf8"))))
