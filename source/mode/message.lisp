;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/message
  (:documentation "Package for `message-mode', a mode for displaying messages
and logs.
All the APIs of the `message-mode' are exported, no implementation details."))
(in-package :nyxt/mode/message)

(define-mode message-mode ()
  "Mode for log and message listing.
Mainly used on `list-messages' page."
  ((messages
    (make-instance 'user-interface:dynamic-element
                   :tag :p
                   :html-class "empty"
                   :text "No messages."
                   :template #'message-row)
    :export nil
    :documentation "The list shown on `list-messages'.
A `user-interface:dynamic-element'."))
  (:toggler-command-p nil))

(define-command clear-messages ()
  "Clear the *Messages* buffer and the underlying message data."
  (setf (messages-content *browser*) '())
  (echo "Messages cleared."))

(-> message-parts (string) (values string string &optional))
(defun message-parts (message)
  "Return LEVEL and TEXT for a simple-layout line.
LEVEL is a capitalized word such as \"Info\" when MESSAGE contains
\"INFO - \". TEXT is the rest of the line."
  (let* ((line (str:trim message))
         (parts (str:split " - " line :limit 2 :omit-nulls nil)))
    (if (second parts)
        (values (str:capitalize (first parts))
                (str:collapse-whitespaces (str:trim (second parts))))
        (values "" (str:collapse-whitespaces line)))))

(-> message-row (string) (values t &optional))
(defun message-row (message)
  "Return a row for MESSAGE, or nil when it has no text.
The row is a level and the text. A missing level is blank."
  (multiple-value-bind (level text) (message-parts message)
    (unless (str:emptyp text)
      (make-instance 'user-interface:element
                     :tag :li
                     :children (list (make-instance 'user-interface:element
                                                    :tag :span
                                                    :html-class (if (str:emptyp level)
                                                                    "level"
                                                                    (format nil "level ~(~a~)"
                                                                            level))
                                                    :text level)
                                     (make-instance 'user-interface:element
                                                    :tag :span
                                                    :html-class "text"
                                                    :title text
                                                    :text text))))))

(-> present-messages (message-mode (maybe browser)) t)
(defun present-messages (mode browser)
  "Show BROWSER's messages in MODE.
The list element keeps a row it has already built."
  (let ((element (messages mode))
        (content (if browser (messages-content browser) '())))
    (user-interface:batch (element)
      (setf (user-interface:items element)
            (coerce (nreverse (loop for message in content
                                    unless (str:emptyp (nth-value 1 (message-parts message)))
                                      collect message))
                    'vector))
      (if (plusp (length (user-interface:items element)))
          (setf (user-interface:tag element) :ul
                (user-interface:html-class element) "messages"
                (user-interface:text element) nil)
          (setf (user-interface:tag element) :p
                (user-interface:html-class element) "empty"
                (user-interface:text element) "No messages.")))))

(defmethod customize-instance :after ((mode message-mode) &key)
  "Keep MODE's list in step with `messages-content'."
  (define-setf-handler browser messages-content mode
    (lambda (browser)
      (present-messages mode browser)))
  (present-messages mode *browser*))

(define-internal-page-command-global list-messages ()
    (buffer "*Messages*" 'nyxt/mode/message:message-mode)
  "Show the *Messages* buffer."
  (let ((messages (messages (find-submode 'message-mode buffer))))
    (user-interface:connect messages buffer)
    (spinneret:with-html-string
      (:nstyle
        '("ul.messages"
          :list-style "none"
          :margin "0 -17px"
          :padding "0")
        '("ul.messages > li"
          :position "relative"
          :display "flex"
          :align-items "center"
          :gap "17px"
          :height "44px"
          :margin "0"
          :padding "0 17px"
          :white-space "nowrap")
        `("ul.messages > li:not(:last-child)::after"
          :content ""
          :position "absolute"
          :right "17px"
          :bottom "0"
          :left "17px"
          :height "1px"
          :background-color ,theme:background-color-)
        '("ul.messages .text"
          :min-width "0"
          :overflow "hidden"
          :text-overflow "ellipsis"
          :white-space "nowrap"
          :font-size "17px"
          :font-weight "400"
          :line-height "24px")
        `("ul.messages .level"
          :flex "none"
          :font-size "11px"
          :font-weight "600"
          :line-height "17px"
          :color ,theme:primary-color)
        '("ul.messages .level:empty"
          :display "none")
        `("ul.messages .level.warn, ul.messages .level.error, ul.messages .level.fatal"
          :color ,theme:warning-color)
        `("p.empty"
          :margin "0"
          :font-size "17px"
          :line-height "24px"
          :color ,theme:primary-color))
      (render-menu 'nyxt/mode/message:message-mode buffer)
      (:h1 "Messages")
      (user-interface:to-html messages))))
