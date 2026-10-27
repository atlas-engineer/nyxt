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
  ()
  (:toggler-command-p nil))

(define-command clear-messages ()
  "Clear the *Messages* buffer and the underlying message data."
  (setf (slot-value *browser* 'messages-content) '())
  (echo "Messages cleared."))

(-> message-parts (string) (values string string &optional))
(defun message-parts (message)
  "Return LEVEL and TEXT for a simple-layout line.
LEVEL is a capitalized word such as \"Info\" when MESSAGE starts with
\"INFO - \". TEXT is the rest. A leading \"Warning: \" is dropped when
LEVEL is Warn, since the level already says it."
  (let* ((line (str:trim message))
         (separator (search " - " line))
         (head (and separator (plusp separator) (subseq line 0 separator))))
    (if (and head (every #'alphanumericp head))
        (let ((level (string-capitalize head))
              (text (subseq line (+ separator 3))))
          (when (and (string-equal level "Warn")
                     (str:starts-with-p "Warning: " text))
            (setf text (subseq text (length "Warning: "))))
          (values level (str:collapse-whitespaces (str:trim text))))
        (values "" (str:collapse-whitespaces line)))))

(-> message-level-class (string) string)
(defun message-level-class (level)
  "Return the class for a LEVEL other than Info."
  (format nil "level ~(~a~)" level))

(define-internal-page-command-global list-messages ()
    (buffer "*Messages*" 'nyxt/mode/message:message-mode)
  "Show the *Messages* buffer."
  ;; One list, flush with the title. Each message is a single 44px line:
  ;; 17px text, ellipsis, no wrap. Info has no label. Any other level sits
  ;; on that same line, at 11px, and a warning uses the warning color.
  (let ((rows (loop for message in (reverse (nyxt:messages-content *browser*))
                    for parts = (multiple-value-list (message-parts message))
                    unless (str:emptyp (second parts))
                      collect parts)))
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
        `("ul.messages .level.warn, ul.messages .level.error, ul.messages .level.fatal"
          :color ,theme:warning-color)
        `("p.empty"
          :margin "0"
          :font-size "17px"
          :line-height "24px"
          :color ,theme:primary-color))
      (render-menu 'nyxt/mode/message:message-mode buffer)
      (:h1 "Messages")
      (if rows
          (:ul :class "messages"
            (dolist (parts rows)
              (let ((level (first parts))
                    (text (second parts)))
                (:li
                 (unless (or (str:emptyp level)
                             (string-equal level "Info"))
                   (:span :class (message-level-class level) level))
                 (:span :class "text" :title text text)))))
          (:p :class "empty" "No messages.")))))
