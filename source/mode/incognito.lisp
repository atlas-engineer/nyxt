;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/incognito
  (:documentation "Incognito mode: isolate session data for this buffer."))

(in-package :nyxt/mode/incognito)

(define-mode incognito-mode ()
  "Mode to browse without persisting data. Uses a separate web context."
  ((visible-in-status-p t)
   (glyph "🕶")
   (keyscheme-map (define-keyscheme-map "incognito-mode" ()
                    keyscheme:default
                    (list)))
   (rememberable-p nil))
  (:documentation "When enabled, the buffer uses a non-persistent renderer context
with no persistent cookies and no global history writes."))

(defmethod enable :after ((mode incognito-mode) &key &allow-other-keys)
  (let* ((buffer (buffer mode)))
    ;; Switch buffer to use the incognito web context.
    (setf (slot-value buffer 'nyxt::web-context-name) "incognito")
    ;; Also prevent history entries on this buffer by removing history-mode if present.
    (when (nyxt:find-submode 'nyxt/mode/history:history-mode buffer)
      (nyxt:disable-modes* 'nyxt/mode/history:history-mode buffer))))

(define-command-global open-incognito (&key (url (nyxt:default-new-buffer-url nyxt:*browser*)))
  "Open a new incognito buffer."
  (let ((buffer (nyxt:make-buffer :buffer-class 'nyxt:web-buffer
                                  :url url
                                  :modes '(nyxt/mode/incognito:incognito-mode))))
    (nyxt:set-current-buffer buffer)
    buffer))
