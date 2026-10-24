;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; The system clipboard is cl-clipboard. This file keeps Nyxt's clipboard ring
;; on top of it.

(export-always 'clipboard-text)
(defun clipboard-text ()
  "Return the system clipboard text, or NIL when it is unavailable or holds no text."
  (cl-clipboard:text))

(defun (setf clipboard-text) (text)
  "Set the system clipboard to TEXT."
  (setf (cl-clipboard:text) text))

(-> ring-insert-clipboard (containers:ring-buffer-reverse) (maybe string))
(export-always 'ring-insert-clipboard)
(defun ring-insert-clipboard (ring)
  "Check if clipboard-content is most recent entry in RING.
If not, insert clipboard-content into RING.
Return most recent entry in RING."
  (let ((clipboard-content (clipboard-text)))
    (when clipboard-content
      (unless (string= clipboard-content (unless (containers:empty-p ring)
                                           (containers:first-item ring)))
        (containers:insert-item ring clipboard-content)))
    (unless (containers:empty-p ring)
      (string (containers:first-item ring)))))

(export-always 'copy-to-clipboard)
(defun copy-to-clipboard (input)
  "Save INPUT text to the system clipboard and the clipboard ring."
  (let ((input (setf (clipboard-text) input)))
    (containers:insert-item (clipboard-ring *browser*) input)
    input))
