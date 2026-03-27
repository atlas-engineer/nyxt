;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(-> ring-insert-clipboard (containers:ring-buffer-reverse) (maybe string))
(export-always 'ring-insert-clipboard)
(defun ring-insert-clipboard (ring)
  "Check if clipboard-content is most recent entry in RING.
If not, insert clipboard-content into RING.
Return most recent entry in RING."
  (let ((clipboard-content (handler-case (sophisticated-clipboard:clipboard-text)
                             (error ()
                               nil))))
    (when clipboard-content
      (unless (string= clipboard-content (unless (containers:empty-p ring)
                                           (containers:first-item ring)))
        (containers:insert-item ring clipboard-content)))
    (unless (containers:empty-p ring)
      (string (containers:first-item ring)))))

(export-always 'copy-to-clipboard)
(defun copy-to-clipboard (input)
  "Save INPUT to clipboard, supporting both text and images."
  (etypecase input
    (string
     (setf (sophisticated-clipboard:clipboard-text) input)
     (containers:insert-item (clipboard-ring *browser*) input))
    ((vector (unsigned-byte 8))
     ;; For binary data like images
     (setf (sophisticated-clipboard:clipboard-image) input)
     input)))
