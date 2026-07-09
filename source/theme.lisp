;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *runtime-theme-lock* (bt:make-lock "runtime theme"))

(defparameter *runtime-themes*
  `(("acme" . ,theme:+acme-theme+)
    ("kanagawa-dragon" . ,theme:+kanagawa-dragon-theme+)))

(defun runtime-theme (designator)
  (typecase designator
    (theme:theme designator)
    (symbol (runtime-theme (symbol-name designator)))
    (string
     (or (cdr (assoc designator *runtime-themes* :test #'string-equal))
         (error "Unknown runtime theme ~s. Expected one of: ~{~a~^, ~}."
                designator (mapcar #'car *runtime-themes*))))
    (t (error "Invalid runtime theme designator ~s." designator))))

(defun live-theme-buffers ()
  (remove-duplicates
   (append (buffer-list) (internal-buffer-list :all t))))

(defun apply-runtime-theme (buffer &optional (browser *browser*))
  (when browser
    (set-document-theme (theme:css-variables (theme browser)) buffer)))

(defmethod on-signal-load-finished :after ((buffer buffer) url title)
  (declare (ignore url title))
  (apply-runtime-theme buffer))

(export-always 'set-runtime-theme)
(defun set-runtime-theme (designator &optional (browser *browser*))
  "Apply DESIGNATOR to current and future browser interface documents."
  (unless browser
    (error "Cannot set a runtime theme before the browser is initialized."))
  (let ((new-theme (runtime-theme designator)))
    (bt:with-lock-held (*runtime-theme-lock*)
      (setf (theme browser) new-theme)
      (dolist (buffer (live-theme-buffers))
        (apply-runtime-theme buffer browser))
      (ffi-apply-theme browser new-theme))
    new-theme))
