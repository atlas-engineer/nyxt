;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/tts-mode
    (:use :common-lisp :nyxt)
  (:documentation "Mode for text-to-speak a documents content."))

(in-package :nyxt/tts-mode)

(defvar executable-process-info nil)

(define-mode tts-mode ()
  "A mode for text-to-speak a documents content.
By default, no executable is configured, so for the mode to work,
you need to configure an executable than can take a string of text.
Something like `espeak` should work out of the box.
Also by default, the content that will be send to the executable
is the text-content of p-tags on the page. This can be configured
by changing the `selector`-slot.

Example:

\(define-configuration nyxt/tts-mode:tts-mode
   ((nyxt/tts-mode:executable \"espeak\")
    (nyxt/tts-mode:selector \"h1, h2, h3, h4, p\")))
"
  ((executable nil)
   ;; TODO: If you specify multiple tags, like h1, h2, h3, p, they are selected in that
   ;; order, not in the order they appear on the page. This is different than how it's
   ;; done with e.g. JS document.querySelectorAll which returns them in the order they
   ;; appear, which is IMO the correct way ... can you configure clss to do this?
   (selector "p")
   (destructor
    (lambda (mode)
      (if-process-then-terminate mode)))
   (constructor
    (lambda (mode)
      (initialize mode))))
  :documentation
  "For the mode to work, you need to set the executable slot
to an executable thats available in the environment path.")

(defmethod initialize ((mode tts-mode)))

(defmethod on-signal-load-finished ((mode tts-mode) url)
  (declare (ignore url)))
  ;; TODO: document-mode not ready here???
  ;; (process-document mode))

(defmethod process-document ((mode tts-mode))
  "Fetch the text in buffer that matches `selector` and send it off
to get *spoken*."
  (if (executable mode)
      (let* ((tags
              (handler-case
                  (coerce 
                   (clss:select (selector mode) (document-model (buffer mode)))
                   'list)
                (error ()
                       (log:warn "tts-mode: no document-model available.")
                       nil)))
             (text (sanitize-text
                    mode
                    (with-output-to-string
                      (s)
                      (dolist (tag tags)
                        (format s "~a" (plump:text tag)))))))
        (when tags
          (speak mode text)))
    (echo-warning "tts-mode: no executable configured.")))

(defmethod speak ((mode tts-mode) text)
  "Start an asynchronous process of the `executable` with `text`
passed as the argument."
  (let ((program-string
         (format nil "~s ~s" (executable mode) text)))
    (progn
      (log:info "tts-mode: starting speak.")
      ;; make sure that a running process is stopped before starting a new
      (if-process-then-terminate mode)
      (setf executable-process-info
            (uiop:launch-program program-string
                                 :output *standard-output*
                                 :error-output *standard-output*))
      (if (not (zerop (uiop:wait-process executable-process-info)))
          (log:warn "tts-mode executable terminated.")
        (log:info "tts-mode: speak done."))
      (if-process-then-terminate mode))))

(defmethod if-process-then-terminate ((mode tts-mode))
  "If there is a running process, terminate it."
  (when (and executable-process-info
             (uiop:process-alive-p executable-process-info))
    (log:info "tts-mode: stopping speak.")
    (uiop:terminate-process executable-process-info :urgent t)
    (setf executable-process-info nil)))

(defmethod sanitize-text ((mode tts-mode) text)
  "Remove anything non- alpha, numeric, and space."
  (remove-if
   (complement
    (lambda (s)
      (or
       (string= #\Space s)
       (alpha-char-p s)
       (not (null (digit-char-p s))))))
   text))
           
(define-command start-speak ()
  "Start text-to-speak the content of the current buffer
matching the selector."
  (let ((mode (find-submode (current-buffer) 'tts-mode)))
    (process-document mode)))

(define-command stop-speak ()
  "Stop running text-to-speak process if there is one."
  (let ((mode (find-submode (current-buffer) 'tts-mode)))
    (if-process-then-terminate mode)))
