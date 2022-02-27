;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/tts-mode
  (:use :common-lisp :nyxt)
  (:documentation "Mode for interacting with Text-to-Speech (TTS) software."))

(in-package :nyxt/tts-mode)
(use-nyxt-package-nicknames)

(define-mode tts-mode ()
  "A mode for interacting with Text-to-Speech (TTS) software.

By default, no executable is configured, so for the mode to work, you need to
configure an executable that can take a string of text. Something like `espeak`
or `espeak-ng` should work out of the box. The content that will be sent to the
executable is the text content of paragraph and heading tags on the page. This
can be configured by changing the `selector` slot.

Example:

\(define-configuration nyxt/tts-mode:tts-mode
   ((nyxt/tts-mode:executable \"espeak\")
    (nyxt/tts-mode:selector \"p, h1, h2, h3, h4, h5, h6\")))
"
  ((executable nil
               :type (or string null)
               :documentation "The executable command to run.")
   (selector "p, h1, h2, h3, h4, h5, h6"
       :type string
       :documentation "CSS selector that describes which elements' text to speak.")
   (executable-process-info nil
                            :type (or uiop/launch-program::process-info null)
                            :documentation "Holds the process-info object of the running process")
   (destructor
    (lambda (mode)
      (if-process-then-terminate mode)))))

(defmethod process-document ((mode tts-mode))
  "Fetch the text in buffer that matches `selector` and send it off to the TTS."
  (if (executable mode)
      (let* ((tags
               (sort (handler-case
                         (coerce
                          (clss:select (selector mode) (document-model (buffer mode)))
                          'list)
                       (error ()
                         (log:warn "tts-mode: no document-model available.")
                         nil))
                     #'< :key (alex:compose #'parse-integer #'get-nyxt-id)))
             ;; TODO properly handle punctuation like Emacspeak does
             (text (str:remove-punctuation
                    (with-output-to-string (s)
                      (dolist (tag tags)
                        (format s "~a" (plump:text tag)))))))
        (when tags
          (speak mode text)))
      (echo-warning "tts-mode: no executable configured.")))

(defmethod speak ((mode tts-mode) text)
  "Start an asynchronous process of the `executable` with TEXT passed as the
argument."
  (let ((program-string
          (format nil "~s ~s" (executable mode) text)))
    (progn
      (log:info "tts-mode: starting TTS.")
      ;; make sure that a running process is stopped before starting a new one
      (if-process-then-terminate mode)
      (setf (executable-process-info mode)
            (uiop:launch-program program-string
                                 :output *standard-output*
                                 :error-output *standard-output*))
      (if (not (zerop (uiop:wait-process (executable-process-info mode))))
          (log:info "tts-mode: TTS done."))
      (if-process-then-terminate mode))))

(defmethod if-process-then-terminate ((mode tts-mode))
  "If there is a running process, terminate it."
  (when (and (executable-process-info mode)
             (uiop:process-alive-p (executable-process-info mode)))
    (log:info "tts-mode: terminating TTS.")
    (uiop:terminate-process (executable-process-info mode) :urgent t)
    (setf (executable-process-info mode) nil)))

(define-command start-tts ()
  "Start TTS on the content of the current buffer matching the selector."
  (process-document (current-mode 'tts-mode)))

(define-command stop-tts ()
  "Stop running TTS process if there is one."
  (if-process-then-terminate (current-mode 'tts-mode)))
