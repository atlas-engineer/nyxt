;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *debug-conditions* (make-hash-table)
  "A hash-table from condition ID (as per `new-id') to the `condition-handler' lists.")

(define-class condition-handler ()
  ((condition-itself
    (error "condition-handler should always wrap a condition.")
    :type condition
    :documentation "The condition itself.")
   (restarts
    '()
    :type list
    :documentation "A list of restarts for the given condition.
Stored in the format given by `compute-restarts'.")
   (channel
    nil
    :type (or null calispel:channel)
    :documentation "The channel to send the chosen restart through.")
   (prompt-text
    "[restart prompt]"
    :type string
    :documentation "The prompt text debugger requires."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "The wrapper for condition.

Made so that `debugger-hook' can wait for the condition to be resolved based on
the channel, wrapped alongside the condition and its restarts."))

(defun debugger-hook (condition hook)
  (when *debug-on-error*
    (let* ((*debugger-hook* hook)
           (id (new-id))
           (restarts (compute-restarts condition))
           (channel (make-channel 1))
           (handler (make-instance 'condition-handler
                                   :condition-itself condition
                                   :restarts restarts
                                   :channel channel))
           (*query-io*
             (make-two-way-stream
              ;; TODO: Understand how Swank makes those streams.
              (swank-backend:make-input-stream
               (lambda ()
                 (str:concat
                  (prompt1
                    :prompt (prompt-text handler)
                    :sources (list (make-instance 'prompter:raw-source)))
                  +newline+)))
              (swank-backend:make-output-stream
               (lambda (string) (setf (prompt-text handler) string)))))
           (debug-buffer (open-debugger :id id)))
      (setf (gethash id *debug-conditions*) handler)
      ;; FIXME: Waits indefinitely. Should it?
      (invoke-restart-interactively (calispel:? channel))
      (remhash id *debug-conditions*)
      (buffer-delete debug-buffer))))

(defun debug->html (condition id &optional restarts)
  "Produce HTML code for the CONDITION with RESTARTS."
  (spinneret:with-html-string
    (:h* (symbol-name (type-of condition)))
    (:pre (format nil "~a" condition))
    (:section
     (loop for restart in restarts
           for i from 0
           collect (:button :class "button"
                            :onclick (ps:ps (nyxt/ps:lisp-eval (:title "condition" )
                                             (let ((condition (gethash id *debug-conditions*)))
                                               (calispel:! (channel condition)
                                                           (nth i (restarts condition))))))
                            (format nil "[~d] ~a" i (restart-name restart))))
     (:h* "Backtrace")
     ;; TODO: SLIME and SLY provide introspectable backtraces. How?
     (:pre (with-output-to-string (s) (uiop:print-backtrace :stream s :condition condition))))))

;; FIXME: Not for interactive use?
(define-internal-page-command open-debugger (&key id)
    ;; TODO: Introduce debug-mode with keys invoking restarts and toggling backtrace.
    (buffer (format nil "*Debug-~d*" id))
  "Open the debugger with the condition indexed by ID."
  (with-slots (condition-itself restarts channel)
      (gethash id *debug-conditions*)
    (declare (ignore channel))
    (debug->html condition-itself id restarts)))

(define-command-global toggle-debug-on-error (&optional (value nil value-provided-p))
  "Toggle Nyxt-native debugging.

See `*debug-on-error*'."
  (let ((value (if value-provided-p value (not *debug-on-error*))))
    (setf *debug-on-error* value)
    ;; FIXME: This messes up SLIME/SLY debugging in REPL, as they set this too.
    (swank-backend:install-debugger-globally (if value 'debugger-hook nil))
    (echo "Nyxt-native debugging ~:[dis~;en~]abled." value)))
