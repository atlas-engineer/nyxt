;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;;; TODO: Turn into a library for UI-independent debugging.

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
    :documentation "The prompt text debugger requires.")
   (stack
    nil
    :documentation  "The state of call stack at the time of the condition firing.")
   (backtrace
    nil
    :documentation "The printed backtrace at the time of condition firing.
Prefer `stack', if present."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "The wrapper for condition.

Made so that `debugger-hook' can wait for the condition to be resolved based on
the channel, wrapped alongside the condition and its restarts."))

(defun make-debugger-stream (handler)
  (make-two-way-stream
   ;; TODO: Understand how Swank makes those streams.
   (swank-backend:make-input-stream
    (lambda ()
      (str:concat
       (prompt1
        :prompt (prompt-text handler)
        :sources 'prompter:raw-source)
       +newline+)))
   (swank-backend:make-output-stream
    (lambda (string) (setf (prompt-text handler) string)))))

(defun debugger-hook (condition hook)
  (when *debug-on-error*
    (let* ((*debugger-hook* hook)
           (id (new-id))
           (restarts (compute-restarts))
           (channel (make-channel 1))
           (handler (make-instance 'condition-handler
                                   :condition-itself condition
                                   :restarts restarts
                                   :channel channel
                                   :stack (dissect:stack)
                                   :backtrace (with-output-to-string (s)
                                                (uiop:print-backtrace :stream s :condition condition))))
           (*interactive-p* t)
           (*query-io* (make-debugger-stream handler))
           (debug-buffer (open-debugger :id id)))
      (setf (gethash id *debug-conditions*) handler)
      (unwind-protect
           ;; FIXME: Waits indefinitely. Should it?
           (invoke-restart-interactively (calispel:? channel))
        (remhash id *debug-conditions*)
        (buffer-delete debug-buffer)))))

(defun restarts->html (handler)
  (spinneret:with-html-string
    (loop for restart in (restarts handler)
          for i from 0
          collect (let ((restart restart)
                        (handler handler))
                    (:button :class "button"
                             :onclick (ps:ps (nyxt/ps:lisp-eval
                                              (:title "condition")
                                              (calispel:! (channel handler) restart)))
                             (format nil "[~d] ~a" i (restart-name restart)))))))

(defun backtrace->html (handler)
  (spinneret:with-html-string
    (cond
      ((stack handler)
       (loop for frame in (stack handler)
             collect (when (or (dissect:call frame)
                               (dissect:args frame))
                       (:details
                        (:summary (:code (princ-to-string (dissect:call frame))))
                        (when (dissect:args frame)
                          (:p "Called with:")
                          (:ul (loop for arg in (dissect:args frame)
                                     when (or (typep arg 'dissect:unknown-arguments)
                                              (typep arg 'dissect:unavailable-argument))
                                       collect (:li (:code "Unknown argument"))
                                     else collect (:li (:raw (value->html arg t))))))))))
      ((backtrace handler)
       (:pre (backtrace handler))))))

(defun debug->html (handler)
  "Produce HTML code for the CONDITION with RESTARTS."
  (let ((condition (condition-itself handler)))
    (spinneret:with-html-string
      (:h* (symbol-name (type-of condition)))
      (:pre (format nil "~a" condition))
      (:section
       (:raw (restarts->html handler))
       (:h* "Backtrace")
       (:raw (backtrace->html handler))))))

;; FIXME: Not for interactive use?
(define-internal-page-command open-debugger (&key id)
    ;; TODO: Introduce debug-mode with keys invoking restarts and toggling backtrace.
    (buffer (format nil "*Debug-~d*" id))
  "Open the debugger with the condition indexed by ID."
  (debug->html (gethash id *debug-conditions*)))

(define-command-global toggle-debug-on-error (&optional (value nil value-provided-p))
  "Toggle Nyxt-native debugging.

See `*debug-on-error*'."
  (let ((value (if value-provided-p value (not *debug-on-error*))))
    (setf *debug-on-error* value)
    ;; FIXME: This messes up SLIME/SLY debugging in REPL, as they set this too.
    (swank-backend:install-debugger-globally (if value 'debugger-hook nil))
    (echo "Nyxt-native debugging ~:[dis~;en~]abled." value)))
