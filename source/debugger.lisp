;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *debug-conditions* (make-hash-table)
  "A hash-table from condition ID (as per `new-id') to the `condition-handler' lists.")

(define-class debug-wrapper (ndebug:condition-wrapper)
  ((prompt-text
    "[restart prompt]"
    :type string
    :documentation "The prompt text debugger requires.")
   (id
    (new-id)
    :type integer
    :documentation "The identifier of the wrapper to find it among other wrappers by.")
   (buffer
    nil
    :type (maybe buffer)
    :documentation "The buffer debugger is open in for this condition."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "The wrapper for condition.

See `ndebug:condition-wrapper' for documentation."))

(defmethod ndebug:ui-display ((wrapper debug-wrapper))
  (setf (gethash (id wrapper) *debug-conditions*) wrapper)
  (set-current-buffer
   (setf (buffer wrapper)
         (buffer-load (nyxt-url 'open-debugger :id (id wrapper))
                      :buffer (ensure-internal-page-buffer 'open-debugger)))))

(defmethod ndebug:ui-cleanup ((wrapper debug-wrapper))
  (remhash (id wrapper) *debug-conditions*)
  (buffer-delete (buffer wrapper)))

(defmethod ndebug:query-read ((wrapper debug-wrapper))
  (let ((*interactive-p* t))
    (prompt1 :prompt (prompt-text wrapper)
             :sources (list (make-instance 'prompter:raw-source)))))

(defmethod ndebug:query-write ((wrapper debug-wrapper) (string string))
  (setf (prompt-text wrapper) string))

(defun restarts->html (wrapper)
  (spinneret:with-html-string
    (dolist (restart (ndebug:restarts wrapper))
      (:button :class "button"
               :onclick (ps:ps (nyxt/ps:lisp-eval
                                (:title "condition")
                                (ndebug:invoke wrapper restart)))
               (format nil "[~a] ~a" (dissect:name restart) (dissect:report restart))))))

(defun backtrace->html (wrapper)
  (spinneret:with-html-string
    (dolist (frame (ndebug:stack wrapper))
      (let ((call (dissect:call frame))
            (args (dissect:args frame)))
        (cond
          ((and call args)
           (:details
            (:summary (:code (princ-to-string call)))
            (when args
              (:p "Called with:")
              (:ul (loop for arg in args
                         when (or (typep arg 'dissect:unknown-arguments)
                                  (typep arg 'dissect:unavailable-argument))
                           collect (:li (:code "Unknown argument"))
                         else collect (:li (:raw (value->html arg t))))))))
          (call
           (:code :style "display: block;" (princ-to-string call))))))))

(defun debug->html (wrapper)
  "Produce HTML code for the condition WRAPPER."
  (let ((condition (ndebug:condition-itself wrapper)))
    (spinneret:with-html-string
      (:h* (symbol-name (type-of condition)))
      (:pre (format nil "~a" condition))
      (:section
       (:raw (restarts->html wrapper))
       (:h* "Backtrace")
       (:raw (backtrace->html wrapper))))))

(define-internal-page open-debugger (&key id)
    ;; TODO: Introduce debug-mode with keys invoking restarts and toggling backtrace.
    (:title "*Debugger*")
  "Open the debugger with the condition indexed by ID."
  (debug->html (gethash id *debug-conditions*)))

(define-command-global toggle-debug-on-error (&optional (value nil value-provided-p))
  "Toggle Nyxt-native debugging.

See `*debug-on-error*'."
  (let ((value (if value-provided-p value (not *debug-on-error*))))
    (setf *debug-on-error* value)
    ;; FIXME: This messes up SLIME/SLY debugging in REPL, as they set this too.
    (swank-backend:install-debugger-globally
     (if value
         (ndebug:make-debugger-hook :wrapper-class 'debug-wrapper)
         nil))
    (echo "Nyxt-native debugging ~:[dis~;en~]abled." value)))
