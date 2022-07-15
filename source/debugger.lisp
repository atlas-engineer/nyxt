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

(export 'make-nyxt-debugger)
(defun make-nyxt-debugger
    (&key (query-read (lambda (wrapper)
                        (let ((*interactive-p* t))
                          (str:concat (prompt1 :prompt (prompt-text wrapper)
                                               :sources (list (make-instance 'prompter:raw-source)))
                                      +newline+))))
       (query-write (lambda (wrapper string)
                      (setf (prompt-text wrapper) string)))
       (ui-display (lambda (wrapper)
                     (setf (gethash (id wrapper) *debug-conditions*) wrapper)
                     (set-current-buffer
                      (setf (buffer wrapper)
                            (buffer-load (nyxt-url 'open-debugger :id (id wrapper))
                                         :buffer (ensure-internal-page-buffer 'open-debugger))))))
       (ui-cleanup (lambda (wrapper)
                     (remhash (id wrapper) *debug-conditions*)
                     (buffer-delete (buffer wrapper)))))
  (ndebug:make-debugger-hook
   :wrapper-class 'debug-wrapper
   :query-read query-read
   :query-write query-write
   :ui-display ui-display
   :ui-cleanup ui-cleanup))

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
      (let ((frame-call (dissect:call frame))
            (frame-args (dissect:args frame)))
        (when frame-call
          (:details
           :attrs (list :open frame-args)
           (:summary (:code (princ-to-string frame-call)))
           (when frame-args
             (:p "Called with:")
             (:ul (loop for arg in frame-args
                        when (or (typep arg 'dissect:unknown-arguments)
                                 (typep arg 'dissect:unavailable-argument))
                          collect (:li (:code "Unknown argument"))
                        else collect (:li (:raw (value->html arg t))))))))))))

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
    (swank-backend:install-debugger-globally (if value (make-nyxt-debugger) nil))
    (echo "Nyxt-native debugging ~:[dis~;en~]abled." value)))
