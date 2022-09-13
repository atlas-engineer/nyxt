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
  (setf (buffer wrapper)
        (buffer-load-internal-page-focus 'open-debugger :id (id wrapper))))

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

(define-command-global report-bug ()
  "Report the bug on Nyxt GitHub, filling all the guessable information in the process."
  (let* ((title (prompt1
                 :prompt "Title of the issue"
                 :sources (list (make-instance 'prompter:raw-source))))
         (buffer (make-buffer-focus
                  :url (quri:uri (format nil "https://github.com/atlas-engineer/nyxt/issues/new?&template=bug_report.md&title=~a"
                                         title)))))
    (hooks:once-on (buffer-loaded-hook buffer)
        (buffer)
      (when (and (equalp (quri:uri-host (url buffer)) "github.com")
                 (equalp (quri:uri-path (url buffer)) "/atlas-engineer/nyxt/issues/new"))
        (nyxt:ps-eval :buffer buffer
          (ps:chain (nyxt/ps:qs document "#issue_body") (focus))
          (setf (ps:@ document active-element value) ""))
        (ffi-buffer-paste
         buffer
         (format
          nil "**Describe the bug**

**Precise recipe to reproduce the issue**

For website-specific issues:
Can you reproduce this issue with Epiphany / GNOME Web (https://wiki.gnome.org/Apps/Web)?

**Information**
- OS name+version:
```sh

```
- Graphics card and driver:
``` sh

```
- Desktop environment / Window manager name+version:
- How you installed Nyxt (Guix pack, package manager, build from source):
- Information from `show-system-information`:
```
~a
```

**Output when started from a shell** "
          (nyxt::system-information)))))))
