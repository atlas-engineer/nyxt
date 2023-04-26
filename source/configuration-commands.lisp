;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun error-in-new-window (title condition-string backtrace)
  (sera:lret* ((window (window-make *browser*))
               (error-buffer (make-instance 'document-buffer)))
    (with-current-buffer error-buffer
      (html-set
       (values
        (spinneret:with-html-string
          (:head
           (:title title)
           (:nstyle (style (current-buffer))))
          (:body
           (:h1 title)
           (:h2 "Condition")
           (:pre condition-string)
           (alex:when-let ((suggestions (alex:mappend #'nyxt/migration:find-suggestions
                                                      (sera:tokens condition-string))))
             (:h2 "Suggestions")
             (:ul (dolist (suggestion suggestions)
                    (:li (:raw (nyxt/migration:tip suggestion))))))
           (:h2 "Backtrace")
           (:pre backtrace)))
        "text/html;charset=utf8")
       error-buffer))
    (window-set-buffer window error-buffer)))

(-> load-lisp
    ((or null types:pathname-designator) &key (:package (or null package)))
    *)
(defun load-lisp (file &key package)
  "Load the Lisp FILE (can also be a stream).
Return T on success.
On error, return the condition as a first value and the backtrace as second value."
  (unless (files:nil-pathname-p file)
    (let ((*package* (or (find-package package) *package*)))
      (flet ((unsafe-load ()
               (cond
                 ((streamp file)
                  (load file))
                 ((uiop:file-exists-p file)
                  (log:info "Loading Lisp file ~s." file)
                  (load file))
                 (t
                  (log:debug "Lisp file ~s does not exist." file)))
               nil))
        (if *run-from-repl-p*
            (tagbody
             loop
               (restart-case (unsafe-load)
                 (load-lisp-retry ()
                   :report "Retry loading Lisp file."
                   (go loop))))
            (catch 'lisp-file-error
              (handler-bind ((error (lambda (c)
                                      (let ((backtrace (with-output-to-string (stream)
                                                         (uiop:print-backtrace :stream stream :condition c))))
                                        (throw 'lisp-file-error
                                          (if *browser*
                                              (error-in-new-window "*Config file errors*" (princ-to-string c) backtrace)
                                              (values c backtrace)))))))
                (unsafe-load))))))))

(define-command load-file ()
  "Load the prompted Lisp file."
  (prompt :prompt "Load file"
          :input (uiop:native-namestring
                  (let ((config-path (files:expand *config-file*)))
                    (if (uiop:file-exists-p config-path)
                        (uiop:pathname-directory-pathname config-path)
                        (uiop:getcwd))))
          :extra-modes 'nyxt/mode/file-manager:file-manager-mode
          :sources
          (make-instance 'nyxt/mode/file-manager:file-source
                         :extensions '("lisp")
                         :actions-on-return (lambda-command load-file* (files)
                                              (dolist (file files)
                                                (load-lisp file))))))

(define-command clean-configuration ()
  "Clean all the user configuration created with `define-configuration' or `customize-instance'."
  (dolist (class (sera:filter #'user-class-p (sym:package-classes* (nyxt-packages))))
    (setf (hooks:handlers-alist (slot-value class 'customize-hook)) nil))
  (dolist (method (mopu:generic-function-methods #'customize-instance))
    (unless (or (equal (list (find-class t)) ; Don't remove default method.
                       (mopu:method-specializers method))
                ;; We only preserve :after methods for ourselves.
                (equal (list :after) (method-qualifiers method))))))

(define-command load-config-file (&key (config-file (files:expand *config-file*)))
  "Load or reload the CONFIG-FILE."
  (if (files:nil-pathname-p config-file)
      (echo "No config file.")
      (progn
        (clean-configuration)
        (load-lisp config-file :package (find-package :nyxt-user))
        (echo "~a loaded." config-file))))
