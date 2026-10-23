;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/integration)

(let ((nyxt-root (truename (asdf:system-source-directory "nyxt")))
      (test-root (truename (asdf:system-source-directory "nyxt/integration"))))
  (unless (uiop:pathname-equal nyxt-root test-root)
    (error "Integration tests loaded Nyxt from ~a, not ~a." nyxt-root test-root)))

#+sb-package-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package :nyxt))

(nyxt:define-internal-page nyxt::integration-home ()
    (:title "Gazette")
  "A local page with something to read, find, scroll, and follow."
  (spinneret:with-html-string
    (:h1 "Gazette")
    (:p "The lighthouse keeper counted every lighthouse.")
    (:p "A second lighthouse stands on the point.")
    (:a :href (nyxt:nyxt-url 'nyxt::integration-article) "Read the article")
    (:input :id "note" :type "text")
    (:div :style "height: 8000px")
    (:h2 "Far end")))

(nyxt:define-internal-page nyxt::integration-article ()
    (:title "Article")
  "The page reached by following the gazette link."
  (spinneret:with-html-string
    (:h1 "Article")
    (:p "The article body is about the lighthouse.")
    (:a :href (nyxt:nyxt-url 'nyxt::integration-home) "Back to the gazette")))

#+sb-package-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :nyxt))

(defun home-url ()
  (nyxt:nyxt-url 'nyxt::integration-home))

(defun article-url ()
  (nyxt:nyxt-url 'nyxt::integration-article))

(defun wait-until (predicate &key (timeout 15) (description "condition"))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop until (funcall predicate)
          when (> (get-internal-real-time) deadline)
            do (error "Timed out waiting for ~a." description)
          do (sleep 0.05))))

(defun page-text (&optional (buffer (nyxt:current-buffer)))
  (or (ignore-errors
       (nyxt:ps-eval :buffer buffer
         (ps:chain document body inner-text)))
      ""))

(defun element-text (buffer selector)
  (or (ignore-errors
       (nyxt:ps-eval :buffer buffer
         (ps:let ((element (nyxt/ps:qs document (ps:lisp selector))))
           (when element (ps:@ element inner-text)))))
      ""))

(defun input-value (buffer &optional (selector "#input"))
  (or (ignore-errors
       (nyxt:ps-eval :buffer buffer
         (ps:let ((element (nyxt/ps:qs document (ps:lisp selector))))
           (when element (ps:@ element value)))))
      ""))

(defun scroll-y (&optional (buffer (nyxt:current-buffer)))
  (or (ignore-errors
       (nyxt:ps-eval :buffer buffer (ps:@ window page-y-offset)))
      0))

(defun search-mark-count (&optional (buffer (nyxt:current-buffer)))
  (or (ignore-errors
       (nyxt:ps-eval :buffer buffer
         (ps:@ (nyxt/ps:qsa document "[nyxt-search-mark]") length)))
      0))

(defun wait-for-text (text &key (buffer (nyxt:current-buffer)) (timeout 20))
  (wait-until (lambda () (search text (page-text buffer)))
              :timeout timeout
              :description text)
  buffer)

(defun assert-test-file (file)
  (let ((path (namestring (files:expand file))))
    (unless (uiop:string-prefix-p (namestring *data-directory*) path)
      (error "Refusing to use ~a, which is outside ~a." path *data-directory*))))

(defun start-browser ()
  (assert-test-file (make-instance 'nyxt:history-file))
  (assert-test-file (make-instance 'nyxt:log-file))
  (assert-test-file (make-instance 'nyxt/mode/bookmark:bookmarks-file))
  (let* ((failure nil)
         (thread (bt:make-thread
                  (lambda ()
                    (handler-case
                        (nyxt:start :no-config t
                                    :no-auto-config t
                                    :socket *socket-path*
                                    :electron-opts
                                    (format nil "--user-data-dir=~a"
                                            *electron-data-directory*))
                      (error (condition) (setf failure condition))))
                  :name "nyxt-integration-start")))
    (wait-until (lambda ()
                  (or failure
                      (and nyxt:*browser*
                           (ignore-errors (nyxt:current-window))
                           (nyxt:current-buffer))))
                :timeout 90
                :description "the browser window")
    (when failure
      (error failure))
    (bt:join-thread thread)
    (setf (nyxt:search-engine-suggestions-p nyxt:*browser*) nil)))

(defun ensure-browser ()
  (when (and nyxt:*browser* (null (ignore-errors (nyxt:window-list))))
    (setf nyxt:*browser* nil))
  (unless (and nyxt:*browser* (ignore-errors (nyxt:current-window)))
    (start-browser))
  (setf (nyxt:search-engine-suggestions-p nyxt:*browser*) nil)
  nyxt:*browser*)

(defun shutdown-browser ()
  (when nyxt:*browser*
    (ignore-errors (nyxt:quit))))

(defun dismiss-prompts ()
  (when (and nyxt:*browser* (ignore-errors (nyxt:current-window)))
    (dolist (prompt (nyxt::active-prompt-buffers (nyxt:current-window)))
      (ignore-errors (prompter:destroy prompt)))))

(defun close-other-buffers ()
  (unless (nyxt:buffer-list)
    (nyxt:make-buffer-focus))
  (let ((keep (nyxt:current-buffer)))
    (dolist (buffer (remove keep (nyxt:buffer-list)))
      (nyxt:ffi-buffer-delete buffer))
    (wait-until (lambda ()
                  (every (lambda (buffer) (eq buffer keep))
                         (nyxt:buffer-list)))
                :timeout 5
                :description "a single open buffer")
    keep))

(defun load-page (url-string text &key page-title)
  (nyxt:ffi-buffer-load (nyxt:current-buffer) (nyxt:url url-string))
  (wait-for-text text)
  (when page-title
    (wait-until (lambda ()
                  (search page-title (or (nyxt:title (nyxt:current-buffer)) "")))
                :timeout 10
                :description page-title))
  (nyxt:update-document-model)
  (nyxt:current-buffer))

(defun open-page (url-string text &key page-title)
  (nyxt:make-buffer-focus :url url-string)
  (wait-for-text text)
  (when page-title
    (wait-until (lambda ()
                  (search page-title (or (nyxt:title (nyxt:current-buffer)) "")))
                :timeout 10
                :description page-title))
  (nyxt:update-document-model)
  (nyxt:current-buffer))

(defmacro with-page ((url-string text &key page-title) &body body)
  `(progn
     (ensure-browser)
     (dismiss-prompts)
     (close-other-buffers)
     (load-page ,url-string ,text :page-title ,page-title)
     ,@body))

(defvar *prompt-error-box* (cons nil nil)
  "A cons whose cdr collects errors from the thread that accepts a prompt.
The command thread is blocked inside the prompt, so acceptance runs elsewhere.
Mutating the cdr keeps the error visible to every thread that shares the cons.")

(defun prompt-error ()
  (first (cdr *prompt-error-box*)))

(defun prompt-input-ready-p (prompt)
  (ignore-errors
   (nyxt:ps-eval :buffer prompt
     (not (null (nyxt/ps:qs document "#input"))))))

(defun return-prompt (prompt)
  (let ((box *prompt-error-box*))
    (bt:make-thread
     (lambda ()
       (handler-case (nyxt/mode/prompt-buffer:run-action-on-return prompt)
         (nyxt:prompt-buffer-canceled ())
         (error (condition) (push condition (cdr box)))))
     :name "nyxt-integration-return")))

(defun accept-prompt (prompt &key input (source 0) shows suggestion-p)
  "Type into PROMPT and accept it.
SOURCE is how many times to move to the next prompt source.
SHOWS lists strings that must be visible in the prompt buffer.
SUGGESTION-P, when supplied, is called with the selected suggestion."
  (wait-until (lambda () (prompt-input-ready-p prompt))
              :timeout 5
              :description "the prompt input")
  (when input
    (nyxt:set-prompt-buffer-input input prompt)
    (wait-until (lambda () (string= input (input-value prompt)))
                :timeout 5
                :description "the prompt input value"))
  (loop repeat source
        do (nyxt/mode/prompt-buffer:next-source prompt))
  (dolist (text shows)
    (wait-for-text text :buffer prompt :timeout 10))
  (when suggestion-p
    (let ((suggestion (nyxt:current-suggestion-value prompt)))
      (unless (funcall suggestion-p suggestion)
        (error "Prompt suggestion was ~s." suggestion))))
  (return-prompt prompt))

(defun wait-for-thread (thread timeout)
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop while (bt:thread-alive-p thread)
          when (prompt-error)
            do (error (prompt-error))
          when (> (get-internal-real-time) deadline)
            do (bt:destroy-thread thread)
               (error "Timed out waiting for ~a." (bt:thread-name thread))
          do (sleep 0.05)))
  (when (prompt-error)
    (error (prompt-error)))
  (bt:join-thread thread))

(defun lparallel-queue-p (channel)
  "True when CHANNEL is an lparallel queue.
In-tree Nyxt creates lparallel queues. The loaded prompter still uses Calispel."
  (let ((queue-type (find-symbol "QUEUE" "LPARALLEL.QUEUE")))
    (and queue-type (typep channel queue-type))))

(defun channel-push (value channel)
  (if (lparallel-queue-p channel)
      (funcall (find-symbol "PUSH-QUEUE" "LPARALLEL.QUEUE") value channel)
      (funcall (find-symbol "!" "CALISPEL") channel value)))

(defun channel-pop (channel timeout)
  "Return the next value from CHANNEL, waiting up to TIMEOUT seconds.
The second value is true when a value arrived."
  (if (lparallel-queue-p channel)
      (funcall (find-symbol "TRY-POP-QUEUE" "LPARALLEL.QUEUE")
               channel :timeout timeout)
      (funcall (find-symbol "?" "CALISPEL") channel timeout)))

(defun drive-prompts (command-thunk steps)
  "Run COMMAND-THUNK and handle each prompt it opens with the functions in STEPS."
  (let* ((ready (nyxt::make-channel))
         (handler (make-instance
                   'hooks:handler
                   :name 'integration-prompt-ready
                   :fn (lambda (prompt)
                         (channel-push prompt ready))))
         (*prompt-error-box* (cons nil nil))
         (box *prompt-error-box*))
    (hooks:add-hook (nyxt:prompt-buffer-ready-hook nyxt:*browser*) handler)
    (unwind-protect
         (let ((thread (bt:make-thread
                        (lambda ()
                          (handler-case (funcall command-thunk)
                            (nyxt:prompt-buffer-canceled ())
                            (error (condition)
                              (push condition (cdr box)))))
                        :name "nyxt-integration-command")))
           (dolist (step steps)
             (loop
               (let ((prompt (channel-pop ready 15)))
                 (unless prompt
                   (unless (bt:thread-alive-p thread)
                     (bt:join-thread thread))
                   (when (prompt-error)
                     (error (prompt-error)))
                   (error "The prompt buffer did not open."))
                 (unless (eq :ignore (funcall step prompt))
                   (return)))))
           (wait-for-thread thread 20))
      (hooks:remove-hook (nyxt:prompt-buffer-ready-hook nyxt:*browser*) handler)
      (dismiss-prompts))))

(defun clear-bookmarks ()
  (setf (files:content (nyxt/mode/bookmark:bookmarks-file (nyxt:current-buffer)))
        '()))

(defmethod asdf:perform :around
    ((op asdf:test-op) (system (eql (asdf:find-system "nyxt/integration"))))
  (unwind-protect (call-next-method)
    (shutdown-browser)))
