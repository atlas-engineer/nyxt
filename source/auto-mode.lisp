;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/auto-mode
  (:use :common-lisp :nyxt)
  (:import-from #:serapeum #:export-always)
  (:documentation "Mode for automatic URL-based mode toggling."))
(in-package :nyxt/auto-mode)

(export-always '*prompt-on-mode-toggle*)
(defvar *prompt-on-mode-toggle* nil
  "Whether user will be asked about adding the mode to included/excluded modes
in `auto-mode-rules' on mode activation/deactivation.")

(export-always '*non-rememberable-modes*)
(defvar *non-rememberable-modes*
  ;; base-mode conflicts with its Nyxt symbol if it's not prefixed
  '(help-mode web-mode auto-mode nyxt::base-mode)
  "Modes that `auto-mode' won't even try to save.
Append names of modes you want to always control manually to this list.
Be careful with deleting the defaults -- it can be harmful for your browsing.")

(declaim (ftype (function ((or symbol root-mode)) (values symbol &optional)) maybe-mode-name))
(defun maybe-mode-name (mode)
  (if (symbolp mode) mode (mode-name mode)))

(declaim (ftype (function ((or symbol root-mode) (or symbol root-mode)) boolean)
                mode-equal))
(defun mode-equal (mode1 mode2)
  (string-equal (symbol-name (maybe-mode-name mode1))
                (symbol-name (maybe-mode-name mode2))))

(defun non-rememberable-mode-p (mode)
  (member mode *non-rememberable-modes* :test #'mode-equal))

(defun rememberable-of (modes)
  (remove-if #'non-rememberable-mode-p modes))

(defclass-export auto-mode-rule ()
  ((test :initarg :test
         :accessor test
         :type list
         :initform (error "Slot `test' should be set."))
   (included :initarg :included
             :accessor included
             :type list-of-symbols
             :initform '()
             :documentation "The list of mode symbols to enable on rule activation.")
   (excluded :initarg :excluded
             :accessor excluded
             :type list-of-symbols
             :initform '()
             :documentation "The list of mode symbols to disable on rule activation.")
   (exact-p :initarg :exact-p
            :accessor exact-p
            :type boolean
            :initform nil
            :documentation "If non-nil, enable the INCLUDED modes exclusively.
Enable INCLUDED modes plus the already present ones, and disable EXCLUDED modes, if nil.")))

(declaim (ftype (function (quri:uri) (or auto-mode-rule null))
                matching-auto-mode-rule))
(defun matching-auto-mode-rule (url)
  (flet ((priority (test1 test2)
           (let ((priority-list '(match-regex match-url match-host match-domain)))
             (< (or (position (first test1) priority-list) 4)
                (or (position (first test2) priority-list) 4)))))
    (first (sort (remove-if-not
                  #'(lambda (rule)
                      (funcall-safely
                       (apply (first (test rule)) (rest (test rule))) url))
                  (or (auto-mode-rules *browser*) (restore-auto-mode-rules)))
                 #'priority :key #'test))))

(declaim (ftype (function (quri:uri buffer)) enable-matching-modes))
(defun enable-matching-modes (url buffer)
  (let ((rule (matching-auto-mode-rule url)))
    (enable-modes (set-difference
                   (included rule)
                   (rememberable-of (mapcar #'mode-name (modes buffer)))
                   :test #'mode-equal)
                  buffer)
    (disable-modes (if (exact-p rule)
                       (set-difference
                        (rememberable-of (mapcar #'mode-name (modes buffer)))
                        (included rule) :test #'mode-equal)
                       (excluded rule))
                   buffer)))

(defun can-store-last-active-modes (auto-mode url)
  (or (null (last-active-modes-url auto-mode))
      (not (quri:uri= url (last-active-modes-url auto-mode)))))

(defun store-last-active-modes (auto-mode url)
  (when (can-store-last-active-modes auto-mode url)
      (setf (last-active-modes auto-mode) (modes (buffer auto-mode))
            (last-active-modes-url auto-mode) url)))

(defun restore-last-active-modes (auto-mode)
  (disable-modes
   (mapcar #'maybe-mode-name
           (set-difference (modes (buffer auto-mode))
                           (last-active-modes auto-mode)
                           :test #'mode-equal))
   (buffer auto-mode))
  (enable-modes
   (mapcar #'maybe-mode-name
           (set-difference (last-active-modes auto-mode)
                           (modes (buffer auto-mode))
                           :test #'mode-equal))
   (buffer auto-mode)))

(defun new-page-request-p (request-data)
  "Whether the REQUEST-DATA is a request for a new page load.
Resource/font/ads/anchor loads are safely ignored.

It relies on the fact that, due to the WebKit limitations, we store the loaded
URL in the buffer slot when we need to load a new page, while, for
non-new-page requests, buffer URL is not altered."
  (quri:uri= (url request-data) (url (buffer request-data))))

(defun history-empty-p (history)
  (eq (htree:root history) (htree:current history)))

(defun auto-mode-handler (request-data)
  (let* ((auto-mode (find-submode (buffer request-data) 'auto-mode))
         ;; We need to suppress prompting when auto-mode modifies modes.
         (*prompt-on-mode-toggle* nil)
         (web-mode (find-submode (buffer request-data) 'web-mode))
         (previous-url
           (unless (history-empty-p (history web-mode))
             (url (htree:data
                   (htree:parent (htree:current (history web-mode)))))))
         (rule (matching-auto-mode-rule (url request-data)))
         (previous-rule (when previous-url (matching-auto-mode-rule previous-url))))
    (when (and rule previous-url (not previous-rule))
      (store-last-active-modes auto-mode previous-url))
    (cond
      ((and (not rule) (new-page-request-p request-data))
       (restore-last-active-modes auto-mode))
      ((and rule (not (equal rule previous-rule)))
       (enable-matching-modes (url request-data) (buffer request-data)))))
  request-data)

(defun mode-covered-by-auto-mode-p (mode auto-mode enable-p)
  (or (non-rememberable-mode-p mode)
      (let ((matching-rule (matching-auto-mode-rule
                            (url (buffer auto-mode)))))
        (member mode (or (and matching-rule (union (included matching-rule)
                                                   (excluded matching-rule)))
                         ;; Mode is covered by auto-mode only if it is
                         ;; in last-active-modes and gets enabled.
                         ;; If it gets disabled, user should be prompted,
                         ;; because they may want to persist it.
                         (and enable-p (last-active-modes auto-mode)))
                :test #'mode-equal))))

(export-always 'url-infer-match)
(declaim (ftype (function (string) list) url-infer-match))
(defun url-infer-match (url)
  "Infer the best `test' for `auto-mode-rule', based on the form of URL.
The rules are:
- If it's a plain domain-only URL (i.e. \"http://domain.com\") -- then use `match-domain'.
- If it's host with subdomains (\"http://whatever.subdomain.domain.com\") -- use `match-host'.
- Use `match-url' otherwise."
  (let ((url (or (ignore-errors (quri:uri url)) url)))
    (if (and (quri:uri-p url)
             (empty-path-url-p url)
             (host-only-url-p url))
        (if (string= (quri:uri-domain url)
                     (cl-ppcre:regex-replace "www[0-9]?\\." (quri:uri-host url) ""))
            `(match-domain ,(quri:uri-domain url))
            `(match-host ,(quri:uri-host url)))
        `(match-url ,(object-display url)))))

(declaim (ftype (function (boolean t) (function (root-mode)))
                make-mode-toggle-prompting-handler))
(defun make-mode-toggle-prompting-handler (enable-p auto-mode)
  #'(lambda (mode)
      (when (and (not (mode-covered-by-auto-mode-p mode auto-mode enable-p))
                 *prompt-on-mode-toggle*)
        (if-confirm ("Permanently ~:[disable~;enable~] ~a for this URL?"
                       enable-p (mode-name mode))
          (with-result (url (read-from-minibuffer
                             (make-minibuffer
                              :input-prompt "URL:"
                              :input-buffer (object-display (url (buffer mode)))
                              :must-match-p nil)))
            (add-modes-to-auto-mode-rules (url-infer-match url)
                                          :append-p t
                                          :include (when enable-p (list mode))
                                          :exclude (unless enable-p (list mode))))
          (setf (last-active-modes auto-mode)
                (if enable-p
                    (union (list mode) (last-active-modes auto-mode)
                           :test #'mode-equal)
                    (remove mode (last-active-modes auto-mode)
                            :test #'mode-equal)))))))

(defun initialize-auto-mode (mode)
  (unless (last-active-modes mode)
    (setf (last-active-modes mode)
          (default-modes (buffer mode))))
  (hooks:add-hook (enable-mode-hook (buffer mode))
                  (nyxt::make-handler-mode
                   (make-mode-toggle-prompting-handler t mode)
                   :name 'enable-mode-auto-mode-handler))
  (hooks:add-hook (disable-mode-hook (buffer mode))
                  (nyxt::make-handler-mode
                   (make-mode-toggle-prompting-handler nil mode)
                   :name 'disable-mode-auto-mode-handler))
  (hooks:add-hook (pre-request-hook (buffer mode))
                  (make-handler-resource #'auto-mode-handler)))

(defun clean-up-auto-mode (mode)
  (hooks:remove-hook (pre-request-hook (buffer mode))
                     'auto-mode-handler)
  (hooks:remove-hook (enable-mode-hook (buffer mode))
                     'enable-mode-auto-mode-handler)
  (hooks:remove-hook (disable-mode-hook (buffer mode))
                     'disable-mode-auto-mode-handler))

(define-mode auto-mode ()
  "Remember the modes setup for given domain/host/URL and store it in an editable form.
These modes will then be activated on every visit to this domain/host/URL."
  ((last-active-modes-url :accessor last-active-modes-url
                          :type (or quri:uri null)
                          :initform nil
                          :documentation "The last URL that the active modes were saved for.
We need to store this to not overwrite the `last-active-modes' for a given URL,
if `auto-mode-handler' will fire more than once.")
   (last-active-modes :accessor last-active-modes
                      :type list
                      :initform '()
                      :documentation "The list of modes that were enabled
on the last URL not covered by `auto-mode'.")
   (destructor :initform #'clean-up-auto-mode)
   (constructor :initform #'initialize-auto-mode)))

(define-command save-non-default-modes-for-future-visits ()
  "Save the modes present in `default-modes' and not present in current modes as :excluded,
and modes that are present in mode list but not in `default-modes' as :included,
to one of `auto-mode-rules'. Apply the resulting rule for all the future visits to this URL,
inferring the matching condition with `url-infer-match'.

For the storage format see the comment in the head of your `auto-mode-rules-data-path' file."
  (with-result (url (read-from-minibuffer
                     (make-minibuffer
                      :input-prompt "URL:"
                      :input-buffer (object-string (url (current-buffer)))
                      :suggestion-function (nyxt::history-suggestion-filter
                                            :prefix-urls (list
                                                          (object-string
                                                           (url (current-buffer)))))
                      :history (minibuffer-set-url-history *browser*)
                      :must-match-p nil)))
    (when (typep url 'nyxt::history-entry)
      (setf url (url url)))
    (add-modes-to-auto-mode-rules
     (url-infer-match url)
     :include (set-difference (modes (current-buffer))
                              (default-modes (current-buffer))
                              :test #'mode-equal)
     :exclude (set-difference (default-modes (current-buffer))
                              (modes (current-buffer))
                              :test #'mode-equal))))

(define-command save-exact-modes-for-future-visits ()
  "Store the exact list of enabled modes to `auto-mode-rules' for all the future visits of this
domain/host/URL/group of websites inferring the suitable matching condition by user input.
Uses `url-infer-match', see its documentation for matching rules.

For the storage format see the comment in the head of your `auto-mode-rules-data-path' file."
  ;; TODO: Should it prompt for modes to save?
  ;; One may want to adjust the modes before persisting them as :exact-p rule.
  (with-result (url (read-from-minibuffer
                     (make-minibuffer
                      :input-prompt "URL:"
                      :input-buffer (object-string (url (current-buffer)))
                      :suggestion-function (nyxt::history-suggestion-filter
                                            :prefix-urls (list
                                                          (object-string
                                                           (url (current-buffer)))))
                      :history (minibuffer-set-url-history *browser*)
                      :must-match-p nil)))
    (when (typep url 'nyxt::history-entry)
      (setf url (url url)))
    (add-modes-to-auto-mode-rules (url-infer-match url)
                                  :include (modes (current-buffer))
                                  :exact-p t)))

(declaim (ftype (function (list &key (:append-p boolean) (:exclude list)
                                (:include list) (:exact-p boolean))
                          (values list &optional))
                add-modes-to-auto-mode-rules))
(defun add-modes-to-auto-mode-rules (test &key (append-p nil) exclude include (exact-p nil))
  (let* ((rule (or (find test (auto-mode-rules *browser*) :key #'test :test #'equal)
                   (make-instance 'auto-mode-rule :test test)))
         (include  (rememberable-of (mapcar #'maybe-mode-name include)))
         (exclude  (rememberable-of (mapcar #'maybe-mode-name exclude))))
    (setf (exact-p rule) exact-p
          (included rule) (union include
                                 (when append-p
                                   (set-difference (included rule) exclude)))
          (excluded rule) (union exclude
                                 (when append-p
                                   (set-difference (excluded rule) include)))
          (auto-mode-rules *browser*) (delete-duplicates
                                       (append (when (or (included rule) (excluded rule))
                                                 (list rule))
                                               (auto-mode-rules *browser*))
                                       :key #'test :test #'equal))
    (if (or (included rule) (excluded rule))
        (store-auto-mode-rules)
        (echo "Only default modes are enabled in this buffer, there's nothing to save."))
    (auto-mode-rules *browser*)))

(defmethod serialize-object ((rule auto-mode-rule) stream)
  (flet ((write-if-present (slot)
           (when (funcall slot rule)
             (format t " :~a ~a"
                     slot
                     (funcall slot rule)))))
    (let ((*standard-output* stream)
          (*print-case* :downcase))
      (write-string "(" stream)
      (if (and (eq (first (test rule)) 'match-url)
               (= 2 (length (test rule))))
          (format t "~s " (second (test rule)))
          (format t "~s " (test rule)))
      (write-if-present 'included)
      (write-if-present 'excluded)
      (write-if-present 'exact-p)
      (write-string ")" stream))))

(defmethod deserialize-auto-mode-rules (stream)
  (handler-case
      (let ((*standard-input* stream))
        (let ((rules (read stream)))
          (mapcar #'(lambda (rule)
                      (let ((rule (append '(:test) rule)))
                        (when (stringp (getf rule :test))
                          (setf (getf rule :test) `(match-url ,(getf rule :test))))
                        (apply #'make-instance 'auto-mode-rule rule)))
                  rules)))
    (error (c)
      (log:error "During auto-mode rules deserialization: ~a" c)
      nil)))

(defun store-auto-mode-rules ()
  (with-data-file (file (auto-mode-rules-data-path *browser*)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (write-string ";; List of auto-mode rules.
;; It is made to be easily readable and editable, but you still need to remember some things:
;;
;; Every rule starts on a new line and consists of one or more of the following elements:
;; - Condition for rule activation. It is either (match-domain ...),
;;   (match-host ...), (match-regex ...) or a string.
;; - :included (optional) -- List of the modes to enable on condition.
;; - :excluded (optional) -- List of the modes to disable on condition.
;; - :exact-p  (optional) -- Whether to enable only :included modes and disable
;;    everything else (if :exact-p is t), or just add :included and exclude :excluded
;;    modes from the current modes list (if :exact-p is nil or not present).
;;
;; Conditions work this way:
;; - match-domain matches the URL domains only.
;;   Example: (match-domain \"reddit.com\") will work for all of Reddit.
;; - match-host is more specific -- it activates only on certain subdomains of the website.
;;   Example: (match-host \"old.reddit.com\") will work on old Reddit only.
;; - match-regex works for any address that matches a given regex. You can add these manually,
;;   but remember: with great regex comes great responsibility!
;;   Example: (match-regex \"https://github\\.com/.*/.*\") will activate only in repos on GitHub.
;; - String format matches the exact URL and nothing else
;;   Example: \"https://lispcookbook.github.io/cl-cookbook/pattern_matching.html\"
;;            will work on the Pattern Matching article of CL Cookbook, and nowhere else.
;;
;; You can write additional URLs in the bracketed conditions, to reuse the rule for other URL
;; Example: (match-host \"reddit.com\" \"old.reddit.com\" \"www6.reddit.com\")
" file)
    (write-string "(" file)
    (dolist (rule (slot-value *browser* 'auto-mode-rules))
      (write-char #\newline file)
      (serialize-object rule file))
    (format file "~%)~%")
    (echo "Saved ~a auto-mode rules to ~s."
          (length (slot-value *browser* 'auto-mode-rules))
          (expand-path (auto-mode-rules-data-path *browser*)))))

(defun restore-auto-mode-rules ()
  (handler-case
      (let ((data (with-data-file (file (auto-mode-rules-data-path *browser*)
                                        :direction :input
                                        :if-does-not-exist nil)
                    (when file (deserialize-auto-mode-rules file)))))
        (when data
          (echo "Loading ~a auto-mode rules from ~s."
                (length data) (expand-path (auto-mode-rules-data-path *browser*)))
          (setf (slot-value *browser* 'auto-mode-rules) data)))
    (error (c)
      (echo-warning "Failed to load auto-mode-rules from ~s: ~a"
                    (expand-path (auto-mode-rules-data-path *browser*)) c))))
