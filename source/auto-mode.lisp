;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/auto-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:documentation "Mode for automatic URL-based mode toggling."))
(in-package :nyxt/auto-mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)
  (trivial-package-local-nicknames:add-package-local-nickname :hooks :serapeum/contrib/hooks))

(declaim (ftype (function ((or symbol root-mode list)) (values symbol &optional)) maybe-mode-name))
(defun maybe-mode-name (mode)
  (let ((mode (if (listp mode)
                  (first mode)
                  mode)))
    (if (symbolp mode) mode (mode-name mode))))

(declaim (ftype (function ((or symbol root-mode list) (or symbol root-mode list)) boolean)
                mode-equal))
(defun mode-equal (mode1 mode2)
  (string-equal (symbol-name (maybe-mode-name mode1))
                (symbol-name (maybe-mode-name mode2))))

(declaim (ftype (function ((or symbol root-mode list) (or root-mode null)) list)
                rememberable-of))
(defun rememberable-of (modes auto-mode)
  "Filter MODES based on rememberability by AUTO-MODE."
  (if auto-mode
      (set-difference modes (non-rememberable-modes auto-mode)
                      :test #'mode-equal)
      modes))

(define-class auto-mode-rule ()
  ((test (error "Slot `test' should be set.")
         :type list)
   (included '()
             :type list
             :documentation "The list of modes to enable on rule activation.
Here 'modes' can be either mode symbols or a list when the first element is the
mode symbols and the rest are the arguments to pass to the mode initiatiation
function.")
   (excluded '()
             :type list-of-symbols
             :documentation "The list of mode symbols to disable on rule activation.")
   (exact-p nil
            :documentation "If non-nil, enable the INCLUDED modes exclusively.
Enable INCLUDED modes plus the already present ones, and disable EXCLUDED modes, if nil."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

(declaim (ftype (function (quri:uri buffer) (or auto-mode-rule null))
                matching-auto-mode-rule))
(defun matching-auto-mode-rule (url buffer)
  (flet ((priority (test1 test2)
           (let ((priority-list '(match-regex match-url match-host match-domain)))
             (< (or (position (first test1) priority-list) 4)
                (or (position (first test2) priority-list) 4)))))
    (first (sort (remove-if-not
                  #'(lambda (rule)
                      (funcall-safely
                       (apply (first (test rule)) (rest (test rule))) url))
                  (or (get-data (auto-mode-rules-path buffer))
                      (restore (data-profile buffer) (auto-mode-rules-path buffer))))
                 #'priority :key #'test))))

(declaim (ftype (function (quri:uri buffer)) enable-matching-modes))
(defun enable-matching-modes (url buffer)
  (let ((rule (matching-auto-mode-rule url buffer))
        (auto-mode (find-mode buffer 'auto-mode)))
    (dolist (modes+args (mapcar #'alex:ensure-list
                                (set-difference
                                 (included rule)
                                 (rememberable-of (modes buffer) auto-mode)
                                 :test #'mode-equal)))
      (enable-modes (list (first modes+args)) buffer (rest modes+args)))
    (disable-modes (if (exact-p rule)
                       (set-difference
                        (rememberable-of (modes buffer) auto-mode)
                        (included rule) :test #'mode-equal)
                       (excluded rule))
                   buffer)))

(defun can-save-last-active-modes (auto-mode url)
  (or (null (last-active-modes-url auto-mode))
      (not (quri:uri= url (last-active-modes-url auto-mode)))))

(defun save-last-active-modes (auto-mode url)
  (when (can-save-last-active-modes auto-mode url)
    ;; `last-active-modes' must be a list separate from the mode list, otherwise
    ;; when modes get modified, last-active-modes would also be altered.
    (setf (last-active-modes auto-mode) (copy-list (modes (buffer auto-mode)))
          (last-active-modes-url auto-mode) url)))

(defun reapply-last-active-modes (auto-mode)
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
         (web-mode (find-submode (buffer request-data) 'web-mode))
         (previous-url
           (unless (history-empty-p (history web-mode))
             (url (htree:data
                   (htree:parent (htree:current (history web-mode)))))))
         (rule (matching-auto-mode-rule (url request-data) (buffer request-data)))
         (previous-rule (when previous-url (matching-auto-mode-rule previous-url (buffer request-data)))))
    (when (and rule previous-url (not previous-rule))
      (save-last-active-modes auto-mode previous-url))
    (cond
      ((and (not rule) (new-page-request-p request-data))
       (reapply-last-active-modes auto-mode))
      ((and rule (not (equalp rule previous-rule)))
       (enable-matching-modes (url request-data) (buffer request-data)))))
  request-data)

(defun mode-covered-by-auto-mode-p (mode auto-mode enable-p)
  (or (member mode (non-rememberable-modes auto-mode) :test #'mode-equal)
      (let ((matching-rule (matching-auto-mode-rule
                            (url (buffer auto-mode))
                            (buffer auto-mode))))
        (member mode (or (and matching-rule (union (included matching-rule)
                                                   (excluded matching-rule)))
                         ;; Mode is covered by auto-mode only if it is
                         ;; in last-active-modes and gets enabled.
                         ;; If it gets disabled, user should be prompted,
                         ;; because they may want to persist it.
                         (and enable-p (last-active-modes auto-mode)))
                :test #'mode-equal))))

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
      (when (not (mode-covered-by-auto-mode-p mode auto-mode enable-p))
        (if-confirm ("Permanently ~:[disable~;enable~] ~a for this URL?"
                       enable-p (mode-name mode))
                    (let ((url (prompt-minibuffer
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
  (when (prompt-on-mode-toggle mode)
    (hooks:add-hook (enable-mode-hook (buffer mode))
                    (nyxt::make-handler-mode
                     (make-mode-toggle-prompting-handler t mode)
                     :name 'enable-mode-auto-mode-handler))
    (hooks:add-hook (disable-mode-hook (buffer mode))
                    (nyxt::make-handler-mode
                     (make-mode-toggle-prompting-handler nil mode)
                     :name 'disable-mode-auto-mode-handler)))
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
  ((prompt-on-mode-toggle nil
                          :type boolean
                          :documentation "Whether user will be asked about adding the mode
to included/excluded modes in the auto-mode rules on mode activation/deactivation.")
   ;; base-mode conflicts with its Nyxt symbol if it's not prefixed
   (non-rememberable-modes '(help-mode web-mode auto-mode nyxt::base-mode)
                           :type list
                           :documentation "Modes that `auto-mode' won't even try to save.
Append names of modes you want to always control manually to this list.
Be careful with deleting the defaults -- it can be harmful for your browsing.")
   (last-active-modes-url nil
                          :type (or quri:uri null)
                          :documentation "The last URL that the active modes were saved for.
We need to store this to not overwrite the `last-active-modes' for a given URL,
if `auto-mode-handler' will fire more than once.")
   (last-active-modes '()
                      :documentation "The list of modes that were enabled
on the last URL not covered by `auto-mode'.")
   (destructor #'clean-up-auto-mode)
   (constructor #'initialize-auto-mode)))

(define-command save-non-default-modes-for-future-visits ()
  "Save the modes present in `default-modes' and not present in current modes as :excluded,
and modes that are present in mode list but not in `default-modes' as :included,
to one of auto-mode rules. Apply the resulting rule for all the future visits to this URL,
inferring the matching condition with `url-infer-match'.

For the storage format see the comment in the head of your `auto-mode-rules-data-path' file."
  (let ((url (prompt-minibuffer
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
  "Store the exact list of enabled modes to auto-mode rules for all the future visits of this
domain/host/URL/group of websites inferring the suitable matching condition by user input.
Uses `url-infer-match', see its documentation for matching rules.

For the storage format see the comment in the head of your `auto-mode-rules-data-path' file."
  ;; TODO: Should it prompt for modes to save?
  ;; One may want to adjust the modes before persisting them as :exact-p rule.
  (let ((url (prompt-minibuffer
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
(sera:export-always 'add-modes-to-auto-mode-rules)
(defun add-modes-to-auto-mode-rules (test &key (append-p nil) exclude include (exact-p nil))
  (with-data-access (rules (auto-mode-rules-path (current-buffer)))
    (let* ((rule (or (find test rules
                           :key #'test :test #'equal)
                     (make-instance 'auto-mode-rule :test test)))
           (auto-mode (find-mode (current-buffer) 'auto-mode))
           (include (rememberable-of include auto-mode))
           (exclude (rememberable-of exclude auto-mode)))
      (setf (exact-p rule) exact-p
            (included rule) (union include
                                   (when append-p
                                     (set-difference (included rule) exclude
                                                     :test #'mode-equal)))
            (excluded rule) (union exclude
                                   (when append-p
                                     (set-difference (excluded rule) include
                                                     :test #'mode-equal)))
            rules (delete-duplicates
                   (append (when (or (included rule) (excluded rule))
                             (list rule))
                           rules)
                   :key #'test :test #'equal)))
    rules))

(defmethod serialize-object ((rule auto-mode-rule) stream)
  (flet ((write-if-present (slot &key modes-p)
           (when (funcall slot rule)
             (format t " :~a ~s"
                     slot
                     (let ((value (funcall slot rule)))
                       (if modes-p
                           (mapcar #'maybe-mode-name value)
                           value))))))
    (let ((*standard-output* stream)
          (*print-case* :downcase))
      (write-string "(" stream)
      (if (and (eq (first (test rule)) 'match-url)
               (= 2 (length (test rule))))
          (format t "~s " (second (test rule)))
          (format t "~s " (test rule)))
      (write-if-present 'included :modes-p t)
      (write-if-present 'excluded :modes-p t)
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

(defmethod store ((profile data-profile) (path auto-mode-rules-data-path))
  (with-data-file (file path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (let ((*package* (find-package :nyxt/auto-mode)))
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
;; Included modes is a list of mode symbols, or a list of lists in the form
;; of (MODE-SYMBOL INIT-ARGS), where init-args are passed to the mode when instantiated.
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
    (dolist (rule (get-data path))
      (write-char #\newline file)
      (serialize-object rule file))
    (format file "~%)~%")
    (echo "Saved ~a auto-mode rules to ~s." (length (get-data path)) (expand-path path)))))

(defmethod restore ((profile data-profile) (path auto-mode-rules-data-path))
  (handler-case
      (let ((data (with-data-file (file path
                                        :direction :input
                                        :if-does-not-exist nil)
                    (when file
                      (let ((*package* (find-package :nyxt/auto-mode)))
                        (deserialize-auto-mode-rules file))))))
        (when data
          (echo "Loading ~a auto-mode rules from ~s." (length data) (expand-path path))
          (setf (get-data path) data)))
    (error (c)
      (echo-warning "Failed to load auto-mode-rules from ~s: ~a" (expand-path path) c))))
