;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/auto-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for automatic URL-based mode toggling."))
(in-package :nyxt/auto-mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)
  (trivial-package-local-nicknames:add-package-local-nickname :hooks :serapeum/contrib/hooks))

(define-class mode-invocation ()
  ((name (error "Mode invocation should have a name to call mode through.")
         :type symbol
         :documentation "Mode symbol to call the mode with.
Package prefix is optional.")
   (arguments nil
              :type list
              :documentation "Arguments to activate the mode with."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(sera:export-always 'equals)
(defmethod equals ((object1 t) (object2 t))
  (equalp object1 object2))
(defmethod equals ((mode1 mode-invocation) (mode2 mode-invocation))
  (string-equal (symbol-name (name mode1))
                (symbol-name (name mode2))))

(defgeneric mode-invocation (mode)
  (:method ((mode mode-invocation))
    mode)
  (:method ((mode nyxt:root-mode))
    (make-instance 'mode-invocation :name (mode-name mode)))
  (:method ((mode symbol))
    (alex:if-let ((command (nyxt::mode-command mode)))
      (make-instance 'mode-invocation :name (nyxt:name command))
      (echo-warning "Auto-mode rule: unknown mode symbol ~s" mode)))
  (:method ((mode list))
    (check-type mode (cons symbol *))
    (make-instance 'mode-invocation
                   :name (first mode)
                   :arguments (rest mode))))

(defun mode-invocations (mode-list)
  "Return the mode invocations corresponding to mode specifiers in MODE-LIST.
If the mode specifier is not known, it's omitted from the results."
  (delete nil (mapcar #'mode-invocation mode-list)))

(defmethod rememberable-p ((mode-invocation mode-invocation))
  (rememberable-p (apply #'make-instance (name mode-invocation)
                         (arguments mode-invocation))))

(declaim (ftype (function ((or (cons (or mode-invocation root-mode) *) null))
                          (values (or (cons mode-invocation *) null) &optional))
                rememberable-of))
(defun rememberable-of (modes)
  "Filter MODES based on `rememberable-p'."
  (mode-invocations (remove-if (complement #'rememberable-p) modes)))

(define-class auto-mode-rule ()
  ((test (error "Slot `test' should be set.")
         :type list)
   (included '()
             :type (or (cons mode-invocation *) null)
             :documentation "The list of `mode-invocation's to enable on rule activation.")
   (excluded '()
             :type (or (cons mode-invocation *) null)
             :documentation "The list of `mode-invocation's to disable on rule activation.")
   (exact-p nil
            :type boolean
            :documentation "Whether to exclusively enable the `included' modes."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(declaim (ftype (function (quri:uri buffer) (or auto-mode-rule null))
                matching-auto-mode-rule))
(defun matching-auto-mode-rule (url buffer)
  (with-data-unsafe (rules (auto-mode-rules-path buffer))
    (flet ((priority (test1 test2)
             (let ((priority-list '(match-regex match-url match-host match-domain)))
               (< (or (position (first test1) priority-list) 4)
                  (or (position (first test2) priority-list) 4)))))
      (first (sort (remove-if-not
                    #'(lambda (rule)
                        (funcall
                         ;; REVIEW: Use `eval'?
                         (apply (first (test rule)) (rest (test rule))) url))
                    rules)
                   #'priority :key #'test)))))

(declaim (ftype (function (quri:uri buffer)) enable-matching-modes))
(defun enable-matching-modes (url buffer)
  (let ((rule (matching-auto-mode-rule url buffer)))
    (dolist (mode-invocation (set-difference
                               (included rule)
                               (rememberable-of (modes buffer))
                               :test #'equals))
      (check-type mode-invocation mode-invocation)
      (enable-modes (list (name mode-invocation)) buffer (arguments mode-invocation)))
    (disable-modes (mapcar #'name
                           (if (exact-p rule)
                               (set-difference
                                (rememberable-of (modes buffer))
                                (included rule) :test #'equals)
                               (excluded rule)))
                   buffer)))

(defun can-save-last-active-modes (auto-mode url)
  (or (null (last-active-modes-url auto-mode))
      (not (quri:uri= url (last-active-modes-url auto-mode)))))

(defun save-last-active-modes (auto-mode url)
  (when (can-save-last-active-modes auto-mode url)
    (setf (last-active-modes auto-mode) (mode-invocations (modes (buffer auto-mode)))
          (last-active-modes-url auto-mode) url)))

(defun reapply-last-active-modes (auto-mode)
  (disable-modes
   (mapcar #'name
           (set-difference (mode-invocations (modes (buffer auto-mode)))
                           (last-active-modes auto-mode)
                           :test #'equals))
   (buffer auto-mode))
  (enable-modes
   (mapcar #'name
           (set-difference (last-active-modes auto-mode)
                           (mode-invocations (modes (buffer auto-mode)))
                           :test #'equals))
   (buffer auto-mode)))

(defun new-page-request-p (request-data)
  "Whether the REQUEST-DATA is a request for a new page load.
Resource/font/ads/anchor loads are safely ignored.

It relies on the fact that, due to the WebKit limitations, we store the loaded
URL in the buffer slot when we need to load a new page, while, for
non-new-page requests, buffer URL is not altered."
  (quri:uri= (url request-data) (url (buffer request-data))))

(defun auto-mode-handler (request-data)
  (let* ((auto-mode (find-submode (buffer request-data) 'auto-mode))
         (rule (matching-auto-mode-rule (url request-data) (buffer request-data)))
         (previous-url (previous-url auto-mode))
         (previous-rule (when previous-url (matching-auto-mode-rule previous-url (buffer request-data)))))
    (when (and rule previous-url (not previous-rule))
      (save-last-active-modes auto-mode previous-url))
    (cond
      ((and (not rule) (new-page-request-p request-data))
       (reapply-last-active-modes auto-mode))
      ((and rule (not (eq rule previous-rule)))
       (enable-matching-modes (url request-data) (buffer request-data))))
    (setf (previous-url auto-mode) (url request-data)))
  request-data)

(declaim (ftype (function (root-mode auto-mode boolean) (values (or list boolean) &optional))
                mode-covered-by-auto-mode-p))
(defun mode-covered-by-auto-mode-p (mode auto-mode enable-p)
  "Says whether AUTO-MODE already knows what to do with MODE.
ENABLE-P is whether mode is being enabled (non-nil) or disabled (nil).
Mode is covered if:
- It's not rememberable (has `rememberable-p' set to nil).
- There is a rule it matches and:
  - when mode is ENABLED-P and part of `included' modes in the rule, or
  - when mode is not ENABLED-P and part of `excluded' modes in the rule.
- If there's no matching rule but it's part of `last-active-modes' and needs to be ENABLED-P.
- If it's getting disabled (not ENABLED-P) after being enabled by a rule on the previous page."
  (let ((invocation (mode-invocation mode)))
    (flet ((invocation-member (list)
             (member invocation list :test #'equals)))
      (or (not (rememberable-p mode))
          (alex:when-let ((matching-rule (matching-auto-mode-rule (url (buffer auto-mode))
                                                                  (buffer auto-mode))))
            (or (and enable-p (invocation-member (included matching-rule)))
                (and (not enable-p) (invocation-member (excluded matching-rule)))))
          ;; Mode is covered by auto-mode only if it is both in
          ;; last-active-modes and gets enabled.  If it gets disabled, user
          ;; should be prompted, because they may want to persist it.
          (and enable-p (invocation-member (last-active-modes auto-mode)))
          (and (not enable-p)
               (invocation-member
                (alex:when-let* ((previous-url (previous-url auto-mode))
                                 (matching-rule (matching-auto-mode-rule previous-url
                                                                         (buffer auto-mode))))
                  (included matching-rule))))))))

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
        `(match-url ,(render-url url)))))

(declaim (ftype (function (boolean t) (function (root-mode)))
                make-mode-toggle-prompting-handler))
(defun make-mode-toggle-prompting-handler (enable-p auto-mode)
  #'(lambda (mode)
      (let ((invocation (mode-invocation mode)))
        (when (not (mode-covered-by-auto-mode-p mode auto-mode enable-p))
          (if-confirm ("Permanently ~:[disable~;enable~] ~a for this URL?"
                       enable-p (mode-name mode))
                      (let ((url (first (prompt
                                         :prompt "URL:"
                                         :input (render-url (url (buffer mode)))
                                         :sources (make-instance 'prompter:raw-source)))))
                        (add-modes-to-auto-mode-rules (url-infer-match url)
                                                      :append-p t
                                                      :include (when enable-p (list invocation))
                                                      :exclude (unless enable-p (list invocation))))
                      (setf (last-active-modes auto-mode)
                            (if enable-p
                                (union (list invocation) (last-active-modes auto-mode)
                                       :test #'equals)
                                (remove invocation (last-active-modes auto-mode)
                                        :test #'equals))))))))

(defun initialize-auto-mode (mode)
  (unless (last-active-modes mode)
    (setf (last-active-modes mode)
          (mode-invocations (default-modes (buffer mode)))))
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
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "auto-mode"
      scheme:cua
      (list
       "C-R" 'reload-with-modes)))
   (prompt-on-mode-toggle nil
                          :type boolean
                          :documentation "Whether the user is asked to confirm
adding the rule corresponding to a mode toggle.")
   (previous-url nil
                 :type (or quri:uri null)
                 :documentation "The last URL for which `auto-mode-handler' was fired.
We need to know if the auto mode rule has been applied before to avoid
re-applying a rule for a sequence of pages that match the same rule.

We can'rely on the previous history entry because
dead buffers and session-restored buffers may have a history with a previous URL
matching the same rule while obviously the rule has never been applied for the
new-born buffer.")

   (last-active-modes-url nil
                          :type (or quri:uri null)
                          :documentation "The last URL that the active modes were saved for.
We need to store this to not overwrite the `last-active-modes' for a given URL,
if `auto-mode-handler' is fired more than once.")
   (last-active-modes '()
                      :type (or (cons mode-invocation *) null)
                      :documentation "The list of `mode-invocation's that were enabled
on the last URL not covered by `auto-mode'.
This is useful when alternative between rule-less and ruled pages.
Example browse sequence:

- https://example.org (noscript-mode noimage-mode) ; No rule.
- https://nyxt.atlas.engineer (dark-mode) ; Rule
- https://en.wikipedia.org (noscript-mode noimage-mode) ; No rule.

In the above, when browsing from nyxt.atlas.engineer to en.wikipedia.org, the
modes that were in place before the nyxt.atlas.engineer rule was applied are
restored.")
   (destructor #'clean-up-auto-mode)
   (constructor #'initialize-auto-mode)))

(define-command save-non-default-modes-for-future-visits ()
  "Save the modes present in `default-modes' and not present in current modes as :excluded,
and modes that are present in mode list but not in `default-modes' as :included,
to one of auto-mode rules. Apply the resulting rule for all the future visits to this URL,
inferring the matching condition with `url-infer-match'.

For the storage format see the comment in the head of your `auto-mode-rules-data-path' file."
  (let ((url (first (prompt
                     :prompt "URL:"
                     :input (render-url (url (current-buffer)))
                     :sources (list
                               (make-instance 'prompter:raw-source
                                              :name "New URL")
                               (make-instance 'user-global-history-source
                                              :actions '()))))))
    (when (typep url 'nyxt::history-entry)
      (setf url (url url)))
    (add-modes-to-auto-mode-rules
     (url-infer-match url)
     :include (set-difference (mode-invocations (modes (current-buffer)))
                              (mode-invocations (default-modes (current-buffer)))
                              :test #'equals)
     :exclude (set-difference (mode-invocations (default-modes (current-buffer)))
                              (mode-invocations (modes (current-buffer)))
                              :test #'equals))))

(define-command save-exact-modes-for-future-visits ()
  "Store the exact list of enabled modes to auto-mode rules for all the future visits of this
domain/host/URL/group of websites inferring the suitable matching condition by user input.
Uses `url-infer-match', see its documentation for matching rules.

For the storage format see the comment in the head of your `auto-mode-rules-data-path' file."
  ;; TODO: Should it prompt for modes to save?
  ;; One may want to adjust the modes before persisting them as :exact-p rule.
  (let ((url (first (prompt
                     :prompt "URL:"
                     :input (render-url (url (current-buffer)))
                     :sources (list
                               (make-instance 'prompter:raw-source
                                              :name "New URL")
                               (make-instance 'user-global-history-source
                                              :actions '()))))))
    (when (typep url 'nyxt::history-entry)
      (setf url (url url)))
    (add-modes-to-auto-mode-rules (url-infer-match url)
                                  :include (mode-invocations (modes (current-buffer)))
                                  :exact-p t)))

(define-command reload-with-modes (&optional (buffer (current-buffer)))
  "Reload the buffer with the queried modes.
This bypasses auto-mode.
Auto-mode is re-enabled once the page is reloaded."
  (let* ((modes-to-enable (prompt
                           :prompt "Mark modes to enable, unmark to disable"
                           :sources (make-instance 'user-mode-source
                                                   :marks (remove 'nyxt/auto-mode:auto-mode
                                                                  (mapcar #'mode-name (modes (current-buffer)))))))
         (modes-to-disable (cons 'nyxt/auto-mode:auto-mode
                                 (set-difference (nyxt::all-mode-names) modes-to-enable
                                                 :test #'string=))))
    (hooks:add-hook (request-resource-hook buffer)
                    (make-handler-resource
                     (lambda (request-data)
                       (auto-mode :activate t)
                       (hooks:remove-hook (request-resource-hook buffer)
                                          'auto-mode-reenable)
                       request-data)
                     :name 'auto-mode-reenable))
    (disable-modes (uiop:ensure-list modes-to-disable) buffer)
    (enable-modes (uiop:ensure-list modes-to-enable) buffer)
    (nyxt::reload-buffers (list buffer))))

(declaim (ftype (function (list &key (:append-p boolean) (:exclude (or (cons mode-invocation *) null))
                                (:include (or (cons mode-invocation *) null)) (:exact-p boolean))
                          (values list &optional))
                add-modes-to-auto-mode-rules))
(sera:export-always 'add-modes-to-auto-mode-rules)
(defun add-modes-to-auto-mode-rules (test &key (append-p nil) exclude include (exact-p nil))
  (with-data-access (rules (auto-mode-rules-path (current-buffer)))
    (let* ((rule (or (find test rules
                           :key #'test :test #'equal)
                     (make-instance 'auto-mode-rule :test test)))
           (include (rememberable-of include))
           (exclude (rememberable-of exclude)))
      (setf (exact-p rule) exact-p
            (included rule) (union include
                                   (when append-p
                                     (set-difference (included rule) exclude
                                                     :test #'equals)))
            (excluded rule) (union exclude
                                   (when append-p
                                     (set-difference (excluded rule) include
                                                     :test #'equals)))
            rules (delete-duplicates
                   (append (when (or (included rule) (excluded rule))
                             (list rule))
                           rules)
                   :key #'test :test #'equal)))
    rules))

(defmethod serialize-object ((rule auto-mode-rule) stream)
  (flet ((write-if-present (slot &key modes-p)
           (when (funcall slot rule)
             (format t " :~a ~a"
                     slot
                     (let ((value (funcall slot rule)))
                       (if modes-p
                           (mapcar
                            #'(lambda (mode-invocation)
                                (if (arguments mode-invocation)
                                    (list (name mode-invocation)
                                          (arguments mode-invocation))
                                    (name mode-invocation)))
                            value)
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
  (let ((*standard-input* stream))
    (let ((rules (read stream)))
      (mapcar #'(lambda (rule)
                  (let ((rule (append '(:test) rule)))
                    (setf (getf rule :included) (mode-invocations (getf rule :included))
                          (getf rule :excluded) (mode-invocations (getf rule :excluded)))
                    (when (stringp (getf rule :test))
                      (setf (getf rule :test) `(match-url ,(getf rule :test))))
                    (apply #'make-instance 'auto-mode-rule rule)))
              rules))))

(defmethod store ((profile data-profile) (path auto-mode-rules-data-path) &key &allow-other-keys)
  (with-data-file-output (file path)
    (let ((*package* (find-package :nyxt/auto-mode))
          (rules (get-data path)))
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
    (dolist (rule rules)
      (write-char #\newline file)
      (serialize-object rule file))
    (format file "~%)~%")
    (echo "Saved ~a auto-mode rules to ~s." (length rules) (expand-path path)))))

(defmethod restore ((profile data-profile) (path auto-mode-rules-data-path) &key &allow-other-keys)
  (with-protect ("Failed to load auto-mode-rules from ~s: ~a"
                      (expand-path path) :condition)
    (let ((data (with-data-file (file path
                                      :direction :input
                                      :if-does-not-exist nil)
                  (when file
                    (let ((*package* (find-package :nyxt/auto-mode)))
                      (deserialize-auto-mode-rules file))))))
      (when data
        (echo "Loading ~a auto-mode rules from ~s." (length data) (expand-path path))
        (setf (get-data path) data)))))
