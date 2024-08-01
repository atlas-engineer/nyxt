;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class auto-rules-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"auto-rules")
   (files:name "auto-rules"))
  (:export-class-name-p t))

(defmethod rememberable-p ((mode symbol))
  (check-type mode sym:mode-symbol)
  ;; FIXME: Can this finalization shoot us in the foot?
  (closer-mop:ensure-finalized (find-class mode) nil)
  (closer-mop:slot-definition-initform
   (find 'rememberable-p (closer-mop:class-slots (find-class mode))
         :key #'closer-mop:slot-definition-name)))

(defmethod rememberable-p ((mode list))
  (check-type (first mode) sym:mode-symbol)
  (rememberable-p (first mode)))

(deftype mode-invocation ()
  ;; First mode name, then `make-instance' args for it.
  `(cons sym:mode-symbol trivial-types:property-list))

(deftype rememberable-mode-invocation ()
  `(and mode-invocation (satisfies rememberable-p)))

(-> normalize-mode (t) (maybe mode-invocation))
(defun normalize-mode (mode)
  "Get MODE to the normalized state:
- a mode symbol,
- or list with mode symbol in the head."
  (typecase mode
    (mode (list (name mode)))
    ((cons sym:mode-symbol *) mode)
    (sym:mode-symbol (list mode))
    (keyword (normalize-mode (resolve-user-symbol mode :mode)))
    (t nil)))

(-> normalize-modes (list) (maybe (cons mode-invocation *)))
(defun normalize-modes (modes)
  "Apply `normalize-mode' to all the MODES."
  (mapcar #'normalize-mode modes))

(-> rememberable-of
    ((or (cons t *) null))
    (values (maybe (cons mode-invocation *)) &optional))
(defun rememberable-of (modes)
  "Filter MODES based on `rememberable-p'."
  (normalize-modes (sera:filter #'rememberable-p modes)))

(defun mode= (m1 m2)
  (eq (first (normalize-mode m1))
      (first (normalize-mode m2))))

(define-class auto-rule ()
  ((test
    (error "Slot `test' should be set.")
    :type list
    :documentation "The s-expression evaluating to a one-argument predicate for URL.")
   (included
    '()
    :type (or (cons rememberable-mode-invocation *) null)
    :documentation "The list of `mode-invocations' to enable on rule activation.")
   (excluded
    '()
    :type (or (cons rememberable-mode-invocation *) null)
    :documentation "The list of `mode-invocations' to disable on rule activation.")
   (exact-p
    nil
    :type boolean
    :documentation "Whether to exclusively enable the `included' modes."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A representation of a URL-matching rule.
- `test' mandates which `request-data' to match.
- `included' and `excluded' lists of modes to enable/disable (respectively).
- `exact-p' is whether the `included'/`excluded' are applied exclusively, with
  all the other modes disabled."))

(defmethod print-object ((rule auto-rule) stream)
  (print-unreadable-object (rule stream :type t :identity t)
    (princ (sera:ellipsize (format nil "~a" (test rule)) 40) stream)))

(defvar *default-auto-rules* '()
  "The list of rules to always use, even when there is no auto-rules file.")

(export-always 'define-auto-rule)
(defun define-auto-rule (test &key excluded included exact-p)
  "Define a new default `auto-rule'.
ARGS as in make-instance of `auto-rule'."
  (push (apply #'make-instance
               'auto-rule
               :exact-p exact-p
               :test (if (stringp test)
                         `(match-url ,test)
                         test)
               (append
                (when included
                  (list :included (normalize-modes (uiop:ensure-list included))))
                (when excluded
                  (list :excluded (normalize-modes (uiop:ensure-list excluded))))))
        *default-auto-rules*))

(export-always 'undefine-auto-rule)
(defgeneric undefine-auto-rule (token)
  (:method ((token list))
    (loop for rule in *default-auto-rules*
          when (equal token (test rule))
            do (alex:deletef *default-auto-rules* rule :test #'equal)))
  (:method ((token string))
    (undefine-auto-rule (list 'match-url token)))
  (:documentation "Remove the rule with TOKEN test."))

(-> matching-auto-rules (quri:uri modable-buffer) (values list &optional))
(defun matching-auto-rules (url buffer)
  (let* ((rules (append *default-auto-rules*
                        (files:content (auto-rules-file buffer))))
         (matching-rules (remove-if-not
                          #'(lambda (rule)
                              (funcall (eval (test rule)) url))
                          rules)))
    (if (apply-all-matching-auto-rules-p buffer)
        matching-rules
        (flet ((priority (test1 test2)
                 "Prioritize the rules based on their predicate.

- MATCH-REGEX is the last option one has for granular matching, so we put it as
  highest priority.
- MATCH-URL matches just one page, so it follows regex in granularity.
- And then, host, domain, and scheme all go in the decreasing order of
  specificity and priority.  It's made this way so that more specific rules can
  override/circumvent the less specific rules.
- User-defined and other predicates always go last, because those are
  unpredictable and should only be used when the default ones don't fit."
                 (let ((priority-list '(match-regex match-url match-host match-domain match-scheme)))
                   (< (or (position (first test1) priority-list) (length priority-list))
                      (or (position (first test2) priority-list) (length priority-list))))))
          (uiop:ensure-list
           (first (sort
                   matching-rules
                   #'priority :key #'test)))))))

(-> enable-matching-modes (quri:uri modable-buffer) *)
(defun enable-matching-modes (url buffer)
  (alex:when-let ((rules (matching-auto-rules url buffer)))
    (dolist (rule rules)
      (dolist (mode (set-difference
                     (included rule)
                     (rememberable-of (modes buffer))
                     :test #'mode=))
        (check-type mode rememberable-mode-invocation)
        (apply #'enable-modes* (first mode) buffer (rest mode)))
      (alex:when-let ((modes (mapcar #'first
                                     (if (exact-p rule)
                                         (set-difference
                                          (rememberable-of (modes buffer))
                                          (included rule)
                                          :test #'mode=)
                                         (excluded rule)))))
        (disable-modes* modes buffer)))))

(defun can-save-last-active-modes-p (buffer url)
  (and url
       (or (null (last-active-modes-url buffer))
           (not (quri:uri= url (last-active-modes-url buffer)))
           (not (matching-auto-rules (previous-url buffer) buffer)))))

(defun save-last-active-modes (buffer url)
  (when (can-save-last-active-modes-p buffer url)
    (setf (last-active-modes buffer) (normalize-modes (modes buffer))
          (last-active-modes-url buffer) url)))

(defun reapply-last-active-modes (buffer)
  (alex:when-let ((modes (mapcar #'first
                                 (set-difference (normalize-modes (modes buffer))
                                                 (last-active-modes buffer)
                                                 :test #'mode=))))
    (disable-modes* modes buffer))
  (alex:when-let ((modes (mapcar #'first
                                 (set-difference (last-active-modes buffer)
                                                 (normalize-modes (modes buffer))
                                                 :test #'mode=))))
    (enable-modes* modes buffer)))

(-> url-infer-match (url-designator) list)
(defun url-infer-match (url)
  "Infer the best `test' for `auto-rule', based on the form of URL.

The rules are:
- If it's a plain domain-only URL (i.e. \"http://domain.com\") -- then use
  `match-domain'.
- If it's host with subdomains (\"http://whatever.subdomain.domain.com\") -- use
  `match-host'.
- Use `match-url' otherwise."
  (let* ((url (or (ignore-errors (quri:uri url)) url))
         (host-url-p (and (quri:uri-p url)
                          (empty-path-url-p url)
                          (host-only-url-p url))))
    (cond
      ((and host-url-p
            (string= (quri:uri-domain url)
                     (ppcre:regex-replace "www[0-9]?\\." (quri:uri-host url) "")))
       `(match-domain ,(quri:uri-domain url)))
      (host-url-p
       `(match-host ,(quri:uri-host url)))
      (t `(match-url ,(render-url url))))))

(defun remember-on-mode-toggle (modes buffers &key (enabled-p t))
  (dolist (buffer (uiop:ensure-list buffers))
    (if (prompt-on-mode-toggle-p buffer)
        (sera:and-let* ((invocations (mapcar #'normalize-mode (uiop:ensure-list modes)))
                        (invocations (remove-if (rcurry #'mode-covered-by-auto-rules-p buffer enabled-p) invocations)))
          (if-confirm ((format nil
                               "Permanently ~:[disable~;enable~] ~{~a~^, ~} for ~a?"
                               enabled-p (mapcar #'first invocations) (url buffer)))
              (let ((url (prompt1
                          :prompt "URL"
                          :input (render-url (url buffer))
                          :sources 'prompter:raw-source)))
                (add-modes-to-auto-rules (url-infer-match url)
                                         :append-p t
                                         :include (when enabled-p invocations)
                                         :exclude (unless enabled-p invocations)))
              (setf (last-active-modes buffer)
                    (if enabled-p
                        (union invocations (last-active-modes buffer)
                               :test #'mode=)
                        (set-difference (last-active-modes buffer) invocations
                                        :test #'mode=)))))
        (save-last-active-modes buffer (url buffer)))))

(defmethod enable-modes* :after (modes buffers &rest keys &key remember-p &allow-other-keys)
  (declare (ignorable modes keys))
  (when remember-p
    (dolist (buffer (uiop:ensure-list buffers))
      (save-last-active-modes buffer (url buffer)))))

(defmethod disable-modes* :after (modes buffers &rest keys &key remember-p &allow-other-keys)
  (declare (ignorable modes keys))
  (when remember-p
    (dolist (buffer (uiop:ensure-list buffers))
      (save-last-active-modes buffer (url buffer)))))

(defmethod customize-instance :after ((buffer modable-buffer) &key &allow-other-keys)
  (unless (last-active-modes buffer)
    (setf (last-active-modes buffer)
          (normalize-modes (default-modes buffer)))))

(-> mode-covered-by-auto-rules-p
    (mode-invocation buffer boolean)
    (values (or list boolean) &optional))
(defun mode-covered-by-auto-rules-p (mode buffer enable-p)
  "Says whether auto-rules in BUFFER already knows what to do with MODE.

ENABLE-P is whether mode is being enabled (non-nil) or disabled (nil).

Mode is covered if:
- `rememberable-p' is nil.
- There is a matching rule and:
  - mode is ENABLED-P and is part of `included' modes in the rule, or
  - mode is not ENABLED-P and is part of `excluded' modes in the rule.
- If there's no matching rule but it's part of `last-active-modes' and needs to
  be ENABLED-P.
- If it's getting disabled (not ENABLED-P) after being enabled by a rule on the
  previous page."
  (flet ((invocation-member (list)
           (member mode list :test #'mode=)))
    (or (not (rememberable-p mode))
        (alex:when-let ((matching-rules (matching-auto-rules (url buffer) buffer)))
          (or (and enable-p (invocation-member (alex:mappend #'included matching-rules)))
              (and (not enable-p) (invocation-member (alex:mappend #'excluded matching-rules)))))
        ;; Mode is covered by auto-rules only if it is both in
        ;; last-active-modes and gets enabled.  If it gets disabled, user
        ;; should be prompted, because they may want to persist the disabled mode.
        (and enable-p (invocation-member (last-active-modes buffer)))
        (and (not enable-p)
             (invocation-member
              (alex:when-let* ((previous-url (previous-url buffer))
                               (matching-rules (matching-auto-rules previous-url buffer)))
                (alex:mappend #'included matching-rules)))))))

(-> apply-auto-rules (quri:uri buffer) *)
(export-always 'apply-auto-rules)
(defun apply-auto-rules (url buffer)
  "Apply the rules based on the URL being loaded.
Implies that the request is a top-level one."
  (let* ((rules (matching-auto-rules url buffer))
         (previous-url (previous-url buffer))
         (previous-rules (when previous-url (matching-auto-rules previous-url buffer))))
    (unless previous-rules
      (save-last-active-modes buffer previous-url))
    (cond
      ((not rules)
       (reapply-last-active-modes buffer))
      ((and rules previous-rules)
       (reapply-last-active-modes buffer)
       (enable-matching-modes url buffer))
      ((and rules (not (eq rules previous-rules)))
       (enable-matching-modes url buffer)))
    (setf (previous-url buffer) url)))

(define-command-global save-non-default-modes-for-future-visits ()
  "Save the enabled non-default modes for future visits.

The matching URL logic is dictacted by `url-infer-match'.

This command does not save non-rememberable modes. If you want auto-rules to
remember a particular mode, configure it to be `rememberable-p' in your
configuration file.

For the storage format see the comment in the header of your `auto-rules-file'."
  (let ((url (prompt1
              :prompt "URL"
              :input (render-url (url (current-buffer)))
              :sources (list
                        (make-instance 'prompter:raw-source
                                       :name "New URL")
                        (make-instance 'global-history-source
                                       :actions-on-return #'identity)))))
    (when (typep url 'nyxt::history-entry)
      (setf url (url url)))
    (add-modes-to-auto-rules
     (url-infer-match url)
     :include (set-difference (normalize-modes (modes (current-buffer)))
                              (normalize-modes (default-modes (current-buffer)))
                              :test #'mode=)
     :exclude (set-difference (normalize-modes (default-modes (current-buffer)))
                              (normalize-modes (modes (current-buffer)))
                              :test #'mode=))))

(define-command-global save-exact-modes-for-future-visits ()
  "Save the enabled modes for future visits.

The matching URL logic is dictacted by `url-infer-match'.

This command does not save non-rememberable modes. If you want auto-rules to
save a particular mode, configure it to be `rememberable-p' in your
configuration file.

For the storage format see the comment in the header of your `auto-rules-file'."
  ;; TODO: Should it prompt for modes to save?
  ;; One may want to adjust the modes before persisting them as :exact-p rule.
  (let ((url (prompt1
              :prompt "URL"
              :input (render-url (url (current-buffer)))
              :sources (list
                        (make-instance 'prompter:raw-source
                                       :name "New URL")
                        (make-instance 'global-history-source
                                       :actions-on-return #'identity)))))
    (setf url (url url))
    (add-modes-to-auto-rules (url-infer-match url)
                             :include (rememberable-of (modes (current-buffer)))
                             :exact-p t)))

(-> add-modes-to-auto-rules
    (list &key
          (:append-p boolean)
          (:exclude (or (cons rememberable-mode-invocation *) null))
          (:buffer modable-buffer)
          (:include (or (cons rememberable-mode-invocation *) null))
          (:exact-p boolean))
    (values list &optional))
(export-always 'add-modes-to-auto-rules)
(defun add-modes-to-auto-rules (test &key (buffer (nyxt:current-buffer)) (append-p nil) exclude include (exact-p nil))
  "Add a rule with TEST, EXCLUDE, INCLUDE, EXACT-P to the set of rules in BUFFER.
Adds it to browser-global rules file by default.
If there's an existing rule:
- And APPEND-P is non-nil: add the modes to the rule.
- And replace the rule otherwise."
  (files:with-file-content (rules (auto-rules-file buffer))
    (let* ((rule (or (find test rules
                           :key #'test :test #'equal)
                     (make-instance 'auto-rule :test test)))
           (include include)
           (exclude exclude))
      (setf (exact-p rule) exact-p
            (included rule) (union include
                                   (when append-p
                                     (set-difference (included rule) exclude
                                                     :test #'mode=)))
            (excluded rule) (union exclude
                                   (when append-p
                                     (set-difference (excluded rule) include
                                                     :test #'mode=)))
            rules (delete-duplicates
                   (append (when (or (included rule) (excluded rule))
                             (list rule))
                           rules)
                   :key #'test :test #'equal)))))

(defun serialize-object (rule &optional (stream *standard-output*))
  (flet ((write-if-present (slot &key modes-p)
           (when (funcall slot rule)
             (format t " :~a ~s"
                     slot
                     (let ((value (funcall slot rule)))
                       (if modes-p
                           (mapcar
                            #'(lambda (mode-invocation)
                                (if (rest mode-invocation)
                                    `(,(first mode-invocation)
                                      ,@(rest mode-invocation))
                                    (first mode-invocation)))
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

(defmethod files:serialize ((profile nyxt-profile) (file auto-rules-file) stream &key)
  (let ((rules (files:content file)))
    (let ((*standard-output* stream)
          (*package* (find-package :nyxt)))
      (write-string ";; List of auto-rules (meant to be human-readable and human-writable)

;; Rules start on a new line and consist of one or more of the following:

;; - Conditions for rule activation. One of match-domain, match-host,
;;   match-regex or match-port (please refer to help system); a user-defined
;;   condition; or a URL between string quotes.

;; - :included (optional) -- List of the modes to enable on condition.

;; - :excluded (optional) -- List of the modes to disable on condition.

;; - :exact-p  (optional) -- Whether to enable only :included modes and disable
;;    everything else (if :exact-p is t), or just add :included and exclude
;;    :excluded modes from the current modes list (if :exact-p is nil or not
;;    present).

;; Included modes is a list of mode symbols, or a list of lists in the form of
;; (MODE-SYMBOL INIT-ARGS), where init-args are passed to the mode when
;; instantiated.

;; Conditions work this way:
;; - match-domain matches the URL domains only.
;;   Example: (match-domain \"reddit.com\") will work for all of Reddit.

;; - match-host is more specific -- it matches only on certain subdomains of
;;   the website.
;;   Example: (match-host \"old.reddit.com\") will work on old Reddit only.

;; - match-regex works for any address that matches a given regex. You can add
;;   these manually, but remember: with great regex comes great responsibility!
;;   Example: (match-regex \"https://github\\.com/.*/.*\") will match only in
;;   repos on GitHub.

;; - match-port matches the port number(s) only.
;;   Example: (match-port 8000 8080) will work for e.g. localhost:8000,
;;   127.0.0.1:8080, etc.

;; - String format matches the exact URL and nothing else
;;   Example: \"https://lispcookbook.github.io/cl-cookbook/pattern_matching.html\"
;;   will work on the Pattern Matching article of CL Cookbook, and nowhere else.

;; - Any other Lisp form is evaluated and the result of it is called with the
;;   URL as an argument. This means you can write arbitrary Lisp code to
;;   activate auto-rules.
;;   Note: The URL is passed as quri:uri object.
;;   Example: (lambda (url) (string= \"/my/path\" (quri:uri-path url)))

;; You can write additional URLs in the parenthesized conditions, to reuse the
;; rule for other URL.
;; Example: (match-host \"reddit.com\" \"old.reddit.com\" \"www6.reddit.com\")

;;; Rules:

")
      (write-string "(")
      (dolist (rule rules)
        (write-char #\newline)
        (serialize-object rule))
      (format t "~%)~%"))
    (echo "Saved ~a auto-rules to ~s." (length rules) (files:expand file))))

(defmethod files:deserialize ((profile nyxt-profile) (file auto-rules-file) raw-content &key)
  (let ((*package* (find-package :nyxt))
        (rules (safe-read raw-content)))
    (mapcar #'(lambda (rule)
                (let ((rule (append '(:test) rule)))
                  (setf (getf rule :included) (normalize-modes (getf rule :included))
                        (getf rule :excluded) (normalize-modes (getf rule :excluded)))
                  (when (stringp (getf rule :test))
                    (setf (getf rule :test) `(match-url ,(getf rule :test))))
                  (apply #'make-instance 'auto-rule rule)))
            rules)))
