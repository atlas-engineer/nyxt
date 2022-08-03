;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class auto-rules-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"auto-rules")
   (files:name "auto-rules"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

;; REVIEW: Do we still need this?
(define-class mode-invocation ()
  ((name
    (error "Mode invocation should have a name to call mode through.")
    :type symbol
    :documentation "Mode symbol to call the mode with.
Package prefix is optional.")
   (arguments
    nil
    :type list
    :documentation "Arguments to activate the mode with."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(sera:export-always 'equals)
(defmethod equals ((object1 t) (object2 t))
  (equalp object1 object2))
(defmethod equals ((mode1 mode-invocation) (mode2 mode-invocation))
  (eq (name mode1)
      (name mode2)))

(defgeneric mode-invocation (mode)
  (:method ((mode mode-invocation))
    mode)
  (:method ((mode nyxt:mode))
    (when (rememberable-p mode)
      (make-instance 'mode-invocation :name (sera:class-name-of mode))))
  (:method ((mode symbol))
    (alex:if-let ((full-mode-symbol (if (and (not (keywordp mode)) (mode-class mode))
                                        mode
                                        (if (keywordp mode)
                                            (resolve-symbol mode :mode)
                                            (alex:when-let ((resolved-symbol (nyxt::resolve-symbol (string mode) :mode)))
                                              (log:warn "Auto-rule: non-package-prefixed symbols are deprecated, use prefixed symbols or keywords instead: ~s" mode)
                                              resolved-symbol)))))
      (when (rememberable-p (make-instance full-mode-symbol))
        (make-instance 'mode-invocation :name full-mode-symbol))
      (log:warn "Auto-rule: unknown mode symbol ~s" mode)))
  (:method ((mode list))
    (check-type mode (cons symbol *))
    (when (rememberable-p (make-instance (class-of (first mode))))
      (make-instance 'mode-invocation
                     :name (sera:class-name-of (first mode))
                     :arguments (rest mode)))))

(defun mode-invocations (mode-list)
  "Return the mode invocations corresponding to mode specifiers in MODE-LIST.
If the mode specifier is not known, it's omitted from the results."
  (delete nil (mapcar #'mode-invocation mode-list)))

(defmethod rememberable-p ((mode-invocation mode-invocation))
  (rememberable-p (apply #'make-instance (name mode-invocation)
                         (arguments mode-invocation))))

(-> rememberable-of
    ((or (cons (or mode-invocation mode) *) null))
    (values (or (cons mode-invocation *) null) &optional))
(defun rememberable-of (modes)
  "Filter MODES based on `rememberable-p'."
  (mode-invocations (sera:filter #'rememberable-p modes)))

(define-class auto-rule ()
  ((test
    (error "Slot `test' should be set.")
    :type list)
   (included
    '()
    :type (or (cons mode-invocation *) null)
    :documentation "The list of `mode-invocation's to enable on rule activation.")
   (excluded
    '()
    :type (or (cons mode-invocation *) null)
    :documentation "The list of `mode-invocation's to disable on rule activation.")
   (exact-p
    nil
    :type boolean
    :documentation "Whether to exclusively enable the `included' modes."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod print-object ((rule auto-rule) stream)
  (print-unreadable-object (rule stream :type t :identity t)
    (princ (sera:ellipsize (format nil "~a" (test rule)) 40) stream)))

(defvar *default-rules* '()
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
                  (list :included (mode-invocations included)))
                (when excluded
                  (list :excluded (mode-invocations excluded)))))
        *default-rules*))

(-> matching-auto-rule (quri:uri modable-buffer) (or auto-rule null))
(defun matching-auto-rule (url buffer)
  (let ((rules (append *default-rules*
                       (files:content (auto-rules-file buffer)))))
    (flet ((priority (test1 test2)
             (let ((priority-list '(match-regex match-url match-host match-domain match-scheme)))
               (< (or (position (first test1) priority-list) (length priority-list))
                  (or (position (first test2) priority-list) (length priority-list))))))
      (first (sort
              (remove-if-not
               #'(lambda (rule)
                   (funcall (eval (test rule)) url))
               rules)
              #'priority :key #'test)))))

(-> enable-matching-modes (quri:uri modable-buffer) *)
(defun enable-matching-modes (url buffer)
  (alex:when-let ((rule (matching-auto-rule url buffer)))
    (dolist (mode-invocation (set-difference
                              (included rule)
                              (rememberable-of (modes buffer))
                              :test #'equals))
      (check-type mode-invocation mode-invocation)
      (apply #'enable-modes :modes (name mode-invocation)
                            :buffers buffer
                            :bypass-auto-rules-p t
                            (arguments mode-invocation)))
    (alex:when-let ((modes (mapcar #'name
                                   (if (exact-p rule)
                                       (set-difference
                                        (rememberable-of (modes buffer))
                                        (included rule) :test #'equals)
                                       (excluded rule)))))
      (disable-modes :modes modes :buffers buffer :bypass-auto-rules-p t))))

(defun can-save-last-active-modes (buffer url)
  (or (null (last-active-modes-url buffer))
      (not (quri:uri= url (last-active-modes-url buffer)))))

(defun save-last-active-modes (buffer url)
  (when (can-save-last-active-modes buffer url)
    (setf (last-active-modes buffer) (mode-invocations (modes buffer))
          (last-active-modes-url buffer) url)))

(defun reapply-last-active-modes (buffer)
  (alex:when-let ((modes (mapcar #'name
                                 (set-difference (mode-invocations (modes buffer))
                                                 (last-active-modes buffer)
                                                 :test #'equals))))
    (disable-modes :modes modes :buffers buffer :bypass-auto-rules-p t))
  (alex:when-let ((modes (mapcar #'name
                                 (set-difference (last-active-modes buffer)
                                                 (mode-invocations (modes buffer))
                                                 :test #'equals))))
    (enable-modes :modes modes :buffest buffer :bypass-auto-rules-p t)))

(-> url-infer-match (string) list)
(defun url-infer-match (url)
  "Infer the best `test' for `auto-rule', based on the form of URL.
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

(-> make-mode-toggle-prompting-handler (boolean modable-buffer) (function (mode)))
(defun make-mode-toggle-prompting-handler (enable-p buffer)
  #'(lambda (mode)
      (sera:and-let* ((_ (not (bypass-auto-rules-p buffer)))
                      (_ (prompt-on-mode-toggle-p buffer))
                      (invocation (mode-invocation mode))
                      (*interactive-p* t))
        (when (not (mode-covered-by-auto-rules-p mode buffer enable-p))
          (if-confirm ((format nil
                               "Permanently ~:[disable~;enable~] ~a for this URL?"
                               enable-p (sera:class-name-of mode)))
                      (let ((url (prompt1
                                   :prompt "URL"
                                   :input (render-url (url (buffer mode)))
                                   :sources 'prompter:raw-source)))
                        (add-modes-to-auto-rules (url-infer-match url)
                                                 :append-p t
                                                 :include (when enable-p (list invocation))
                                                 :exclude (unless enable-p (list invocation))))
                      (setf (last-active-modes buffer)
                            (if enable-p
                                (union (list invocation) (last-active-modes buffer)
                                       :test #'equals)
                                (remove invocation (last-active-modes buffer)
                                        :test #'equals))))))))

(defmethod customize-instance :after ((buffer modable-buffer) &key &allow-other-keys)
  (unless (last-active-modes buffer)
    (setf (last-active-modes buffer)
          (mode-invocations (default-modes buffer))))
  (when (prompt-on-mode-toggle-p buffer)
    (hooks:add-hook (enable-mode-hook buffer)
                    (make-instance 'hooks:handler
                                   :fn (make-mode-toggle-prompting-handler t buffer)
                                   :name 'enable-mode-auto-rules-handler))
    (hooks:add-hook (disable-mode-hook buffer)
                    (make-instance 'hooks:handler
                                   :fn (make-mode-toggle-prompting-handler nil buffer)
                                   :name 'disable-mode-auto-rules-handler))))

(-> mode-covered-by-auto-rules-p
    (mode buffer boolean)
    (values (or list boolean) &optional))
(defun mode-covered-by-auto-rules-p (mode buffer enable-p)
  "Says whether auto-rules in BUFFER already knows what to do with MODE.
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
          (alex:when-let ((matching-rule (matching-auto-rule (url buffer) buffer)))
            (or (and enable-p (invocation-member (included matching-rule)))
                (and (not enable-p) (invocation-member (excluded matching-rule)))))
          ;; Mode is covered by auto-rules only if it is both in
          ;; last-active-modes and gets enabled.  If it gets disabled, user
          ;; should be prompted, because they may want to persist it.
          (and enable-p (invocation-member (last-active-modes buffer)))
          (and (not enable-p)
               (invocation-member
                (alex:when-let* ((previous-url (previous-url buffer))
                                 (matching-rule (matching-auto-rule previous-url buffer)))
                  (included matching-rule))))))))

(define-command save-non-default-modes-for-future-visits ()
  "Save the modes present in `default-modes' and not present in current modes as :excluded,
and modes that are present in mode list but not in `default-modes' as :included,
to one of auto-rules. Apply the resulting rule for all the future visits to this URL,
inferring the matching condition with `url-infer-match'.

This command does not save non-rememberable modes. If you want auto-rules to
remember a particular mode, configure it to be `rememberable-p' in your
initfile.

For the storage format see the comment in the head of your `auto-rules-file'."
  (let ((url (prompt1
              :prompt "URL"
              :input (render-url (url (current-buffer)))
              :sources (list
                        (make-instance 'prompter:raw-source
                                       :name "New URL")
                        (make-instance 'global-history-source
                                       :return-actions '())))))
    (when (typep url 'nyxt::history-entry)
      (setf url (url url)))
    (add-modes-to-auto-rules
     (url-infer-match url)
     :include (set-difference (mode-invocations (modes (current-buffer)))
                              (mode-invocations (default-modes (current-buffer)))
                              :test #'equals)
     :exclude (set-difference (mode-invocations (default-modes (current-buffer)))
                              (mode-invocations (modes (current-buffer)))
                              :test #'equals))))

(define-command save-exact-modes-for-future-visits ()
  "Store the exact list of enabled modes to auto-rules for all the future visits of this
domain/host/URL/group of websites inferring the suitable matching condition by user input.
Uses `url-infer-match', see its documentation for matching rules.

This command does not save non-rememberable modes. If you want auto-rules to
save a particular mode, configure it to be `rememberable-p' in your initfile.

For the storage format see the comment in the head of your `auto-rules-file'."
  ;; TODO: Should it prompt for modes to save?
  ;; One may want to adjust the modes before persisting them as :exact-p rule.
  (let ((url (prompt1
              :prompt "URL"
              :input (render-url (url (current-buffer)))
              :sources (list
                        (make-instance 'prompter:raw-source
                                       :name "New URL")
                        (make-instance 'global-history-source
                                       :return-actions '())))))
    (when (typep url 'nyxt::history-entry)
      (setf url (url url)))
    (add-modes-to-auto-rules (url-infer-match url)
                             :include (mode-invocations (modes (current-buffer)))
                             :exact-p t)))

(-> add-modes-to-auto-rules
    (list &key
          (:append-p boolean)
          (:exclude (or (cons mode-invocation *) null))
          (:buffer modable-buffer)
          (:include (or (cons mode-invocation *) null))
          (:exact-p boolean))
    (values list &optional))
(sera:export-always 'add-modes-to-auto-rules)
(defun add-modes-to-auto-rules (test &key (buffer (nyxt:current-buffer)) (append-p nil) exclude include (exact-p nil))
  (nfiles:with-file-content (rules (auto-rules-file buffer))
    (let* ((rule (or (find test rules
                           :key #'test :test #'equal)
                     (make-instance 'auto-rule :test test)))
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
                                (if (arguments mode-invocation)
                                    `(,(name mode-invocation)
                                      ,@(arguments mode-invocation))
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

(defmethod files:serialize ((profile nyxt-profile) (file auto-rules-file) stream &key)
  (let ((rules (files:content file)))
    (let ((*standard-output* stream)
          (*package* (find-package :nyxt)))
      (write-string ";; List of auto-rules.
;; It is made to be easily readable and editable, but you still need to remember some things:
;;
;; Every rule starts on a new line and consists of one or more of the following elements:
;; - Condition for rule activation. It is either (match-domain ...),
;;   (match-host ...), (match-regex ...), (match-port ...), some other condition
;;   you define, or a string.
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
;; - match-port matches the port number(s) only.
;;   Example: (match-port 8000 8080) will work for e.g. localhost:8000, 127.0.0.1:8080, etc.
;; - String format matches the exact URL and nothing else
;;   Example: \"https://lispcookbook.github.io/cl-cookbook/pattern_matching.html\"
;;            will work on the Pattern Matching article of CL Cookbook, and nowhere else.
;;
;; You can write additional URLs in the parenthesized conditions, to reuse the rule for other URL
;; Example: (match-host \"reddit.com\" \"old.reddit.com\" \"www6.reddit.com\")
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
                  (setf (getf rule :included) (mode-invocations (getf rule :included))
                        (getf rule :excluded) (mode-invocations (getf rule :excluded)))
                  (when (stringp (getf rule :test))
                    (setf (getf rule :test) `(match-url ,(getf rule :test))))
                  (apply #'make-instance 'auto-rule rule)))
            rules)))
