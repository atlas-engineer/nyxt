(uiop:define-package :nyxt/auto-mode
  (:use :common-lisp :nyxt)
  (:import-from #:serapeum #:export-always)
  (:documentation "Mode for automatic URL-based mode toggling."))
(in-package :nyxt/auto-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria))

(export-always '*prompt-on-mode-toggle*)
(defvar *prompt-on-mode-toggle* nil
  "Whether user will be asked about adding the mode to included/excluded modes
in auto-mode-list on mode activation/deactivation.")

(export-always '*non-rememberable-modes*)
(defvar *non-rememberable-modes*
  ;; Base mode conflicts with its Nyxt symbol if it's not prefixed
  '(help-mode web-mode auto-mode nyxt::base-mode)
  "Modes that AUTO-MODE won't even try to save.")

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
   (modes :initarg :modes
          :accessor modes
          :type list-of-symbols
          :initform '())))

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
                  (or (auto-mode-list *browser*) (restore-auto-mode-list)))
                 #'priority :key #'test))))

(declaim (ftype (function (quri:uri buffer)) enable-matching-modes))
(defun enable-matching-modes (url buffer)
  (let ((rule (matching-auto-mode-rule url)))
    (enable-modes (rememberable-of (set-difference
                                    (modes rule)
                                    (mapcar #'mode-name (modes buffer))
                                    :test #'mode-equal))
                  buffer)
    (disable-modes (rememberable-of (set-difference
                                     (mapcar #'mode-name (modes buffer))
                                     (modes rule) :test #'mode-equal))
                   buffer)))

(defun clean-up-auto-mode (mode)
  (dolist (handler-name '(auto-mode-request-handler
                          enable-mode-prompting-handler
                          disable-mode-prompting-handler))
    (hooks:remove-hook (request-resource-hook (buffer mode)) handler-name)))

(defun store-last-active-modes (auto-mode)
  (setf (last-active-modes auto-mode)
        (modes (buffer auto-mode))))

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

(defun auto-mode-request-handler (request-data)
  (let* ((auto-mode (find-submode (buffer request-data) 'auto-mode))
         ;; We need to supress prompting when auto-mode modifies modes.
         (*prompt-on-mode-toggle* nil)
         (web-mode (find-submode (buffer request-data) 'web-mode))
         (previous-url
           (unless (eq (htree:root (history web-mode))
                       (htree:current (history web-mode)))
             (url (htree:data
                   (htree:parent (htree:current (history web-mode))))))))
    (if (matching-auto-mode-rule (url request-data))
        (progn
          (when (and previous-url (not (matching-auto-mode-rule previous-url)))
            (store-last-active-modes auto-mode))
          (enable-matching-modes (url request-data) (buffer request-data)))
        ;; Apply previous saved modes only on buffer-loads, not anytime else.
        ;;
        ;; TODO: Relies on somewhat illogical behavior of (url (current-buffer))
        ;; and (url request-data) being equal after buffer-load is called.
        ;; Replace with more stable and logical condition?
        (when (quri:uri= (url request-data) (url (buffer auto-mode)))
          (restore-last-active-modes auto-mode))))
  request-data)

(defun mode-covered-by-auto-mode-p (mode auto-mode-instance)
  (or (non-rememberable-mode-p mode)
      (let ((matching-rule (matching-auto-mode-rule
                            (url (buffer auto-mode-instance)))))
        (member mode (or (and matching-rule (modes matching-rule))
                         (last-active-modes auto-mode-instance))
                :test #'mode-equal))))

(declaim (ftype (function (boolean t) (function (root-mode)))
                make-mode-toggle-auto-mode-handler))
(defun make-mode-toggle-prompting-handler (enable-p auto-mode-instance)
  #'(lambda (mode)
      (when (and (not (mode-covered-by-auto-mode-p mode auto-mode-instance))
                 *prompt-on-mode-toggle*)
        (with-confirm ("Permanently ~a ~a for this URL?"
                       (if enable-p "enable" "disable")
                       (mode-name mode))
          (with-result (url (read-from-minibuffer
                             (make-minibuffer
                              :input-prompt "URL:"
                              :input-buffer (object-string (url (buffer mode)))
                              :must-match-p nil)))
            (let* ((test (make-dwim-match url))
                   (rule (find test (auto-mode-list *browser*)
                               :key #'test :test #'equal))
                   (rule-modes (if rule (modes rule) (modes (buffer mode))))
                   (modes (if enable-p
                              (union (list mode) rule-modes :test #'mode-equal)
                              (set-difference rule-modes (list mode)
                                              :test #'mode-equal))))
              (add-modes-to-auto-mode-list test modes)))))))

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
  (hooks:add-hook (request-resource-hook (buffer mode))
                  (make-handler-resource #'auto-mode-request-handler)))

(define-mode auto-mode ()
  "Remember the modes setup for given domain/host/URL and store it in an editable form.
These modes will then be activated on every visit to this domain/host/URL."
  ((last-active-modes :accessor last-active-modes
                      :type list
                      :initform '())
   (destructor :initform #'clean-up-auto-mode)
   (constructor :initform #'initialize-auto-mode)))

(declaim (ftype (function (string) list) make-dwim-match))
(defun make-dwim-match (url)
  (let ((url (or (ignore-errors (quri:uri url)) url)))
    (if (and (quri:uri-p url)
             (empty-path-url-p url)
             (host-only-url-p url))
        (if (string= (quri:uri-domain url)
                     (cl-ppcre:regex-replace "www[0-9]?\\." (quri:uri-host url) ""))
            `(match-domain ,(quri:uri-domain url))
            `(match-host ,(quri:uri-host url)))
        `(match-url ,(object-display url)))))

(define-command save-modes-to-auto-mode-list ()
  "Store the enabled modes to auto-mode-list for all the future visits of this
domain/host/URL/group of websites inferring the suitable matching condition by user input.
The rules are:
- If it's a plain domain-only URL (i.e. \"http://domain.com\") -- then use `match-domain'.
- If it's host with subdomains (\"http://whatever.subdomain.domain.com\") -- use `match-host'.
- Use `match-url' otherwise.

For the storage format see the comment in the head of your `auto-mode-list-data-path' file."
  (if (find-submode (current-buffer) 'auto-mode)
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
        (add-modes-to-auto-mode-list (make-dwim-match url)
                                     (modes (current-buffer))))
      (echo "Enable auto-mode first.")))

(declaim (ftype (function (list (or null list)) (values list &optional))
                add-modes-to-auto-mode-list))
(defun add-modes-to-auto-mode-list (test modes)
  (let ((rule (or (find test (auto-mode-list *browser*) :key #'test :test #'equal)
                  (make-instance 'auto-mode-rule :test test))))
    (when modes
      (setf (modes rule) (rememberable-of (mapcar #'maybe-mode-name modes))
            (auto-mode-list *browser*) (delete-duplicates
                                        (push rule (auto-mode-list *browser*))
                                        :key #'test :test #'equal))
      (store-auto-mode-list)
      (auto-mode-list *browser*))))

(defmethod serialize-object ((rule auto-mode-rule) stream)
  (let ((*standard-output* stream)
        (*print-case* :downcase))
    (write-string "(" stream)
    (if (and (eq (first (test rule)) 'match-url)
             (= 2 (length (test rule))))
        (format t "~s " (second (test rule)))
        (format t "~s " (test rule)))
    (format t ":modes ~a"
            (mapcar (alex:compose #'string-downcase #'symbol-name)
                    (modes rule)))
    (write-string ")" stream)))

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

(defun store-auto-mode-list ()
  (with-data-file (file (auto-mode-list-data-path *browser*)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (write-string ";; List of auto-mode rules.
;; It is made to be easily readable and editable, but you still need to remember some things:
;;
;; Every rule starts on a new line and consists of two parts:
;; - Condition for rule activation. It is either (match-domain ...),
;;   (match-host ...), (match-regex ...) or a string.
;; - :MODES -- List of the modes to enable on condition. Edit carefully:
;;   all the modes except these and nyxt/auto-mode:*non-rememberable-modes*
;;   will be disabled on matching URL.
;;
;; Conditions work this way:
;; - match-domain matches the URL domains only.
;;   Example: (match-domain \"reddit.com\") will work for all of Reddit.
;; - match-host is more specific -- it activates only on certain subdomains of the website.
;;   Example: (match-host \"old.reddit.com\") will work on old Reddit only.
;; - match-regex works for any address that matches a given regex. You can add these manually,
;;   but remember: with great regex comes great responsibility!
;;   Example: \"https://github.com/.*/.*\" will activate only in repos on GitHub.
;; - String format matches the exact URL and nothing else
;;   Example: \"https://lispcookbook.github.io/cl-cookbook/pattern_matching.html\"
;;            will work on the Pattern Matching article of CL Cookbook, and nowhere else.
;;
;; You can write additional URLs in the bracketed conditions, to reuse the rule for other URL
;; Example: (match-host \"reddit.com\" \"old.reddit.com\" \"www6.reddit.com\")
" file)
    (write-string "(" file)
    (dolist (rule (slot-value *browser* 'auto-mode-list))
      (write-char #\newline file)
      (serialize-object rule file))
    (format file "~%)~%")
    (echo "Saved ~a auto-mode rules to ~s."
          (length (slot-value *browser* 'auto-mode-list))
          (expand-path (auto-mode-list-data-path *browser*)))))

(defun restore-auto-mode-list ()
  (handler-case
      (let ((data (with-data-file (file (auto-mode-list-data-path *browser*)
                                        :direction :input
                                        :if-does-not-exist nil)
                    (when file (deserialize-auto-mode-rules file)))))
        (when data
          (echo "Loading ~a auto-mode rules from ~s."
                (length data) (expand-path (auto-mode-list-data-path *browser*)))
          (setf (slot-value *browser* 'auto-mode-list) data)))
    (error (c)
      (echo-warning "Failed to load auto-mode-list from ~s: ~a"
                    (expand-path (auto-mode-list-data-path *browser*)) c))))
