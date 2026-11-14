;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/authenticator
  (:documentation "Package for `authenticator-mode', which generates one-time codes.
Secrets are stored in `authenticators-file' beside the other Nyxt data files.

The codes follow the Authenticator extension: Base32 secrets, HMAC-SHA1,
HMAC-SHA256, or HMAC-SHA512, and the TOTP, HOTP, Steam, and Battle.net
encodings.

See `authenticator-mode' for the user-facing commands."))
(in-package :nyxt/mode/authenticator)

(export-always 'authenticator-error)
(define-condition authenticator-error (error)
  ((message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "~a" (slot-value condition 'message))))
  (:documentation "Signaled when an authenticator secret or otpauth URI cannot be used."))

(defun authenticator-error (format-string &rest arguments)
  (error 'authenticator-error
         :message (apply #'format nil format-string arguments)))

(define-mode authenticator-mode ()
  "Generate one-time codes for saved authenticators.
Secrets are stored in `authenticators-file', beside the other Nyxt data files.
`add-authenticator' saves a secret. `copy-authenticator-code' copies the current
code, and `paste-authenticator-code' inserts it into the focused field."
  ((visible-in-status-p nil)
   (authenticators-file
    (make-instance 'authenticators-file)
    :type authenticators-file
    :documentation "File where authenticator secrets are saved.")))

(pushnew 'authenticator-mode nyxt::%default-modes)

(defmethod authenticators-file ((buffer buffer))
  (authenticators-file (find-submode 'authenticator-mode buffer)))

(define-class authenticators-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"authenticators")
   (files:name "authenticators"))
  (:export-class-name-p t)
  (:documentation "Saved authenticator secrets, beside the other Nyxt data files."))

(define-class authenticator-entry ()
  ((label
    ""
    :documentation "Name shown when choosing this authenticator.")
   (secret
    ""
    :documentation "Base32 secret used to generate codes.")
   (issuer
    ""
    :documentation "Service that issued the secret.")
   (account
    ""
    :documentation "Account name at the issuer.")
   (otp-type
    :totp
    :type (member :totp :hotp :steam :battle)
    :documentation "Code family: TOTP, HOTP, Steam, or Battle.net.")
   (algorithm
    :sha1
    :type (member :sha1 :sha256 :sha512)
    :documentation "HMAC digest: SHA1, SHA256, or SHA512.")
   (digits
    6
    :documentation "Digits in a decimal code. Steam codes ignore this.")
   (period
    30
    :documentation "Seconds a time-based code stays valid. HOTP ignores this.")
   (counter
    0
    :documentation "HOTP moving factor. Incremented when a code is used."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:documentation "One saved authenticator.
`secret' is Base32. Codes are generated the way the Authenticator extension
generates them."))

(-> normalize-secret (string) string)
(defun normalize-secret (secret)
  "Return SECRET without spaces, hyphens, or Base32 padding, in upper case."
  (string-upcase
   (remove-if (lambda (character)
                (or (char= character #\=)
                    (char= character #\-)
                    (char= character #\Space)
                    (char= character #\Tab)
                    (char= character #\Newline)
                    (char= character #\Return)))
              secret)))

(-> decode-base32 (string) (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun decode-base32 (secret)
  "Decode SECRET as Base32.
Spaces, hyphens, and padding are ignored. Signals `authenticator-error' when
SECRET is empty or contains a character outside the Base32 alphabet."
  (let* ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")
         (cleaned (normalize-secret secret))
         (value 0)
         (bits 0)
         (octets (make-array (floor (* (length cleaned) 5) 8)
                             :element-type '(unsigned-byte 8)
                             :fill-pointer 0)))
    (when (zerop (length cleaned))
      (authenticator-error "The authenticator secret is empty."))
    (loop for character across cleaned
          for index = (position character alphabet)
          do (unless index
               (authenticator-error
                "The authenticator secret contains ~c, which is not a Base32 character."
                character))
             (setf value (logior (ash value 5) index))
             (incf bits 5)
             (when (>= bits 8)
               (decf bits 8)
               (vector-push (ldb (byte 8 bits) value) octets)
               (setf value (logand value (1- (ash 1 bits))))))
    (make-array (length octets)
                :element-type '(unsigned-byte 8)
                :initial-contents octets)))

(-> hmac-octets ((simple-array (unsigned-byte 8) (*))
                 (simple-array (unsigned-byte 8) (*))
                 keyword)
    (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun hmac-octets (key message digest)
  "Return the HMAC of MESSAGE under KEY using Ironclad digest DIGEST."
  (let ((mac (ironclad:make-mac :hmac key digest)))
    (ironclad:update-mac mac message)
    (ironclad:produce-mac mac)))

(-> truncated-hmac (vector) integer)
(defun truncated-hmac (octets)
  "Return the 31-bit HOTP truncation of HMAC OCTETS."
  (let ((offset (logand (alexandria:last-elt octets) #x0f)))
    (logior (ash (logand (aref octets offset) #x7f) 24)
            (ash (aref octets (+ offset 1)) 16)
            (ash (aref octets (+ offset 2)) 8)
            (aref octets (+ offset 3)))))

(-> format-otp (integer integer) string)
(defun format-otp (value digits)
  "Return the last DIGITS of VALUE, padded with zeros."
  (unless (plusp digits)
    (authenticator-error "The authenticator digit count must be positive."))
  (format nil "~v,'0d" digits (mod value (expt 10 digits))))

(-> steam-code (integer) string)
(defun steam-code (value)
  "Return VALUE encoded as a five-character Steam Guard code."
  (let ((alphabet "23456789BCDFGHJKMNPQRTVWXY")
        (code value)
        (characters (make-string 5)))
    (loop for index from 0 below 5
          do (setf (char characters index)
                   (char alphabet (mod code (length alphabet))))
             (setf code (floor code (length alphabet))))
    characters))

(-> digest-name (keyword) keyword)
(defun digest-name (algorithm)
  "Return the Ironclad digest name for ALGORITHM.
Signals `authenticator-error' for an unsupported digest."
  (case algorithm
    (:sha1 :sha1)
    (:sha256 :sha256)
    (:sha512 :sha512)
    (t (authenticator-error "The authenticator algorithm ~a is not supported."
                            algorithm))))

(export-always 'one-time-code)
(-> one-time-code (authenticator-entry &key (:unix-time integer)) string)
(defun one-time-code (entry &key (unix-time (time:timestamp-to-unix (time:now))))
  "Return the current code for ENTRY.
UNIX-TIME is seconds since 1970 and supplies the moving factor for TOTP, Steam,
and Battle.net. HOTP uses `counter' and ignores UNIX-TIME. Signals
`authenticator-error' when the secret or its parameters cannot produce a code."
  (let* ((otp-type (otp-type entry))
         (moving (cond
                   ((eq otp-type :hotp)
                    (unless (and (integerp (counter entry)) (>= (counter entry) 0))
                      (authenticator-error
                       "The authenticator counter must be a non-negative integer."))
                    (counter entry))
                   ((not (plusp (period entry)))
                    (authenticator-error
                     "The authenticator period must be a positive number of seconds."))
                   (t (floor unix-time (period entry)))))
         (truncated (truncated-hmac
                     (hmac-octets (decode-base32 (secret entry))
                                  (ironclad:integer-to-octets moving :n-bits 64)
                                  (digest-name (algorithm entry))))))
    (cond
      ((eq otp-type :steam) (steam-code truncated))
      ((eq otp-type :battle) (format-otp truncated 8))
      (t (format-otp truncated (digits entry))))))

(-> authenticator-name (string string) string)
(defun authenticator-name (issuer account)
  "Return the name shown for ISSUER and ACCOUNT."
  (cond
    ((and (not (str:emptyp issuer)) (not (str:emptyp account)))
     (format nil "~a (~a)" issuer account))
    ((not (str:emptyp issuer)) issuer)
    ((not (str:emptyp account)) account)
    (t "Authenticator")))

(-> decoded-text (string) string)
(defun decoded-text (text)
  "Return TEXT with percent-escapes decoded."
  (if (str:emptyp text)
      ""
      (quri:url-decode text :lenient t)))

(-> query-parameter (list string) (maybe string))
(defun query-parameter (parameters name)
  "Return the value of NAME in PARAMETERS, or NIL when it is absent."
  (assoc-value parameters name :test #'string-equal))

(-> parse-whole-integer ((maybe string)) (maybe integer))
(defun parse-whole-integer (text)
  "Return TEXT as an integer when every character is part of it."
  (when (and text (not (str:emptyp text)))
    (multiple-value-bind (value end)
        (parse-integer text :junk-allowed t)
      (when (and value (= end (length text)))
        value))))

(-> otp-type-from-host (string) keyword)
(defun otp-type-from-host (host)
  "Return the code family named by an otpauth host.
Signals `authenticator-error' for an unsupported host."
  (cond
    ((string-equal host "totp") :totp)
    ((string-equal host "hotp") :hotp)
    ((string-equal host "steam") :steam)
    ((or (string-equal host "battle")
         (string-equal host "battlenet"))
     :battle)
    (t (authenticator-error "The authenticator type ~a is not supported." host))))

(-> algorithm-from-text ((maybe string)) keyword)
(defun algorithm-from-text (text)
  "Return the digest named by TEXT, or SHA1 when TEXT is empty.
Signals `authenticator-error' for an unsupported digest."
  (cond
    ((or (null text) (str:emptyp text) (string-equal text "SHA1")) :sha1)
    ((string-equal text "SHA256") :sha256)
    ((string-equal text "SHA512") :sha512)
    (t (authenticator-error "The authenticator algorithm ~a is not supported." text))))

(-> positive-integer ((maybe string) integer string) integer)
(defun positive-integer (text default description)
  "Return TEXT as a positive integer, or DEFAULT when TEXT is empty.
Signals `authenticator-error' when TEXT is present and not a positive integer.
DESCRIPTION names the setting in that message."
  (if (or (null text) (str:emptyp text))
      default
      (let ((value (parse-whole-integer text)))
        (if (and value (plusp value))
            value
            (authenticator-error "The authenticator ~a ~a is not a positive integer."
                                 description text)))))

(-> split-label (string) (values string string &optional))
(defun split-label (label)
  "Return the issuer and account stored in an otpauth label."
  (let ((colon (position #\: label)))
    (if colon
        (values (subseq label 0 colon)
                (string-left-trim " " (subseq label (1+ colon))))
        (values "" label))))

(-> otpauth-uri-p (string) boolean)
(defun otpauth-uri-p (text)
  "Return T when TEXT is an otpauth URI."
  (alexandria:starts-with-subseq "otpauth://" text :test #'char-equal))

(export-always 'authenticator-from-otpauth)
(-> authenticator-from-otpauth (string) (values authenticator-entry &optional))
(defun authenticator-from-otpauth (uri-string)
  "Build an `authenticator-entry' from an otpauth URI.
Signals `authenticator-error' when the URI, its type, or its secret is unusable."
  (let ((uri (handler-case (quri:uri uri-string)
               (error ()
                 (authenticator-error "The authenticator URI ~a is not an otpauth URI."
                                      uri-string)))))
    (unless (string-equal (or (quri:uri-scheme uri) "") "otpauth")
      (authenticator-error "The authenticator URI ~a is not an otpauth URI." uri-string))
    (let* ((parameters (quri:uri-query-params uri))
           (secret (query-parameter parameters "secret"))
           (otp-type (otp-type-from-host (or (quri:uri-host uri) "")))
           (digits (positive-integer (query-parameter parameters "digits")
                                     (if (eq otp-type :battle) 8 6)
                                     "digit count"))
           (counter (let ((text (query-parameter parameters "counter")))
                      (if (or (null text) (str:emptyp text))
                          0
                          (or (parse-whole-integer text)
                              (authenticator-error
                               "The authenticator counter ~a is not a non-negative integer."
                               text)))))
           (label (decoded-text (string-left-trim "/" (or (quri:uri-path uri) "")))))
      (unless (and secret (not (str:emptyp secret)))
        (authenticator-error "The authenticator URI has no secret."))
      (when (minusp counter)
        (authenticator-error
         "The authenticator counter ~a is not a non-negative integer." counter))
      (multiple-value-bind (path-issuer account) (split-label label)
        (let* ((query-issuer (query-parameter parameters "issuer"))
               (issuer (if (or (null query-issuer) (str:emptyp query-issuer))
                           path-issuer
                           query-issuer))
               (normalized (normalize-secret secret)))
          (decode-base32 normalized)
          (make-instance 'authenticator-entry
                         :label (authenticator-name issuer account)
                         :secret normalized
                         :issuer issuer
                         :account account
                         :otp-type otp-type
                         :algorithm (algorithm-from-text
                                     (query-parameter parameters "algorithm"))
                         :digits (if (eq otp-type :battle) 8 digits)
                         :period (positive-integer (query-parameter parameters "period")
                                                   30
                                                   "period")
                         :counter counter))))))

(export-always 'persist-authenticator)
(-> persist-authenticator (authenticator-entry &optional buffer) t)
(defun persist-authenticator (entry &optional (buffer (current-buffer)))
  "Store ENTRY in BUFFER's `authenticators-file'.
An entry with the same label, compared case-insensitively, is replaced."
  (files:with-file-content (authenticators (authenticators-file buffer))
    (setf authenticators
          (cons entry
                (remove (label entry)
                        (or authenticators '())
                        :key #'label
                        :test #'string-equal)))))

(-> delete-authenticators (list &optional buffer) t)
(defun delete-authenticators (entries &optional (buffer (current-buffer)))
  "Remove ENTRIES from BUFFER's `authenticators-file', matched by label."
  (files:with-file-content (authenticators (authenticators-file buffer))
    (setf authenticators
          (set-difference (or authenticators '())
                          entries
                          :key #'label
                          :test #'string-equal))))

(-> authenticator-entries (buffer) list)
(defun authenticator-entries (buffer)
  "Return the authenticators saved for BUFFER."
  (or (files:content (authenticators-file buffer)) '()))

(-> use-code (authenticator-entry) string)
(defun use-code (entry)
  "Return ENTRY's current code.
Advance and save `counter' when ENTRY is HOTP."
  (let ((code (one-time-code entry)))
    (when (eq (otp-type entry) :hotp)
      (incf (counter entry))
      (persist-authenticator entry))
    code))

(-> code-text (authenticator-entry) string)
(defun code-text (entry)
  "Return ENTRY's current code, or \"invalid\" when it cannot be generated."
  (handler-case (one-time-code entry)
    (authenticator-error () "invalid")))

(-> authenticator-detail (authenticator-entry) string)
(defun authenticator-detail (entry)
  "Return the HOTP counter, or the seconds left in a time-based code."
  (cond
    ((eq (otp-type entry) :hotp)
     (format nil "counter ~d" (counter entry)))
    ((not (plusp (period entry)))
     "")
    (t
     (format nil "~ds"
             (- (period entry)
                (mod (time:timestamp-to-unix (time:now))
                     (period entry)))))))

(defmethod prompter:object-attributes ((entry authenticator-entry)
                                       (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(label entry) (:width 3))
    ("Code" ,(code-text entry) (:width 1))
    ("Account" ,(account entry) (:width 2))
    ("Detail" ,(authenticator-detail entry) (:width 1))))

(defvar *authenticator-copy-action*
  (lambda-command copy-authenticator-code* (entries)
    "Copy the code to the clipboard."
    (when-let ((entry (first entries)))
      (echo "Copied authenticator code ~a." (copy-to-clipboard (use-code entry))))))

(defvar *authenticator-paste-action*
  (lambda-command paste-authenticator-code* (entries)
    "Paste the code into the focused field."
    (when-let ((entry (first entries)))
      (let ((code (use-code entry)))
        (ffi-buffer-paste (current-buffer) code)
        (echo "Pasted authenticator code ~a." code)))))

(defvar *authenticator-delete-action*
  (lambda-command delete-authenticators* (entries)
    "Delete the selected authenticators."
    (when entries
      (delete-authenticators entries)
      (echo "Deleted ~a authenticator~:p." (length entries)))))

(define-class authenticator-source (prompter:source)
  ((prompter:name "Authenticators")
   (prompter:constructor
    (or (files:content (authenticators-file (current-buffer))) '()))
   (prompter:actions-on-return
    (list *authenticator-copy-action* *authenticator-paste-action*)))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "Source listing saved authenticators and their current codes."))

(-> buffer-domain (buffer) string)
(defun buffer-domain (buffer)
  "Return the domain of BUFFER's URL, or an empty string when it has none."
  (let ((buffer-url (url buffer)))
    (or (and (quri:uri-p buffer-url)
             (quri:uri-domain buffer-url))
        "")))

(-> authenticator-from-secret (string buffer)
    (values authenticator-entry &optional))
(defun authenticator-from-secret (secret buffer)
  "Prompt for a name and return a TOTP entry for SECRET.
Signals `authenticator-error' when SECRET is not valid Base32."
  (let ((normalized (normalize-secret secret)))
    (decode-base32 normalized)
    (let ((name (prompt1 :prompt "Authenticator name"
                         :input (buffer-domain buffer)
                         :sources 'prompter:raw-source)))
      (make-instance 'authenticator-entry
                     :label (if (or (null name) (str:emptyp name))
                                "Authenticator"
                                name)
                     :secret normalized
                     :account (or name "")
                     :otp-type :totp
                     :algorithm :sha1
                     :digits 6
                     :period 30
                     :counter 0))))

(define-command add-authenticator (&optional (buffer (current-buffer)))
  "Save a Base32 secret or otpauth:// URI in `authenticators-file'."
  (with-current-buffer buffer
    (let ((input (prompt1 :prompt "Authenticator secret or otpauth:// URI"
                          :sources 'prompter:raw-source
                          :invisible-input-p t)))
      (when (and input (not (str:emptyp input)))
        (let ((entry (handler-case
                         (if (otpauth-uri-p input)
                             (authenticator-from-otpauth input)
                             (authenticator-from-secret input buffer))
                       (authenticator-error (condition)
                         (echo-warning "~a" condition)
                         nil))))
          (when entry
            (persist-authenticator entry buffer)
            (echo "Saved authenticator ~a." (label entry))))))))

(define-command copy-authenticator-code (&optional (buffer (current-buffer)))
  "Copy an authenticator code to the clipboard. M-return pastes it into the focused field."
  (with-current-buffer buffer
    (if (authenticator-entries buffer)
        (prompt :prompt "Copy authenticator code"
                :sources 'authenticator-source)
        (echo-warning "No authenticators saved."))))

(define-command paste-authenticator-code (&optional (buffer (current-buffer)))
  "Paste an authenticator code into the focused field. M-return copies it to the clipboard."
  (with-current-buffer buffer
    (if (authenticator-entries buffer)
        (prompt :prompt "Paste authenticator code"
                :sources (make-instance
                          'authenticator-source
                          :actions-on-return
                          (list *authenticator-paste-action*
                                *authenticator-copy-action*)))
        (echo-warning "No authenticators saved."))))

(define-command delete-authenticator (&optional (buffer (current-buffer)))
  "Delete saved authenticators from `authenticators-file'."
  (with-current-buffer buffer
    (if (authenticator-entries buffer)
        (prompt :prompt "Delete authenticator(s)"
                :sources (make-instance
                          'authenticator-source
                          :enable-marks-p t
                          :actions-on-return *authenticator-delete-action*))
        (echo-warning "No authenticators saved."))))
