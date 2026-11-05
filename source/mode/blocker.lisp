;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/blocker
  (:documentation "Package for `blocker-mode', which blocks requests for hosts.
The mode stores the names in `hosts' and publishes them through
`ffi-buffer-blocked-hosts'. The renderer matches the request host against that
list."))
(in-package :nyxt/mode/blocker)

(define-mode blocker-mode ()
  "Block requests to the hosts in `hosts'.
The request host is matched exactly, ignoring case, so \"example.com\" does not
block \"ads.example.com\".

Example:

\(define-configuration nyxt/mode/blocker:blocker-mode
  ((nyxt/mode/blocker:hosts '(\"platform.twitter.com\"
                              \"syndication.twitter.com\"))))"
  ((hosts
    '()
    :type (list-of string)
    :documentation "Host names to block.
Each entry is one host, such as \"ads.example\". A port, a trailing dot, and
surrounding brackets are ignored.")))

(defmethod (setf hosts) :after (hosts (mode blocker-mode))
  (when (and (enabled-p mode) (buffer mode))
    (setf (nyxt:ffi-buffer-blocked-hosts (buffer mode)) hosts)))

(defmethod enable ((mode blocker-mode) &key)
  (setf (nyxt:ffi-buffer-blocked-hosts (buffer mode)) (hosts mode)))

(defmethod disable ((mode blocker-mode) &key)
  (setf (nyxt:ffi-buffer-blocked-hosts (buffer mode)) nil))

(-> normalize-blocked-host (t) (or string null))
(defun normalize-blocked-host (host)
  "Return HOST folded for an exact match, or NIL when it is not a host name.
The result is lowercase, without a port, brackets, or a trailing dot."
  (when (stringp host)
    (let ((name (string-trim '(#\Space #\Tab #\Newline #\Return) host)))
      (when (and (plusp (length name)) (char= (char name 0) #\[))
        (let ((close (position #\] name)))
          (setf name (if close (subseq name 1 close) (subseq name 1)))))
      (let ((colon (position #\: name)))
        (when (and colon (not (position #\: name :start (1+ colon))))
          (setf name (subseq name 0 colon))))
      (when (and (plusp (length name)) (char= (char name (1- (length name))) #\.))
        (setf name (subseq name 0 (1- (length name)))))
      (when (plusp (length name))
        (string-downcase name)))))

(-> normalize-blocked-hosts (t) list)
(defun normalize-blocked-hosts (hosts)
  "Return the distinct normalized names in HOSTS."
  (let ((normalized nil))
    (dolist (host hosts)
      (when-let ((name (normalize-blocked-host host)))
        (pushnew name normalized :test #'string=)))
    (nreverse normalized)))

(-> blocked-host-table (list) hash-table)
(export-always 'blocked-host-table)
(defun blocked-host-table (hosts)
  "Return an equal hash table of normalized HOSTS."
  (let ((table (make-hash-table :test 'equal :size (length hosts))))
    (dolist (host hosts table)
      (when-let ((name (normalize-blocked-host host)))
        (setf (gethash name table) t)))))

(-> url-host (string) (or string null))
(defun url-host (url)
  "Return the normalized host of an http or https URL, or NIL."
  (let ((scheme-end (cond
                      ((and (>= (length url) 8) (string= url "https://" :end1 8)) 8)
                      ((and (>= (length url) 7) (string= url "http://" :end1 7)) 7))))
    (when scheme-end
      (let* ((limit (or (position-if (lambda (char)
                                       (or (char= char #\/)
                                           (char= char #\?)
                                           (char= char #\#)))
                                     url :start scheme-end)
                        (length url)))
             (at (position #\@ url :start scheme-end :end limit :from-end t)))
        (when at
          (setf scheme-end (1+ at)))
        (when (< scheme-end limit)
          (normalize-blocked-host (subseq url scheme-end limit)))))))

(-> host-blocked-p ((or hash-table null) t) boolean)
(export-always 'host-blocked-p)
(defun host-blocked-p (table host)
  "Return T when HOST is a key of TABLE.
HOST is a host name, not a URL."
  (when table
    (when-let ((name (normalize-blocked-host host)))
      (and (gethash name table) t))))

(-> request-blocked-p ((or hash-table null) t) boolean)
(export-always 'request-blocked-p)
(defun request-blocked-p (table url)
  "Return T when URL's host is a key of TABLE."
  (when (and table (stringp url))
    (when-let ((host (url-host url)))
      (and (gethash host table) t))))

(defmethod (setf nyxt:ffi-buffer-blocked-hosts) :around (hosts (buffer buffer))
  (call-next-method (normalize-blocked-hosts hosts) buffer))
