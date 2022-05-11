;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/blocker-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum #:->)
  (:documentation "Block resource queries for listed hosts."))
(in-package :nyxt/blocker-mode)
(use-nyxt-package-nicknames)

;; TODO: Add convenient interface to block hosts depending on the current URL.

(define-class hostlist (files:data-file nyxt-file)
  ((url
    (quri:uri "")
    :type quri:uri
    :documentation "URL where to download the list from.  If empty, no attempt
will be made at updating it.")
   (url-body
    ""
    :type string)
   (hosts
    '()
    :type (or (cons string *) null)
    :documentation "List of hosts to ignore.
This is useful to reference hosts manually instead of via `url'.")
   (last-update
    0
    :type integer)
   (update-interval
    (* 60 60 24)
    :type integer
    :documentation "If URL is provided, update the list after this amount of
seconds.")
   (force-update-p
    nil
    :type boolean
    :export nil
    :documentation "When non-nil, reload the list from its URL, by-passing local files if any.
It's automatically reset by `load-hostlists'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A hostlist `blocker-mode' can use for its `hostlists' slot.
See `*default-hostlist*' for an example."))

(sera:export-always 'make-hostlist)
(defun make-hostlist (&rest args)
  "Return a new `hostlist'.
See the `hostlist' class documentation."
  (apply #'make-instance 'hostlist args))


(sera:export-always '*default-hostlist*)
(defparameter *default-hostlist*
  (make-instance 'hostlist
                 :url (quri:uri "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts")
                 :base-path #p"hostlist-stevenblack.txt")
  "Default hostlist for `blocker-mode'.")

(define-mode blocker-mode ()
  "Enable blocking of listed hosts.
To customize the list of blocked hosts, set the `hostlists' slot.
See the `hostlist' class documentation.

Example:

\(defvar *my-blocked-hosts*
  (nyxt/blocker-mode:make-hostlist
   :hosts '(\"platform.twitter.com\"
            \"syndication.twitter.com\"
            \"m.media-amazon.com\")))

\(define-mode my-blocker-mode (nyxt/blocker-mode:blocker-mode)
  \"Blocker mode with custom hosts from `*my-blocked-hosts*'.\"
  ((nyxt/blocker-mode:hostlists (list *my-blocked-hosts* nyxt/blocker-mode:*default-hostlist*))))

\(define-configuration buffer
  ((default-modes (append '(my-blocker-mode) %slot-default%))))"
  ((hostlists (list *default-hostlist*))
   (blocked-hosts
    (make-hash-table :test 'equal)
    :export nil
    :documentation "The set of host names to block.")))

(defmethod enable ((mode blocker-mode) &key)
  (when (web-buffer-p (buffer mode))
    (if (request-resource-hook (buffer mode))
        (hooks:add-hook (request-resource-hook (buffer mode))
                        'request-resource-block)
        (make-instance 'hook-resource
                       :combination #'combine-composed-hook-until-nil
                       :handlers '(request-resource-block)))))

(defmethod disable ((mode blocker-mode) &key)
  (when (web-buffer-p (buffer mode))
    (hooks:remove-hook (request-resource-hook (buffer mode))
                       'request-resource-block)))

(defmethod files:write-file ((profile nyxt-profile) (hostlist hostlist) &key &allow-other-keys)
  "Download the hostlist file if it has a URL."
  (when (url-body hostlist)
    (let ((path (files:expand hostlist)))
      (uiop:with-staging-pathname (destination path)
        (alex:write-string-into-file (url-body hostlist) destination :if-exists :supersede)))))

(defmethod files:read-file :around ((profile nyxt-profile) (hostlist hostlist) &key)
  ;; Must be an `:around' method so that the `files:file' specialization which
  ;; does file existence check is not called.
  "Fetch HOSTLIST file from its `url' and save it on disk.
If file is already on disk and younger then `update-interval', load it instead of fetching online."
  (let ((path (files:expand hostlist)))
    (cond
      ((and (not (url-empty-p (url hostlist)))
            (or (force-update-p hostlist)
                (uiop:emptyp (url-body hostlist))
                (< (update-interval hostlist)
                   (- (get-universal-time) (last-update hostlist))))
            (or (force-update-p hostlist)
                (not (uiop:file-exists-p path))
                (< (update-interval hostlist)
                   (- (get-universal-time) (uiop:safe-file-write-date path)))))
       (handler-case (let ((body (dex:get (url hostlist))))
                       (unless (uiop:emptyp body)
                         (setf (url-body hostlist) body)
                         (setf (last-update hostlist) (get-universal-time))
                         (echo "Restoring hostlist from ~a." (url hostlist))))
         (t (c)
           (log:info "Error during ~s hostlist download: ~a"
                     (render-url (url hostlist))
                     c))))
      ((uiop:file-exists-p path)
       (echo "Restoring hostlist from ~s." path)
       (setf (url-body hostlist)
             (alex:read-file-into-string path))
       (setf (last-update hostlist) (get-universal-time))))
    (unless (uiop:emptyp (url-body hostlist))
      (with-input-from-string (s (url-body hostlist))
        (files:deserialize profile hostlist s)))))

(defmethod files:deserialize ((profile nyxt-profile) (hostlist hostlist) raw-content &key)
  (flet ((empty-line? (line)
           (< (length line) 2))
         (comment? (line)
           (string= (subseq line 0 1) "#"))
         (custom-hosts? (line)
           (not (str:starts-with? "0.0.0.0" line))))
    (loop as line = (read-line raw-content nil)
          while line
          unless (or (empty-line? line)
                     (comment? line)
                     (custom-hosts? line))
            collect (second (str:split " " line)))))

(-> load-hostlists (blocker-mode &key (:force-update-p boolean)) t)
(defun load-hostlists (blocker-mode &key force-update-p)
  "Load BLOCKER-MODE's hostlists into `blocked-hosts' (in the background)."
  (clrhash (blocked-hosts blocker-mode))
  ;; TODO: Kill previous thread if running `load-hostlists' multiple times in a row.
  (run-thread "Blocker-mode list updater"
    (dolist (hostlist (hostlists blocker-mode))
      (unwind-protect
           (progn
             (setf (force-update-p hostlist) force-update-p)
             (dolist (host (or (hosts hostlist)
                               (files:content hostlist
                                               :force-read force-update-p)))
               (setf (gethash host (blocked-hosts blocker-mode)) host)))
        (setf (force-update-p hostlist) nil))
      ;;  We write the file here because we are not setting the content
      ;;  anytime after this point.
      (unless (hosts hostlist)
        (files:write-file (files:profile hostlist) hostlist)))))

(defmethod blocklisted-host-p ((mode blocker-mode) host)
  "Return non-nil of HOST if found in the hostlists of MODE.
Return nil if MODE's hostlist cannot be parsed."
  (gethash host (blocked-hosts mode)))

(defun request-resource-block (request-data)
  "Block resource queries from blocklisted hosts.
This is an acceptable handler for `request-resource-hook'."
  (let ((mode (find-submode 'blocker-mode (buffer request-data))))
    (if (and mode
             (blocklisted-host-p
              mode
              (quri:uri-host (url request-data))))
        (progn
          (log:debug "Dropping ~a for ~a (~a)"
                     (render-url (url request-data))
                     (buffer request-data)
                     (render-url (url (buffer request-data))))
          nil)
        ;; Pass request to the other handlers.
        request-data)))

(defmethod s-serialization:serializable-slots ((object blocker-mode))
  "Discard hostlists which can get pretty big."
  (delete 'nyxt/blocker-mode::hostlists
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of object)))))

(define-command update-hostlists (&optional (blocker-mode (find-submode 'nyxt/blocker-mode:blocker-mode (current-buffer))))
  "Forces update for all the hostlists of `blocker-mode'."
  (load-hostlists blocker-mode :force-update-p t)
  (echo "Hostlists updating..."))
