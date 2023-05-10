;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/blocker
  (:documentation "Block resource queries for listed hosts.
`blocker-mode' relies on:
- `hostlist' as the hostlist representation.
- `*default-hostlist*' as the most reliable hostlist.
- `load-hostlists' as the function forcing hostlist update and the user-space
  `update-hostlists' commands relying on it.

- `request-resource-block' as the hook handler that does all the automagic
  blocking."))
(in-package :nyxt/mode/blocker)

;; TODO: Add convenient interface to block hosts depending on the current URL.

(define-class hostlist (files:data-file nyxt-remote-file)
  ((hosts
    '()
    :type (or (cons string *) null)
    :documentation "List of hosts to ignore.
This is useful to reference hosts manually instead of via `nfiles:url'.")
   (files:update-interval
    #.(* 60 60 24)
    :type unsigned-byte
    :documentation "If URL is provided, update the list after this amount of
seconds."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A hostlist `blocker-mode' can use for its `hostlists' slot.
See `*default-hostlist*' for an example."))

(export-always 'make-hostlist)
(defun make-hostlist (&rest args)
  "Return a new `hostlist'.
See the `hostlist' class documentation."
  (apply #'make-instance 'hostlist args))


(export-always '*default-hostlist*)
(defparameter *default-hostlist*
  (make-instance 'hostlist
                 :url (quri:uri "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts")
                 :base-path #p"hostlist-stevenblack.txt")
  "Default hostlist for `blocker-mode'.")

(define-mode blocker-mode ()
  "Enable blocking of listed hosts.
To customize the list of blocked hosts, set the `hostlists' slot.
To force hostlist update, use `update-hostlists'.

Example:

\(defvar *my-blocked-hosts*
  (nyxt/mode/blocker:make-hostlist
   :hosts '(\"platform.twitter.com\"
            \"syndication.twitter.com\"
            \"m.media-amazon.com\")))

\(define-mode my-blocker-mode (nyxt/mode/blocker:blocker-mode)
  \"Blocker mode with custom hosts from `*my-blocked-hosts*'.\"
  ((nyxt/mode/blocker:hostlists (list *my-blocked-hosts* nyxt/mode/blocker:*default-hostlist*))))

\(define-configuration :buffer
  ((default-modes (append '(my-blocker-mode) %slot-default%))))"
  ((hostlists (list *default-hostlist*))
   (blocked-hosts
    (make-hash-table :test 'equal)
    :export nil
    :documentation "The set of host names to block.")))

(defmethod enable ((mode blocker-mode) &key)
  (when (network-buffer-p (buffer mode))
    (hooks:add-hook (request-resource-hook (buffer mode))
                    'request-resource-block)))

(defmethod disable ((mode blocker-mode) &key)
  (when (network-buffer-p (buffer mode))
    (hooks:remove-hook (request-resource-hook (buffer mode))
                       'request-resource-block)))

(defmethod files:write-file ((profile nyxt-profile) (hostlist hostlist) &key destination)
  "Write the downloaded hostlist to disk.
This is the raw downloaded content and not the serialized parsed content.
This gives more integrity guarantees to the user and allows external manipulation."
  (unless (uiop:emptyp (files:url-content hostlist))
    (alex:write-string-into-file (files:url-content hostlist) destination :if-exists :supersede)))

(defmethod files:deserialize ((profile nyxt-profile) (hostlist hostlist) raw-content &key)
  (flet ((empty-line? (line)
           (< (length line) 2))
         (comment? (line)
           (string= line "#" :end1 1))
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
  (dolist (hostlist (hostlists blocker-mode))
    ;; TODO: Allow running in the background, but warning, it could leak
    ;; personal information to undesired third-party.
    (dolist (host (or (hosts hostlist)
                      (let ((path (files:expand hostlist)))
                        (unless (uiop:file-exists-p path)
                          (echo "Updating hostlist ~s..." path))
                        (files:content hostlist
                                       :force-update force-update-p))))
      (setf (gethash host (blocked-hosts blocker-mode)) host))))

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
  (delete 'nyxt/mode/blocker::hostlists
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of object)))))

(define-command update-hostlists (&optional (blocker-mode (find-submode 'nyxt/mode/blocker:blocker-mode (current-buffer))))
  "Forces update for all the hostlists of `blocker-mode'."
  (load-hostlists blocker-mode :force-update-p t))
