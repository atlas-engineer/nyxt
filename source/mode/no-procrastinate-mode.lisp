;; IT WAS NEEDED TO USE (in-package :nyxt), OTHERWISE, IT WOULD NOT WORK
;; (uiop:define-package :nyxt/no-procrastinate-mode
;;     (:use :common-lisp :nyxt :nyxt/blocker-mode)
;;   (:import-from #:class-star #:define-class)
;;   (:documentation "Block resource queries for listed hosts."))
;; (in-package :nyxt/no-procrastinate-mode)
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (use-nyxt-package-nicknames))

(in-package :nyxt)

;; (defun procrastination-host-add (host) 
;;   (with-data-access (x (no-procrastination-path (current-buffer)))
;;     (push host x)))

(define-class no-procrastinate-entry ()
  ((url (quri:uri ""))
   (title "")
   (annotation "")
   (date (local-time:now))
   (tags
    '()
    :type list-of-strings))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(-> no-procrastinate-add
    (quri:uri &key (:title string) (:date (or local-time:timestamp null)) (:tags t))
    t)
(export-always 'no-procrastinate-add)
(defun no-procrastinate-add  (url &key date title tags)
  (with-data-access (no-procrastination-list (no-procrastination-path (current-buffer)))
    (unless (or (url-empty-p url)
                (string= "about:blank" (render-url url)))
      (multiple-value-bind (entries bookmarks-without-url)
          (sera:partition (sera:partial #'url-equal url) no-procrastination-list :key #'url)
        (let ((entry (if entries
                         (first entries)
                         (make-instance 'no-procrastinate-entry
                                        :url url))))
          (unless (str:emptyp title)
            (setf (title entry) title))
          (setf tags (delete "" tags :test #'string=))
          (setf tags (delete-duplicates tags :test #'string=))
          (setf (tags entry) (sort tags #'string<))
          (when date
            (setf (date entry) date))
          (push entry bookmarks-without-url)
          (setf no-procrastination-list bookmarks-without-url))))))

;; (define-command no-procrastinate-current-url (&optional (buffer (current-buffer)))
;;   "Bookmark the URL of BUFFER."
;;   (if (url-empty-p (url buffer))
;;       (echo "Buffer has no URL.")
;;       (progn (procrastination-host-add (quri:uri-host (url buffer)))
;;              (echo "Associated as a procrastination host ~a." (render-url
;;                                                                (url buffer))))))
;; (defun list-procrastinating-hosts ()
;;   "Lists all hosts' associated to procrastination saved in the local file."
;;   (with-data-access (no-procrastination-hosts (no-procrastination-path (current-buffer)))
;;     no-procrastination-hosts))

;; NEED TO COMPILE AFTER NYXT STARTED, DO NOT KNOW WHY.
;; (defvar *my-blocked-hosts*
;;   (nyxt/blocker-mode:make-hostlist
;;    :hosts (list-procrastinating-hosts)))

;; NEED TO COMPILE AFTER NYXT STARTED, DO NOT KNOW WHY.
;; (define-mode no-procrastinate-mode (nyxt/blocker-mode:blocker-mode)
;;   "Blocker mode with custom hosts from `*my-blocked-hosts*'."
;;   ((nyxt/blocker-mode:hostlists
;;     (list *my-blocked-hosts* nyxt/blocker-mode:*default-hostlist*))))

;; NEED TO COMPILE AFTER NYXT STARTED, DO NOT KNOW WHY.
;; (define-configuration buffer
;;   ((default-modes (append '(no-procrastinate-mode) %slot-default%))))

;; (defmethod store ((profile data-profile) (path no-procrastination-data-path) &key &allow-other-keys)
;;   "Store the hosts data associated to procrastination to the buffer
;; `no-procrastination-path'."
;;   (with-data-file (file path :direction :output)
;;     (%set-data path (get-data path))
;;     (s-serialization:serialize-sexp (get-data path) file))
;;   t)

;; (defmethod restore ((profile data-profile) (path no-procrastination-data-path) &key &allow-other-keys)
;;   "Restore the hosts' data associated to procrastination from the buffer
;; `inputs-path'."
;;   (handler-case
;;       (let ((data (with-data-file (file path)
;;                     (when file
;;                       (s-serialization:deserialize-sexp file)))))
;;         (when data
;;           (%set-data path data)))
;;     (error (c)
;;       (echo-warning "Failed to load inputs from ~s: ~a" (expand-path path) c))))
(defmethod serialize-object ((entry no-procrastinate-entry) stream)
  (unless (url-empty-p (url entry))
    (flet ((write-slot (slot)
             (let ((entry-slot (funcall slot entry)))
               (unless (str:emptyp entry-slot)
                 (format t " :~a ~s"
                         (str:downcase slot)
                         entry-slot)))))
      (let ((*standard-output* stream))
        (write-string "(:url ")
        (format t "~s" (render-url (url entry)))
        (write-slot 'title)
        (write-slot 'annotation)
        (when (date entry)
          (write-string " :date ")
          ;; If we don't force the timezone, the timestamp could be serialized
          ;; differently depending on the local timezone, e.g.
          ;;     2020-12-10T11:46:02.500515+01:00
          ;; instead of
          ;;     2020-12-10T10:46:02.500515Z
          (format t "~s" (local-time:format-timestring nil (date entry)
                                                       :timezone local-time:+utc-zone+)))
        (when (tags entry)
          (write-string " :tags (")
          (format t "~s" (first (tags entry)))
          (dolist (tag (rest (tags entry)))
            (write-string " ")
            (write tag))
          (write-string ")"))
        (write-string ")")))))

;; CHANGE THE NAME
(defmethod deserialize-no-procrastinate-hosts (stream)
  (handler-case
      (let ((*standard-input* stream))
        (let ((entries (read stream)))
          (mapcar (lambda (entry)
                    (when (getf entry :url)
                      (setf (getf entry :url)
                            (quri:uri (getf entry :url))))
                    (when (getf entry :date)
                      (setf (getf entry :date)
                            (local-time:parse-timestring (getf entry :date))))
                    (apply #'make-instance 'no-procrastinate-entry
                           entry))
                  entries)))
    (error (c)
      (log:error "During no-procrastinate-host deserialization: ~a" c)
      nil)))

(defmethod store ((profile data-profile) (path no-procrastination-data-path) &key &allow-other-keys)
  "Store the no-procrastinate-host to the buffer `no-procrastination-data-path'."
  (with-data-file (file path :direction :output)
    (%set-data path
              (sort (get-data path)
                    #'url< :key #'url))
    (write-string "(" file)
    (dolist (entry (get-data path))
      (write-char #\newline file)
      (serialize-object entry file))
    (format file "~%)~%")
    (echo "Saved ~a host to ~s."
          (length (get-data path))
          (expand-path path)))
  t)

(defmethod restore ((profile data-profile) (path no-procrastination-data-path) &key &allow-other-keys)
  "Restore the no-procrastinate host from the buffer `no-procrastination-data-path'."
  (handler-case
      (let ((data (with-data-file (file path)
                    (when file
                      (deserialize-no-procrastinate-hosts file)))))
        (when data
          (echo "Loading ~a no-procrastinate host from ~s." (length data) (expand-path path))
          (%set-data path data)))
    (error (c)
      (echo-warning "Failed to load no-procrastinate host from ~s: ~a"
                    (expand-path path) c))))

