(uiop:define-package :nyxt/no-procrastinate-mode
    (:use :common-lisp :nyxt :nyxt/blocker-mode)
  (:import-from #:class-star #:define-class)
  (:documentation "Block resource queries for listed hosts."))
(in-package :nyxt/no-procrastinate-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-nyxt-package-nicknames))

;; IT WAS NEEDED TO USE (in-package :nyxt), OTHERWISE, IT WOULD NOT WORK
(in-package :nyxt)

(define-mode no-procrastinate-mode ()
  "Mode to block access to hosts associated to procrastination."
  ((constructor
    (lambda (mode)
      (nyxt:on-signal-load-finished mode (url (current-buffer)))))))

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

(define-internal-page-command list-no-procrastinate-hosts ()
    (no-procrastinate-list-buffer "*Hostnames to avoid procrastination*" 'base-mode)
  "List all hosts associated to no-procrastinate in a new buffer."
  (flet ((html-no-procrastinate-item-id (id)
           (format nil "no-procrastinate-host-~d" id)))
    (spinneret:with-html-string
      (:style (style no-procrastinate-list-buffer))
      (:h1 "No procrastination on the following hostnames")
      (:body
       (or (with-data-unsafe (no-procrastinate-list (no-procrastination-path (current-buffer)))
             (loop for no-procrastinate-item in no-procrastinate-list
                   for id from 0 by 1
                   collect
                   (let ((url-display (quri:uri-host (url no-procrastinate-item)))
                         (url-href (render-url (url no-procrastinate-item))))
                     (:div
                      :id (html-no-procrastinate-item-id id)
                      (:p (:b "Title: ") (title no-procrastinate-item))
                      (:p (:b "Hostname: ") (:a :href url-href
                                                 url-display))
                      (:p (:b "Tags: ")
                          (when (tags no-procrastinate-item)
                            (format nil " (~{~a~^, ~})" (tags no-procrastinate-item))))
                      (:p (:button :class "button"
                                   :onclick
                                   (ps:ps
                                    (let ((element
                                           (ps:chain
                                            document
                                            (get-element-by-id (ps:lisp (html-no-procrastinate-item-id id))))))
                                      (ps:chain element parent-node (remove-child element))
                                      (nyxt/ps:send-lisp-url
                                       `(nyxt::delete-no-procrastinate-host ,url-href))))
                                   "Delete"))
                      (:hr "")))))
           (format nil "No hostnames associated to no-procrastinate in ~s."
                   (expand-path (no-procrastination-path (current-buffer)))))))))

(defun url-no-procrastinate-host-tags (url)
  "Return the list of tags of the host corresponding to URL."
  (with-data-unsafe (no-procrastinate-hosts (no-procrastination-path (current-buffer)))
    (alex:when-let ((existing (find url no-procrastinate-hosts :key #'url :test #'url-equal)))
      (tags existing))))

(define-command no-procrastinate-current-host (&optional (buffer (current-buffer)))
  "Block procrastination on current hostname of BUFFER's URL."
  (if (url-empty-p (url buffer))
      (echo "Buffer has no URL.")
      (let ((tags (prompt
                   :prompt "Tag(s)"
                   :sources (list
                             (make-instance 'prompter:word-source
                                            :name "New tags"
                                            ;; On no input, suggest the empty tag which effectively acts as "no tag".
                                            ;; Without it, we would be force to specify a tag.
                                            :filter-postprocessor
                                            (lambda (suggestions source input)
                                              (declare (ignore source input))
                                              (or suggestions
                                                  (list "")))
                                            :multi-selection-p t)
                             (make-instance 'keyword-source
                                            :buffer buffer)
                             (make-instance 'tag-source
                                            :marks (url-no-procrastinate-host-tags (url buffer)))))))
        (no-procrastinate-add (url buffer)
                      :title (title buffer)
                      :tags tags)
        (echo "Associated as a host to avoid procrastination ~a." (render-url (url buffer))))))

(define-class no-procrastinate-source (prompter:source)
  ((prompter:name "Hosts to avoid procrastination")
   (prompter:constructor (get-data (no-procrastination-path (current-buffer))))
   (prompter:multi-selection-p t)
   (prompter:active-attributes-keys '("URL" "Title" "Tags"))))

(define-command delete-no-procrastinate-host (&optional urls-or-bookmark-entries)
  "Delete bookmark(s) matching URLS-OR-BOOKMARK-ENTRIES.
URLS is either a list or a single element."
  (if urls-or-bookmark-entries
      (with-data-access (bookmarks (no-procrastination-path (current-buffer)))
        (setf bookmarks
              (set-difference
               bookmarks
               (mapcar (lambda (url)
                         (if (bookmark-entry-p url)
                             url
                             (make-instance 'no-procrastinate-entry :url (quri:uri url))))
                       (uiop:ensure-list urls-or-bookmark-entries))
               :test #'equals)))
      (let ((entries (prompt
                      :prompt "Delete bookmark(s)"
                      ;; :default-modes '(minibuffer-tag-mode minibuffer-mode)
                      :sources (make-instance 'no-procrastinate-source
                                              :multi-selection-p t))))
        (delete-no-procrastinate-host entries))))

(define-command no-procrastinate-host (&key url)
  "Allow the user to avoid procrastination on a HOST via minibuffer input."
  (let ((url (or url
                 (ignore-errors
                  (quri:uri
                   (first
                    (prompt
                     :prompt "Avoid procrastination on HOST"
                     :sources (list
                               (make-instance 'prompter:raw-source
                                              :name "New URL")))))))))
    (if (not (valid-url-p url))
        (echo "Invalid URL '~a'" url)
        (let* ((url (quri:uri url))
               (tags (prompt
                      :prompt "Tag(s)"
                      :sources (list
                                (make-instance 'prompter:word-source
                                               :name "New tags"
                                               :multi-selection-p t)
                                (make-instance 'tag-source
                                               :marks (url-no-procrastinate-host-tags url))))))
          (no-procrastinate-add url :tags tags)))))

(define-command no-procrastinate-buffer-host ()
  "Add to the list of hostnames to avoid procrastination the currently opened
page(s) in the active buffer."
  (prompt
   :prompt "Avoid procrastination on HOSTS from buffer(s)"
   :sources (make-instance 'user-buffer-source
                           :multi-selection-p t
                           :actions (list (make-unmapped-command no-procrastinate-current-host)))))

;; I already have the no-procrastinate-current-host
;; (define-command no-procrastinate-current-url (&optional (buffer (current-buffer)))
;;   "Add to the list to avoid procrastination the BUFFER's HOST."
;;   (if (url-empty-p (url buffer))
;;       (echo "Buffer has no URL.")
;;       (progn (procrastination-host-add (quri:uri-host (url buffer)))
;;              (echo "Associated as a procrastination host ~a." (render-url
;;                                                                (url buffer))))))

(defun list-procrastinating-hosts ()
  "Lists all hosts' associated to procrastination saved in the local file."
  (with-data-access (no-procrastination-hosts (no-procrastination-path (current-buffer)))
    no-procrastination-hosts))

;;See the documentation of the BLOCKER-MODE and customizations
;;NEED TO COMPILE AFTER NYXT STARTED, DO NOT KNOW WHY. Probably, because of the Constructor
(defvar *my-blocked-hosts*
  (nyxt/blocker-mode:make-hostlist
   :hosts (mapcar #'(lambda (no-procrastinate-entry)
                      (quri:uri-host (url no-procrastinate-entry)))
                  (list-procrastinating-hosts))))

;; See the documentation of the BLOCKER-MODE and customizations
;; NEED TO COMPILE AFTER NYXT STARTED, DO NOT KNOW WHY. Probably, because of the Constructor
(define-mode no-procrastinate-mode (nyxt/blocker-mode:blocker-mode)
  "Blocker mode with custom hosts from `*my-blocked-hosts*'."
  ((nyxt/blocker-mode:hostlists
    (list *my-blocked-hosts* nyxt/blocker-mode:*default-hostlist*))))

;; See the documentation of the BLOCKER-MODE and customizations
;; NEED TO COMPILE AFTER NYXT STARTED, DO NOT KNOW WHY. Probably, because of the Constructor
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

(defmethod nyxt:on-signal-load-finished ((mode no-procrastinate-mode) url)
  ;call-function-to-create-instance-of-blocker-mode
  (echo "test"))
