;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/renderer/cef)

(defun op-result (value)
  (json-object "ok" t "result" value))

(defun request-object (request key)
  (let ((value (nth-value 0 (json-field request key))))
    (when (hash-table-p value)
      value)))

(defun now-ms ()
  (round (* 1000 (time:timestamp-to-unix (time:now)))))

(defun extension-update-tab (tab-id properties)
  "Load PROPERTIES's URL in the tab TAB-ID and return the tab."
  (let ((buffer (find-extension-tab tab-id))
        (properties (or properties (json-object))))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (let ((url-text (json-string properties "url")))
      (when url-text
        (let ((uri (quri:uri url-text)))
          (setf (url buffer) uri)
          (ffi-buffer-load buffer uri))))
    (multiple-value-bind (active present-p) (json-field properties "active")
      (when (and present-p active)
        (set-current-buffer buffer)))
    (tab-description buffer)))

(defun extension-reload-tab (tab-id)
  (let ((buffer (find-extension-tab tab-id)))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (ffi-buffer-reload buffer)
    :null))

(defun extension-duplicate-tab (tab-id)
  (let ((buffer (find-extension-tab tab-id)))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (extension-create-tab (json-object "url" (tab-url buffer) "active" nil))))

(defun extension-go (tab-id direction)
  (extension-execute-script tab-id
                            (if (and (realp direction) (minusp direction))
                                "history.back()"
                                "history.forward()")
                            t)
  :null)

(defun extension-zoom (tab-id factor)
  (let ((buffer (find-extension-tab tab-id)))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (when (and (realp factor) (plusp factor))
      (setf (ffi-buffer-zoom-ratio buffer) (float factor 1.0d0)))
    (or (ffi-buffer-zoom-ratio buffer) 1.0)))

(defun extension-window (&optional populate)
  "Return the single window object extensions see. POPULATE includes its tabs."
  (let ((window (current-window)))
    (let ((description (json-object "id" 1
                                    "focused" t
                                    "incognito" nil
                                    "type" "normal"
                                    "state" (if (and window (nyxt::fullscreen-p window))
                                                "fullscreen"
                                                "normal")
                                    "alwaysOnTop" nil)))
      (when populate
        (setf (gethash "tabs" description)
              (coerce (mapcar #'tab-description (extension-tabs)) 'vector)))
      description)))

(defun extension-update-window (properties)
  (let ((state (json-string properties "state"))
        (window (current-window)))
    (when (and window state)
      (cond
        ((and (string= state "fullscreen") (not (nyxt::fullscreen-p window)))
         (ffi-window-fullscreen window :user-event-p nil))
        ((and (string= state "normal") (nyxt::fullscreen-p window))
         (ffi-window-unfullscreen window :user-event-p nil)))))
  (extension-window nil))

(defun extension-frames (tab-id)
  (let ((buffer (find-extension-tab tab-id)))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (vector (json-object "frameId" 0
                         "parentFrameId" -1
                         "url" (tab-url buffer)
                         "errorOccurred" nil))))

(-> extension-replace-menus (string t t) (eql :null))
(defun extension-replace-menus (extension-id revision items)
  "Replace EXTENSION-ID's context-menu rows with ITEMS.
REVISION orders overlapping updates. An older revision is ignored."
  (let ((incoming (if (realp revision) (round revision) 0)))
    (bt:with-lock-held (*extension-menu-lock*)
      (let ((seen (gethash extension-id *extension-menu-revision* 0)))
        (when (< incoming seen)
          (return-from extension-replace-menus :null))
        (setf (gethash extension-id *extension-menu-revision*) incoming)
        (setf *extension-menu-items*
              (remove-if (lambda (item)
                           (string= (extension-menu-item-extension-id item)
                                    extension-id))
                         *extension-menu-items*))
        (dolist (item (json-list items))
          (when (hash-table-p item)
            (let ((menu-id (json-identifier (gethash "id" item)))
                  (title (or (json-string item "title") "")))
              (when (plusp (length menu-id))
                (push (make-extension-menu-item :extension-id extension-id
                                                :menu-id menu-id
                                                :title (if (uiop:emptyp title)
                                                           menu-id
                                                           title))
                      *extension-menu-items*))))))))
  :null)

(defun extension-remove-menus (extension-id &optional menu-id)
  (bt:with-lock-held (*extension-menu-lock*)
    (setf *extension-menu-items*
          (remove-if (lambda (item)
                       (and (string= (extension-menu-item-extension-id item) extension-id)
                            (or (null menu-id)
                                (string= (extension-menu-item-menu-id item) menu-id))))
                     *extension-menu-items*)))
  :null)

(defun extension-pause-download (id paused)
  (let ((download-id (json-integer id)))
    (bt:with-lock-held (*extension-download-lock*)
      (let ((download (gethash download-id *extension-downloads*)))
        (when download
          (setf (extension-download-paused download) paused))))
    :null))

(defun history-entries ()
  (let ((vector (and *browser* (history-vector *browser*))))
    (if (and vector (plusp (length vector)))
        (coerce vector 'list)
        '())))

(defun history-item (entry index total)
  (json-object "id" (princ-to-string index)
               "url" (render-url (url entry))
               "title" (or (title entry) "")
               "lastVisitTime" (- (now-ms) (* 1000 (- total index 1)))
               "visitCount" 1
               "typedCount" 0))

(defun history-items ()
  (let* ((entries (history-entries))
         (total (length entries)))
    (loop for entry in entries
          for index from 0
          collect (history-item entry index total))))

(defun save-history (entries)
  (let ((vector (if entries
                    (make-array (length entries)
                                :fill-pointer t
                                :adjustable t
                                :initial-contents entries)
                    (make-array 0 :fill-pointer t :adjustable t))))
    (setf (history-vector *browser*) vector)
    (handler-case
        (files:with-file-content (history (history-file *browser*))
          (setf history vector))
      (error ()
        nil))
    vector))

(defun extension-search-history (query)
  (let* ((query (or query (json-object)))
         (text (or (json-string query "text") ""))
         (limit (or (json-integer (nth-value 0 (json-field query "maxResults"))) 100))
         (items (reverse (history-items)))
         (matched (remove-if-not
                   (lambda (item)
                     (or (zerop (length text))
                         (search text (or (json-string item "url") "") :test #'char-equal)
                         (search text (or (json-string item "title") "") :test #'char-equal)))
                   items)))
    (coerce (subseq matched 0 (min limit (length matched))) 'vector)))

(defun extension-add-history (details)
  (unless *browser*
    (error "History is not available."))
  (let ((url-text (json-string details "url")))
    (unless url-text
      (error "A history item needs a URL."))
    (let ((entry (make-instance 'history-entry
                                :url (quri:uri url-text)
                                :title (or (json-string details "title") ""))))
      (vector-push-extend entry (history-vector *browser*))
      (save-history (history-entries))
      (let ((item (history-item entry
                                (1- (length (history-vector *browser*)))
                                (length (history-vector *browser*)))))
        (broadcast-extension-event (extension-event "history.onVisited" item))
        item))))

(defun extension-delete-history-url (url-text)
  (unless url-text
    (error "A history deletion needs a URL."))
  (let ((uri (quri:uri url-text)))
    (save-history (remove-if (lambda (entry)
                               (url-equal (url entry) uri))
                             (history-entries))))
  :null)

(defun extension-bookmark-file ()
  (let ((buffer (or (current-buffer) (first (extension-tabs)))))
    (unless buffer
      (error "No buffer is available for bookmarks."))
    (nyxt/mode/bookmark:bookmarks-file buffer)))

(defun read-bookmarks ()
  (let ((bookmarks (handler-case
                       (files:content (extension-bookmark-file))
                     (error ()
                       nil))))
    (if (listp bookmarks) bookmarks '())))

(defun write-bookmarks (bookmarks)
  (files:with-file-content (stored (extension-bookmark-file))
    (setf stored bookmarks))
  bookmarks)

(defvar *extension-bookmark-ids* (make-hash-table :test 'equal))
(defvar *extension-bookmark-counter* 0)

(defun bookmark-id (url-string)
  (or (gethash url-string *extension-bookmark-ids*)
      (setf (gethash url-string *extension-bookmark-ids*)
            (princ-to-string (incf *extension-bookmark-counter*)))))

(defun bookmark-url (bookmark-id)
  (let ((found nil))
    (maphash (lambda (url id)
               (when (string= id bookmark-id)
                 (setf found url)))
             *extension-bookmark-ids*)
    found))

(defun bookmark-node (entry)
  (let ((url-string (render-url (url entry)))
        (stamp (handler-case
                   (time:timestamp-to-unix (nyxt/mode/bookmark:date entry))
                 (error ()
                   (time:timestamp-to-unix (time:now))))))
    (json-object "id" (bookmark-id url-string)
                 "parentId" "1"
                 "url" url-string
                 "title" (or (title entry) "")
                 "dateAdded" (round (* 1000 stamp)))))

(defun bookmark-nodes ()
  (mapcar #'bookmark-node (read-bookmarks)))

(defun bookmark-tree ()
  (vector (json-object "id" "0"
                       "title" ""
                       "children" (vector (json-object "id" "1"
                                                       "parentId" "0"
                                                       "title" "Nyxt"
                                                       "children" (coerce (bookmark-nodes) 'vector))))))

(defun extension-search-bookmarks (query)
  (let ((text (or (json-string query "query") "")))
    (coerce (remove-if-not
             (lambda (node)
               (or (zerop (length text))
                   (search text (or (json-string node "title") "") :test #'char-equal)
                   (search text (or (json-string node "url") "") :test #'char-equal)))
             (bookmark-nodes))
            'vector)))

(defun extension-create-bookmark (properties)
  (let ((url-text (json-string properties "url"))
        (title (or (json-string properties "title") "")))
    (unless url-text
      (error "A bookmark needs a URL."))
    (let* ((uri (quri:uri url-text))
           (bookmarks (read-bookmarks))
           (existing (find uri bookmarks :key #'url :test #'url-equal)))
      (cond
        (existing
         (setf (title existing) title))
        (t
         (push (make-instance 'nyxt/mode/bookmark:bookmark-entry
                              :url uri :title title :date (time:now))
               bookmarks)))
      (write-bookmarks bookmarks)
      (bookmark-node (or existing
                         (find uri bookmarks :key #'url :test #'url-equal))))))

(defun extension-remove-bookmark (bookmark-id)
  (let ((url-text (bookmark-url bookmark-id)))
    (unless url-text
      (error "No bookmark with id ~a." bookmark-id))
    (write-bookmarks (remove (quri:uri url-text) (read-bookmarks)
                             :key #'url :test #'url-equal))
    (remhash url-text *extension-bookmark-ids*)
    :null))

(defun extension-top-sites ()
  (let ((items (last (history-items) 10)))
    (coerce (mapcar (lambda (item)
                      (json-object "url" (or (json-string item "url") "")
                                   "title" (or (json-string item "title") "")))
                    (reverse items))
            'vector)))

(defvar *extension-closed-tabs* '())

(defun remember-closed-tab (buffer)
  (push (json-object "tab" (handler-case
                               (tab-description buffer)
                             (error ()
                               nil))
                     "lastModified" (now-ms))
        *extension-closed-tabs*)
  (when (> (length *extension-closed-tabs*) 25)
    (setf *extension-closed-tabs* (subseq *extension-closed-tabs* 0 25)))
  *extension-closed-tabs*)

(defmethod ffi-buffer-delete :after ((buffer cef-buffer))
  (when (extension-tab-p buffer)
    (remember-closed-tab buffer)
    (remhash (id buffer) *extension-known-tabs*)
    (when (eql *extension-active-tab-id* (id buffer))
      (setf *extension-active-tab-id* nil))
    (broadcast-extension-event
     (extension-event "tabs.onRemoved"
                      (id buffer)
                      (json-object "windowId" 1 "isWindowClosing" nil)))))

(defun extension-restore-session ()
  (let ((closed (pop *extension-closed-tabs*)))
    (unless (hash-table-p closed)
      (error "No closed tab to restore."))
    (let ((tab (request-object closed "tab")))
      (extension-create-tab (json-object "url" (or (and tab (json-string tab "url")) "about:blank")
                                         "active" t)))))

(defvar *extension-groups* (make-hash-table))
(defvar *extension-tab-group* (make-hash-table))
(defvar *extension-group-counter* 0)

(defun group-description (group-id)
  (gethash group-id *extension-groups*))

(defun extension-group-tabs (tab-ids properties)
  (let ((group-id (or (json-integer (nth-value 0 (json-field properties "groupId")))
                      (incf *extension-group-counter*))))
    (dolist (tab-id (json-list tab-ids))
      (let ((id (json-integer tab-id)))
        (when id
          (setf (gethash id *extension-tab-group*) group-id))))
    (setf (gethash group-id *extension-groups*)
          (json-object "id" group-id
                       "collapsed" (json-bool properties "collapsed")
                       "color" (or (json-string properties "color") "grey")
                       "title" (or (json-string properties "title") "")
                       "windowId" 1))
    group-id))

(defun extension-ungroup-tabs (tab-ids)
  (dolist (tab-id (json-list tab-ids))
    (remhash (json-integer tab-id) *extension-tab-group*))
  :null)

(defun extension-groups ()
  (let ((groups '()))
    (maphash (lambda (id description)
               (declare (ignore id))
               (push description groups))
             *extension-groups*)
    (coerce (nreverse groups) 'vector)))

(defun extension-update-group (group-id properties)
  (let ((description (group-description group-id)))
    (unless description
      (error "No tab group with id ~a." group-id))
    (when (json-string properties "title")
      (setf (gethash "title" description) (json-string properties "title")))
    (when (json-string properties "color")
      (setf (gethash "color" description) (json-string properties "color")))
    (multiple-value-bind (collapsed present-p) (json-field properties "collapsed")
      (when present-p
        (setf (gethash "collapsed" description) (and collapsed t))))
    description))

(defun extension-management-list ()
  (coerce (mapcar (lambda (extension)
                    (let ((id (extension-id extension)))
                      (json-object "id" id
                                   "name" (extension-name extension)
                                   "version" (version extension)
                                   "description" (description extension)
                                   "enabled" (extension-enabled-p extension)
                                   "installType" "development"
                                   "type" "extension"
                                   "mayDisable" t
                                   "offlineEnabled" nil)))
                  (listed-web-extensions))
          'vector))

(defvar *extension-proxy-config* nil)

(defun proxy-url-from-value (value)
  (let ((rules (request-object value "rules")))
    (when rules
      (let ((single (or (request-object rules "singleProxy")
                        (request-object rules "proxyForHttp")
                        (request-object rules "fallbackProxy"))))
        (when single
          (let ((host (json-string single "host"))
                (port (json-integer (nth-value 0 (json-field single "port"))))
                (scheme (or (json-string single "scheme") "http")))
            (when (and host port)
              (quri:uri (format nil "~a://~a:~d" scheme host port)))))))))

(defun extension-proxy-set (details)
  (let* ((value (or (request-object details "value") (json-object "mode" "system")))
         (mode (or (json-string value "mode") "system"))
         (proxy-url (unless (or (string= mode "direct") (string= mode "system"))
                      (proxy-url-from-value value)))
         (rules (request-object value "rules"))
         (bypass (if rules
                     (remove-if-not #'stringp
                                    (json-list (nth-value 0 (json-field rules "bypassList"))))
                     '())))
    (setf *extension-proxy-config* value)
    (dolist (buffer (extension-tabs))
      (handler-case
          (setf (ffi-buffer-proxy buffer)
                (if proxy-url
                    (list proxy-url bypass)
                    (list (quri:uri "") '())))
        (error ()
          nil))))
  :null)

(defun extension-search (query)
  (let ((text (or (json-string query "text") "")))
    (extension-create-tab
     (json-object "url" (uiop:strcat "https://duckduckgo.com/?q="
                                     (quri:url-encode text))
                  "active" t))))

(defstruct rule-set
  (by-host (make-hash-table :test 'equal))
  (generic '())
  (all '()))

(defvar *extension-rule-indexes* (make-hash-table :test 'equal))
(defvar *filter-scanners* (make-hash-table :test 'equal))
(defvar *regex-scanners* (make-hash-table :test 'equal))

(defun cached-scanner (table key factory)
  (multiple-value-bind (scanner present-p) (gethash key table)
    (if present-p
        scanner
        (setf (gethash key table)
              (handler-case
                  (funcall factory)
                (error ()
                  nil))))))

(defun filter-regex (filter)
  (let ((host-p (str:starts-with-p "||" filter))
        (end-p (and (plusp (length filter))
                    (char= (char filter (1- (length filter))) #\|)))
        (body filter))
    (when host-p
      (setf body (subseq body 2)))
    (when (and (not host-p) (str:starts-with-p "|" body))
      (setf body (subseq body 1)))
    (when (and end-p (plusp (length body)) (char= (char body (1- (length body))) #\|))
      (setf body (subseq body 0 (1- (length body)))))
    (with-output-to-string (out)
      (when host-p
        (write-string "^[a-z][a-z0-9+.-]*://(?:[^/?#]*\\.)?" out))
      (unless host-p
        (write-string "^" out))
      (loop for char across body
            do (cond
                 ((char= char #\*) (write-string ".*" out))
                 ((char= char #\^) (write-string "(?:[^A-Za-z0-9_.%-]|$)" out))
                 (t (write-string (cl-ppcre:quote-meta-chars (string char)) out))))
      (when end-p
        (write-string "$" out)))))

(defun filter-matches-p (filter url)
  (when (and (stringp filter) (stringp url))
    (let ((scanner (cached-scanner *filter-scanners* filter
                                   (lambda ()
                                     (cl-ppcre:create-scanner (filter-regex filter)
                                                              :case-insensitive-mode t)))))
      (and scanner (cl-ppcre:scan scanner url) t))))

(defun regex-matches-p (regex url)
  (when (and (stringp regex) (stringp url))
    (let ((scanner (cached-scanner *regex-scanners* regex
                                   (lambda ()
                                     (cl-ppcre:create-scanner regex :case-insensitive-mode t)))))
      (and scanner (cl-ppcre:scan scanner url) t))))

(defun filter-host-key (filter)
  (when (and (stringp filter) (str:starts-with-p "||" filter))
    (let* ((body (subseq filter 2))
           (end (position-if (lambda (char)
                               (member char '(#\^ #\/ #\* #\? #\|) :test #'char=))
                             body)))
      (let ((host (string-downcase (if end (subseq body 0 end) body))))
        (when (and (plusp (length host)) (find #\. host))
          host)))))

(defun host-suffixes (host)
  (when (and (stringp host) (find #\. host))
    (let ((parts (uiop:split-string (string-downcase host) :separator ".")))
      (loop for tail on parts
            while (rest tail)
            collect (str:join "." tail)))))

(defun index-rules (rules)
  (let ((set (make-rule-set)))
    (dolist (rule rules)
      (when (hash-table-p rule)
        (push rule (rule-set-all set))
        (let* ((condition (nth-value 0 (json-field rule "condition")))
               (filter (when (hash-table-p condition) (json-string condition "urlFilter")))
               (key (filter-host-key filter)))
          (if key
              (push rule (gethash key (rule-set-by-host set)))
              (push rule (rule-set-generic set))))))
    set))

(defun store-rules (extension-id kind rules)
  (setf (gethash (cons extension-id kind) *extension-rule-indexes*)
        (index-rules rules)))

(defun stored-rules (extension-id kind)
  (let ((set (gethash (cons extension-id kind) *extension-rule-indexes*)))
    (if set (rule-set-all set) '())))

(defun chrome-resource-type (type-name)
  (cond
    ((equal type-name "document") "main_frame")
    ((equal type-name "subdocument") "sub_frame")
    (t (or type-name "other"))))

(defun url-host (url-string)
  (when (stringp url-string)
    (handler-case
        (quri:uri-host (quri:uri url-string))
      (error ()
        nil))))

(defun domain-listed-p (host domains)
  (when (and host domains)
    (some (lambda (domain)
            (and (stringp domain)
                 (or (string-equal host domain)
                     (str:ends-with-p (uiop:strcat "." domain) host))))
          domains)))

(defun rule-priority (rule)
  (or (json-integer (nth-value 0 (json-field rule "priority"))) 1))

(defun rule-action-type (rule)
  (let ((action (nth-value 0 (json-field rule "action"))))
    (if (hash-table-p action)
        (or (json-string action "type") "")
        "")))

(defun rule-matches-p (rule url document-url type-name)
  (let ((condition (nth-value 0 (json-field rule "condition"))))
    (unless (hash-table-p condition)
      (return-from rule-matches-p nil))
    (let ((resource (chrome-resource-type type-name))
          (types (json-list (nth-value 0 (json-field condition "resourceTypes"))))
          (excluded (json-list (nth-value 0 (json-field condition "excludedResourceTypes"))))
          (domains (append (json-list (nth-value 0 (json-field condition "domains")))
                           (json-list (nth-value 0 (json-field condition "initiatorDomains")))))
          (excluded-domains (json-list (nth-value 0 (json-field condition "excludedDomains"))))
          (request-domains (json-list (nth-value 0 (json-field condition "requestDomains"))))
          (host (url-host document-url))
          (request-host (url-host url))
          (filter (json-string condition "urlFilter"))
          (regex (json-string condition "regexFilter")))
      (and (or (null types) (member resource types :test #'string=))
           (not (member resource excluded :test #'string=))
           (or (null filter) (filter-matches-p filter url))
           (or (null regex) (regex-matches-p regex url))
           (or (null domains) (domain-listed-p host domains))
           (or (null request-domains) (domain-listed-p request-host request-domains))
           (not (domain-listed-p host excluded-domains))))))

(defun candidate-rules (set url document-url type-name)
  (let ((found '())
        (hosts (append (host-suffixes (url-host url))
                       (host-suffixes (url-host document-url)))))
    (dolist (host hosts)
      (setf found (append (gethash host (rule-set-by-host set)) found)))
    (when (member type-name '("document" "subdocument" "script" "xmlhttprequest")
                  :test #'string=)
      (setf found (append (rule-set-generic set) found)))
    found))

(defun set-blocks-p (set url document-url type-name)
  (let ((winner nil)
        (winner-priority -1))
    (dolist (rule (candidate-rules set url document-url type-name))
      (when (rule-matches-p rule url document-url type-name)
        (let ((priority (rule-priority rule)))
          (when (or (> priority winner-priority)
                    (and (= priority winner-priority)
                         (string= (rule-action-type rule) "allow")))
            (setf winner rule
                  winner-priority priority)))))
    (and winner (string= (rule-action-type winner) "block") t)))

(defun dnr-blocks-p (url document-url type-name)
  (let ((blocked nil))
    (maphash (lambda (key set)
               (declare (ignore key))
               (unless blocked
                 (setf blocked (set-blocks-p set url document-url type-name))))
             *extension-rule-indexes*)
    blocked))

(defun load-static-rules (directory manifest)
  "Index enabled declarativeNetRequest rule resources from DIRECTORY."
  (let ((dnr (gethash "declarative_net_request" manifest))
        (id (extension-id-for manifest directory))
        (rules '()))
    (when (and (hash-table-p dnr) (plusp (length id)))
      (dolist (resource (json-list (gethash "rule_resources" dnr)))
        (when (hash-table-p resource)
          (multiple-value-bind (enabled present-p) (json-field resource "enabled")
            (when (or (not present-p) enabled)
              (let* ((path (json-string resource "path"))
                     (file (and path
                                (uiop:merge-pathnames*
                                 path
                                 (uiop:ensure-directory-pathname directory)))))
                (when (and file (uiop:file-exists-p file))
                  (handler-case
                      (dolist (rule (json-list (j:decode (uiop:read-file-string file))))
                        (when (hash-table-p rule)
                          (push rule rules)))
                    (error (condition)
                      (log:warn "Could not read declarative rules ~a: ~a."
                                file condition)))))))))
      (store-rules id "static" (nreverse rules)))))

(setf *extension-static-rules-hook* #'load-static-rules)

(defvar *extension-blocking-clients* (make-hash-table :test 'equal))
(defvar *extension-blocking-lock* (bt:make-lock "extension webRequest"))
(defvar *extension-request-votes* (make-hash-table :test 'equal))
(defvar *extension-request-vote-lock* (bt:make-lock "extension request votes"))
(defvar *extension-decision-counter* 0)

(defstruct request-vote
  (lock (bt:make-lock))
  (wait (bt:make-condition-variable))
  (needed 0 :type integer)
  (got 0 :type integer)
  (cancel nil))

(defun tab-id-for-url (url)
  (let ((buffer (find url (extension-tabs) :key #'tab-url :test #'string=)))
    (when buffer
      (id buffer))))

(defun blocking-extension-ids ()
  (let ((ids '()))
    (bt:with-lock-held (*extension-blocking-lock*)
      (maphash (lambda (id on)
                 (when on
                   (push id ids)))
               *extension-blocking-clients*))
    ids))

(defun web-request-blocks-p (url document-url type-name)
  (let ((ids (blocking-extension-ids)))
    (unless ids
      (return-from web-request-blocks-p nil))
    (let* ((vote (make-request-vote :needed (length ids)))
           (decision-id (princ-to-string (incf *extension-decision-counter*)))
           (details (json-object "requestId" decision-id
                                 "url" url
                                 "method" "GET"
                                 "frameId" 0
                                 "parentFrameId" -1
                                 "tabId" (or (tab-id-for-url document-url) -1)
                                 "type" (chrome-resource-type type-name)
                                 "timeStamp" (now-ms))))
      (bt:with-lock-held (*extension-request-vote-lock*)
        (setf (gethash decision-id *extension-request-votes*) vote))
      (dolist (id ids)
        (enqueue-event id (json-object "type" "webRequest.onBeforeRequest"
                                       "decisionId" decision-id
                                       "details" details)))
      (bt:with-lock-held ((request-vote-lock vote))
        (loop with deadline = (+ (get-internal-real-time)
                                 (round (* 0.75 internal-time-units-per-second)))
              until (>= (request-vote-got vote) (request-vote-needed vote))
              do (let ((left (/ (- deadline (get-internal-real-time))
                                internal-time-units-per-second)))
                   (when (or (minusp left)
                             (not (bt:condition-wait (request-vote-wait vote)
                                                     (request-vote-lock vote)
                                                     :timeout left)))
                     (return)))))
      (bt:with-lock-held (*extension-request-vote-lock*)
        (remhash decision-id *extension-request-votes*))
      (and (request-vote-cancel vote) t))))

(defun extension-network-blocked-p (url document-url type-name)
  (or (dnr-blocks-p url document-url type-name)
      (web-request-blocks-p url document-url type-name)))

(defvar *extension-decider-installed* nil)

(defun install-extension-request-decider ()
  "Run extension network rules after whatever request decider is already installed."
  (when *extension-decider-installed*
    (return-from install-extension-request-decider nil))
  (setf *extension-decider-installed* t)
  (let ((previous cef::*request-decider*))
    (setf cef::*request-decider*
          (lambda (url document-url type-name)
            (multiple-value-bind (blockp css replacements)
                (if previous
                    (funcall previous url document-url type-name)
                    (values nil "" ""))
              (if (or blockp (extension-network-blocked-p url document-url type-name))
                  (values t (or css "") (or replacements ""))
                  (values nil (or css "") (or replacements "")))))))
  t)

(defun merge-rules (extension-id kind options)
  (let* ((options (or options (json-object)))
         (remove-ids (remove nil
                             (mapcar #'json-integer
                                     (json-list (nth-value 0 (json-field options "removeRuleIds"))))))
         (current (remove-if (lambda (rule)
                               (member (json-integer (nth-value 0 (json-field rule "id")))
                                       remove-ids))
                             (copy-list (stored-rules extension-id kind))))
         (added (remove-if-not #'hash-table-p
                               (json-list (nth-value 0 (json-field options "addRules"))))))
    (store-rules extension-id kind (append current added))
    :null))

(defun record-blocking (extension-id on)
  (bt:with-lock-held (*extension-blocking-lock*)
    (setf (gethash extension-id *extension-blocking-clients*) on))
  :null)

(defun record-decision (request)
  (let ((vote (bt:with-lock-held (*extension-request-vote-lock*)
                (gethash (or (json-string request "decisionId") "")
                         *extension-request-votes*))))
    (when vote
      (bt:with-lock-held ((request-vote-lock vote))
        (when (json-bool request "cancel")
          (setf (request-vote-cancel vote) t))
        (incf (request-vote-got vote))
        (bt:condition-notify (request-vote-wait vote)))))
  :null)

(extension-op "tabs.update"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-update-tab (nth-value 0 (json-field request "tabId"))
                                                 (request-object request "properties")))))

(extension-op "tabs.reload"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-reload-tab (nth-value 0 (json-field request "tabId")))
                (op-result :null)))

(extension-op "tabs.duplicate"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-duplicate-tab (nth-value 0 (json-field request "tabId"))))))

(extension-op "tabs.go"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-go (nth-value 0 (json-field request "tabId"))
                              (json-integer (nth-value 0 (json-field request "direction"))))
                (op-result :null)))

(extension-op "tabs.zoom"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-zoom (nth-value 0 (json-field request "tabId"))
                                           (let ((factor (nth-value 0 (json-field request "factor"))))
                                             (when (realp factor) factor))))))

(extension-op "windows.get"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((id (json-integer (nth-value 0 (json-field request "windowId")))))
                  (unless (or (null id) (eql id 1) (eql id -2))
                    (error "No window with id: ~a." id))
                  (op-result (extension-window (json-bool request "populate"))))))

(extension-op "windows.getAll"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (vector (extension-window (json-bool request "populate"))))))

(extension-op "windows.create"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((properties (request-object request "properties")))
                  (when (or (json-string properties "url") properties)
                    (extension-create-tab (or properties (json-object)))))
                (op-result (extension-window t))))

(extension-op "windows.update"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-update-window (or (request-object request "properties")
                                                       (json-object))))))

(extension-op "windows.remove"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (op-result :null)))

(extension-op "webNavigation.getAllFrames"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-frames (nth-value 0 (json-field request "tabId"))))))

(extension-op "webNavigation.getFrame"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (aref (extension-frames (nth-value 0 (json-field request "tabId"))) 0))))

(extension-op "contextMenus.replace"
              (lambda (request extension-id)
                (extension-replace-menus extension-id
                                         (nth-value 0 (json-field request "revision"))
                                         (nth-value 0 (json-field request "items")))
                (op-result :null)))

(extension-op "contextMenus.remove"
              (lambda (request extension-id)
                (extension-remove-menus extension-id
                                        (json-identifier (nth-value 0 (json-field request "menuId"))))
                (op-result :null)))

(extension-op "contextMenus.removeAll"
              (lambda (request extension-id)
                (declare (ignore request))
                (extension-remove-menus extension-id)
                (op-result :null)))

(extension-op "contextMenus.update"
              (lambda (request extension-id)
                (let ((item (request-object request "item")))
                  (when item
                    (remember-menu-item extension-id
                                        (json-identifier (nth-value 0 (json-field request "menuId")))
                                        (or (json-string item "title") ""))))
                (op-result :null)))

(extension-op "downloads.pause"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-pause-download (nth-value 0 (json-field request "id")) t)
                (op-result :null)))

(extension-op "downloads.resume"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-pause-download (nth-value 0 (json-field request "id")) nil)
                (op-result :null)))

(extension-op "history.search"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-search-history (request-object request "query")))))

(extension-op "history.addUrl"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-add-history (or (request-object request "details")
                                                     (json-object))))))

(extension-op "history.deleteUrl"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-delete-history-url (json-string request "url"))
                (op-result :null)))

(extension-op "history.deleteRange"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (save-history '())
                (op-result :null)))

(extension-op "history.getVisits"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((url-text (json-string request "url")))
                  (op-result (coerce (loop for item in (history-items)
                                           when (string= url-text (json-string item "url"))
                                           collect (json-object "id" (json-string item "id")
                                                                "visitId" (json-string item "id")
                                                                "visitTime" (nth-value 0
                                                                                       (json-field item "lastVisitTime"))
                                                                "referringVisitId" "0"
                                                                "transition" "link"))
                                     'vector)))))

(extension-op "bookmarks.getTree"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (op-result (bookmark-tree))))

(extension-op "bookmarks.get"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((ids (json-list (nth-value 0 (json-field request "ids")))))
                  (op-result (coerce (if ids
                                         (remove-if-not (lambda (node)
                                                          (member (json-string node "id") ids
                                                                  :test #'string=))
                                                        (bookmark-nodes))
                                         (bookmark-nodes))
                                     'vector)))))

(extension-op "bookmarks.search"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-search-bookmarks (or (request-object request "query")
                                                          (json-object))))))

(extension-op "bookmarks.create"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-create-bookmark (or (request-object request "properties")
                                                         (json-object))))))

(extension-op "bookmarks.update"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let* ((id (json-identifier (nth-value 0 (json-field request "id"))))
                       (properties (or (request-object request "properties") (json-object)))
                       (url-text (bookmark-url id)))
                  (unless url-text
                    (error "No bookmark with id ~a." id))
                  (op-result (extension-create-bookmark
                              (json-object "url" (or (json-string properties "url") url-text)
                                           "title" (or (json-string properties "title") "")))))))

(extension-op "bookmarks.remove"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-remove-bookmark (json-identifier (nth-value 0 (json-field request "id"))))
                (op-result :null)))

(extension-op "topSites.get"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (op-result (extension-top-sites))))

(extension-op "sessions.getRecentlyClosed"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (op-result (coerce *extension-closed-tabs* 'vector))))

(extension-op "sessions.restore"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (op-result (extension-restore-session))))

(extension-op "tabGroups.query"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (op-result (extension-groups))))

(extension-op "tabGroups.get"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((description (group-description
                                    (json-integer (nth-value 0 (json-field request "groupId"))))))
                  (unless description
                    (error "No tab group with id ~a." (json-field request "groupId")))
                  (op-result description))))

(extension-op "tabGroups.update"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (op-result (extension-update-group
                            (json-integer (nth-value 0 (json-field request "groupId")))
                            (or (request-object request "properties") (json-object))))))

(extension-op "tabs.group"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((properties (or (request-object request "properties") (json-object))))
                  (op-result (extension-group-tabs (nth-value 0 (json-field properties "tabIds"))
                                                   properties)))))

(extension-op "tabs.ungroup"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-ungroup-tabs (nth-value 0 (json-field request "tabIds")))
                (op-result :null)))

(extension-op "browsingData.remove"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((data (request-object request "dataToRemove")))
                  (when (and data (json-bool data "history") *browser*)
                    (save-history '())))
                (op-result :null)))

(extension-op "management.getAll"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (op-result (extension-management-list))))

(extension-op "management.setEnabled"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((id (json-string request "id")))
                  (when id
                    (set-extension-enabled id (json-bool request "enabled"))))
                (op-result :null)))

(extension-op "proxy.get"
              (lambda (request extension-id)
                (declare (ignore request extension-id))
                (op-result (json-object "value" (or *extension-proxy-config*
                                                    (json-object "mode" "system"))
                                        "levelOfControl" "controllable_by_this_extension"))))

(extension-op "proxy.set"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-proxy-set request)
                (op-result :null)))

(extension-op "search.query"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (extension-search (or (request-object request "query") (json-object)))
                (op-result :null)))

(extension-op "dnr.update"
              (lambda (request extension-id)
                (merge-rules extension-id
                             (or (json-string request "kind") "dynamic")
                             (request-object request "options"))
                (op-result :null)))

(extension-op "dnr.get"
              (lambda (request extension-id)
                (op-result (coerce (stored-rules extension-id
                                                 (or (json-string request "kind") "dynamic"))
                                   'vector))))

(extension-op "dnr.regex"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (let ((regex (or (json-string request "regex") "")))
                  (handler-case
                      (progn
                        (cl-ppcre:create-scanner regex)
                        (op-result (json-object "isSupported" t)))
                    (error (condition)
                      (op-result (json-object "isSupported" nil
                                              "reason" (princ-to-string condition))))))))

(extension-op "webRequest.blocking"
              (lambda (request extension-id)
                (record-blocking extension-id (json-bool request "on"))
                (op-result :null)))

(extension-op "webRequest.decision"
              (lambda (request extension-id)
                (declare (ignore extension-id))
                (record-decision request)
                (op-result :null)))
