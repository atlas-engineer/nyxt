;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/renderer/cef)

(defvar *extension-bridge-port* nil)
(defvar *extension-bridge-token* "")
(defvar *extension-bridge-started* nil)
(defvar *extension-clients* (make-hash-table :test 'equal))
(defvar *extension-clients-lock* (bt:make-lock "extension clients"))
(defvar *extension-menu-items* '())
(defvar *extension-menu-lock* (bt:make-lock "extension menus"))
(defvar *extension-menu-revision* (make-hash-table :test 'equal)
  "Newest context-menu revision applied for each extension id.")
(defvar *extension-downloads* (make-hash-table))
(defvar *extension-download-counter* 0)
(defvar *extension-download-lock* (bt:make-lock "extension downloads"))

(defstruct extension-client
  (events '())
  (lock (bt:make-lock))
  (wait (bt:make-condition-variable)))

(defstruct extension-menu-item
  (extension-id "")
  (menu-id "")
  (title ""))

(defstruct extension-download
  (id 0)
  (url "")
  (filename "")
  (state "complete")
  (paused nil))

(-> json-object (&rest t) hash-table)
(defun json-object (&rest pairs)
  "Return a string-keyed table of PAIRS, which are alternating keys and values."
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key table) value))
    table))

(-> json-field ((or null hash-table) string) (values t boolean))
(defun json-field (object key)
  "Return the value of KEY and whether KEY is present.
:NULL from JSON is treated as absent."
  (if (hash-table-p object)
      (multiple-value-bind (value present-p) (gethash key object)
        (if (eq value :null)
            (values nil nil)
            (values value present-p)))
      (values nil nil)))

(-> json-string ((or null hash-table) string &optional (or null string)) (or null string))
(defun json-string (object key &optional default)
  (multiple-value-bind (value present-p) (json-field object key)
    (if (and present-p (stringp value))
        value
        default)))

(-> json-bool ((or null hash-table) string &optional boolean) boolean)
(defun json-bool (object key &optional (default nil))
  (multiple-value-bind (value present-p) (json-field object key)
    (if present-p
        (and value t)
        default)))

(-> json-integer (t) (or null integer))
(defun json-integer (value)
  (typecase value
    (integer value)
    (real (round value))
    (t nil)))

(-> json-identifier (t) string)
(defun json-identifier (value)
  "Return VALUE as the string an extension uses for a menu or request id."
  (typecase value
    (string value)
    (integer (princ-to-string value))
    (real (princ-to-string (round value)))
    (t "")))

(-> json-list (t) list)
(defun json-list (value)
  (cond
    ((null value) '())
    ((stringp value) (list value))
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t '())))

(-> js-string (string) string)
(defun js-string (text)
  "Return TEXT as a double-quoted JavaScript string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across text
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\newline (write-string "\\n" out))
               (#\return (write-string "\\r" out))
               (t (write-char char out))))
    (write-char #\" out)))

(setf (gethash 'import-module ps::*special-statement-operators*)
      (lambda (specifier)
        `(ps-js:escape ,(format nil "import ~a;" (js-string specifier)))))

(-> extension-eval-javascript (string) string)
(defun extension-eval-javascript (code)
  "Return a page script that evaluates CODE and encodes the result."
  (str:replace-all
   "__NYXT_CODE__"
   (js-string code)
   (ps:ps
     (funcall
      (lambda ()
        (ps:try
         (progn
           (ps:var value (ps:chain global-this (eval |__nyxt_code__|)))
           (js-return
            (ps:chain |json|
                      (stringify
                       (ps:create :ok t
                                  :value (if (eql value |UNDEFINED|)
                                             nil
                                             value))))))
         (:catch (error)
           (js-return
            (ps:chain |json|
                      (stringify
                       (ps:create :ok ps:false
                                  :error (|String|
                                          (or (and error (ps:@ error message))
                                              error)))))))))))))

(-> background-worker-javascript (string boolean) string)
(defun background-worker-javascript (worker module-p)
  "Return the service worker script that loads the shim before WORKER."
  (let ((page (string-left-trim "/" worker)))
    (if module-p
        (ps:ps*
         `(progn
            (import-module ,(concatenate 'string "./_nyxt/shim.js"))
            (import-module ,(concatenate 'string "./" page))))
        (ps:ps*
         `(progn
            (|importScripts| ,(concatenate 'string "_nyxt/shim.js"))
            (|importScripts| ,page))))))

(-> user-script-javascript (t t t) string)
(defun user-script-javascript (include exclude code)
  "Return the page script that gates CODE on INCLUDE and EXCLUDE patterns."
  (let ((text (ps:ps
                (funcall
                 (lambda ()
                   (ps:var href (ps:@ location href))
                   (ps:var include |__nyxt_include__|)
                   (ps:var exclude |__nyxt_exclude__|)
                   (defun match (pattern)
                     (ps:var source "^")
                     (ps:for ((i 0)) ((< i (ps:@ pattern length))) ((incf i))
                       (ps:var c (ps:chain pattern (char-at i)))
                       (if (eql c "*")
                           (incf source ".*")
                           (if (>= (ps:chain "\\.^$+?()[]{}|" (index-of c)) 0)
                               (incf source (+ "\\" c))
                               (incf source c))))
                     (js-return (ps:chain (ps:new (funcall |RegExp| (+ source "$")))
                                          (test href))))
                   (if (and (ps:@ include length)
                            (not (ps:chain include (some match))))
                       (js-return))
                   (if (ps:chain exclude (some match))
                       (js-return))
                   |__nyxt_user_script__|
                   (js-return))))))
    (setf text (str:replace-all "__NYXT_INCLUDE__"
                                (cl-json:encode-json-to-string include)
                                text))
    (setf text (str:replace-all "__NYXT_EXCLUDE__"
                                (cl-json:encode-json-to-string (or exclude #()))
                                text))
    (str:replace-all "__NYXT_USER_SCRIPT__" (or code "") text)))

(-> extension-client (string) extension-client)
(defun extension-client (extension-id)
  (bt:with-lock-held (*extension-clients-lock*)
    (or (gethash extension-id *extension-clients*)
        (setf (gethash extension-id *extension-clients*)
              (make-extension-client)))))

(-> extension-event (string &rest t) hash-table)
(defun extension-event (type &rest args)
  "Return an event the shim delivers by calling listeners with ARGS."
  (json-object "type" type
               "args" (coerce args 'vector)))

(defvar *extension-ops* (make-hash-table :test 'equal)
  "Bridge operation name to a function of (request extension-id).")

(defvar *extension-known-tabs* (make-hash-table)
  "Tab ids already announced with tabs.onCreated.")

(defvar *extension-active-tab-id* nil
  "Tab id last announced with tabs.onActivated.")

(defvar *extension-static-rules-hook* nil
  "Function of (directory manifest), called after an extension shim is installed.")

(-> extension-op (string function) string)
(defun extension-op (name function)
  "Register FUNCTION as the handler for the bridge operation NAME."
  (setf (gethash name *extension-ops*) function)
  name)

(-> enqueue-event (string hash-table) hash-table)
(defun enqueue-event (extension-id event)
  "Queue EVENT for the extension worker identified by EXTENSION-ID."
  (let ((client (extension-client extension-id)))
    (bt:with-lock-held ((extension-client-lock client))
      (setf (extension-client-events client)
            (append (extension-client-events client) (list event))))
    (bt:condition-notify (extension-client-wait client)))
  event)

(-> broadcast-extension-event (hash-table) hash-table)
(defun broadcast-extension-event (event)
  (let ((clients '()))
    (bt:with-lock-held (*extension-clients-lock*)
      (maphash (lambda (id client)
                 (declare (ignore id))
                 (push client clients))
               *extension-clients*))
    (dolist (client clients)
      (bt:with-lock-held ((extension-client-lock client))
        (setf (extension-client-events client)
              (append (extension-client-events client) (list event))))
      (bt:condition-notify (extension-client-wait client))))
  event)

(-> take-extension-events (string real) vector)
(defun take-extension-events (extension-id timeout)
  "Return events queued for EXTENSION-ID, waiting up to TIMEOUT seconds."
  (let ((client (extension-client extension-id)))
    (bt:with-lock-held ((extension-client-lock client))
      (loop with deadline = (+ (get-internal-real-time)
                               (round (* timeout internal-time-units-per-second)))
            until (extension-client-events client)
            do (let ((left (/ (- deadline (get-internal-real-time))
                              internal-time-units-per-second)))
                 (when (or (minusp left)
                           (not (bt:condition-wait (extension-client-wait client)
                                                   (extension-client-lock client)
                                                   :timeout left)))
                   (return))))
      (let ((events (extension-client-events client)))
        (setf (extension-client-events client) '())
        (coerce events 'vector)))))

(-> extension-tab-p (t) boolean)
(defun extension-tab-p (buffer)
  (and (web-buffer-p buffer)
       (not (panel-buffer-p buffer))))

(-> extension-tabs () list)
(defun extension-tabs ()
  (remove-if-not #'extension-tab-p (reverse (buffer-list))))

(-> tab-status (buffer) string)
(defun tab-status (buffer)
  (case (nyxt::status buffer)
    (:loading "loading")
    (:unloaded "unloaded")
    (t "complete")))

(-> tab-url (buffer) string)
(defun tab-url (buffer)
  (let ((live (ignore-errors (cef:get-url buffer))))
    (if (and (stringp live) (plusp (length live)) (not (string= live "about:blank")))
        live
        (handler-case
            (let ((rendered (render-url (url buffer))))
              (if (and (stringp rendered) (plusp (length rendered)))
                  rendered
                  ""))
          (error ()
            "")))))

(-> tab-active-p (buffer) boolean)
(defun tab-active-p (buffer)
  (handler-case
      (eq buffer (current-buffer))
    (error ()
      nil)))

(-> tab-description (buffer) hash-table)
(defun tab-description (buffer)
  "Return the chrome.tabs.Tab object for BUFFER."
  (let ((active (tab-active-p buffer))
        (index (or (position buffer (extension-tabs)) 0)))
    (json-object "id" (id buffer)
                 "index" index
                 "windowId" 1
                 "active" active
                 "highlighted" active
                 "incognito" nil
                 "selected" active
                 "status" (tab-status buffer)
                 "url" (tab-url buffer)
                 "title" (or (title buffer) "")
                 "discarded" nil
                 "autoDiscardable" t
                 "pinned" nil)))

(-> match-pattern-p (string string) boolean)
(defun match-pattern-p (pattern url)
  "Return T when URL matches a Chrome match pattern PATTERN."
  (cond
    ((or (string= pattern "<all_urls>") (string= pattern "*://*/*"))
     (or (str:starts-with-p "http:" url)
         (str:starts-with-p "https:" url)
         (str:starts-with-p "file:" url)
         (str:starts-with-p "ftp:" url)))
    ((not (search "://" pattern))
     (and (handler-case
              (cl-ppcre:scan (cl-ppcre:create-scanner
                              (format nil "^~a$"
                                      (cl-ppcre:regex-replace-all
                                       "\\\\\\*"
                                       (cl-ppcre:quote-meta-chars pattern)
                                       ".*"))
                              :case-insensitive-mode t)
                             url)
            (error ()
              nil))
          t))
    (t
     (let* ((scheme-end (search "://" pattern))
            (scheme (subseq pattern 0 scheme-end))
            (rest (subseq pattern (+ scheme-end 3)))
            (slash (position #\/ rest))
            (host (if slash (subseq rest 0 slash) rest))
            (path (if slash (subseq rest slash) "/*"))
            (uri (handler-case (quri:uri url) (error () nil))))
       (and uri
            (let ((url-scheme (or (quri:uri-scheme uri) "")))
              (or (and (string= scheme "*")
                       (or (string= url-scheme "http") (string= url-scheme "https")))
                  (string= scheme url-scheme)))
            (let ((url-host (or (quri:uri-host uri) "")))
              (cond
                ((string= host "*") t)
                ((str:starts-with-p "*." host)
                 (let ((domain (subseq host 2)))
                   (or (string-equal url-host domain)
                       (str:ends-with-p (uiop:strcat "." domain) url-host))))
                (t (string-equal url-host host))))
            (and (handler-case
                     (cl-ppcre:scan
                      (cl-ppcre:create-scanner
                       (format nil "^~a$"
                               (cl-ppcre:regex-replace-all
                                "\\\\\\*"
                                (cl-ppcre:quote-meta-chars path)
                                ".*")))
                      (or (quri:uri-path uri) "/"))
                   (error ()
                     nil))
                 t))))))

(-> extension-url-query-matches-p (t string) boolean)
(defun extension-url-query-matches-p (pattern url)
  "Return T when URL satisfies the tabs.query url filter PATTERN."
  (cond
    ((null pattern) t)
    ((stringp pattern) (match-pattern-p pattern url))
    ((vectorp pattern)
     (some (lambda (item)
             (and (stringp item) (match-pattern-p item url)))
           (coerce pattern 'list)))
    (t t)))

(-> tab-matches-p (buffer hash-table) boolean)
(defun tab-matches-p (buffer query)
  (and (multiple-value-bind (active present-p) (json-field query "active")
         (or (not present-p) (eq (and active t) (tab-active-p buffer))))
       (multiple-value-bind (current present-p) (json-field query "currentWindow")
         (or (not present-p) current))
       (multiple-value-bind (window-id present-p) (json-field query "windowId")
         (or (not present-p)
             (let ((id (json-integer window-id)))
               (or (eql id 1) (eql id -2)))))
       (let ((status (json-string query "status")))
         (or (null status) (string= status (tab-status buffer))))
       (extension-url-query-matches-p (nth-value 0 (json-field query "url"))
                                      (tab-url buffer))
       (let ((title (json-string query "title")))
         (or (null title) (search title (or (title buffer) "") :test #'char-equal)))))

(-> find-extension-tab (t) (or null buffer))
(defun find-extension-tab (tab-id)
  (let ((id (json-integer tab-id)))
    (find id (extension-tabs) :key #'id)))

(-> extension-query-tabs ((or null hash-table)) vector)
(defun extension-query-tabs (query)
  (coerce (mapcar #'tab-description
                  (remove-if-not (lambda (buffer)
                                   (tab-matches-p buffer (or query (json-object))))
                                 (extension-tabs)))
          'vector))

(-> extension-create-tab ((or null hash-table)) hash-table)
(defun extension-create-tab (properties)
  "Open a Nyxt buffer for chrome.tabs.create and return its tab object."
  (let* ((url-text (or (json-string properties "url") "about:blank"))
         (buffer (make-buffer :url (quri:uri url-text))))
    (cond
      ((json-bool properties "active" t)
       (set-current-buffer buffer))
      ((current-window)
       (cef:set-bar-height buffer 1)
       (cef:add-view (current-window) buffer)))
    (tab-description buffer)))

(-> extension-delete-tab (t) (eql :null))
(defun extension-delete-tab (tab-id)
  (let ((buffer (find-extension-tab tab-id)))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (ffi-buffer-delete buffer)
    :null))

(-> current-extension-tab () (or null hash-table))
(defun current-extension-tab ()
  (let ((buffer (find-if #'tab-active-p (extension-tabs))))
    (when buffer
      (tab-description buffer))))

(-> extension-send-message (t t string string) (eql :null))
(defun extension-send-message (tab-id message token extension-id)
  "Post MESSAGE to the content scripts of the tab TAB-ID."
  (let ((buffer (find-extension-tab tab-id)))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (let ((payload (json-object "__nyxt" "deliver"
                                "token" token
                                "message" (or message :null)
                                "sender" (json-object "id" extension-id
                                                      "url" (tab-url buffer)
                                                      "tab" (tab-description buffer)))))
      (ffi-buffer-evaluate-javascript-async
       buffer
       (format nil "window.postMessage(JSON.parse(~a), \"*\")"
               (js-string (j:encode payload)))))
    :null))

(-> extension-script-value (string) t)
(defun extension-script-value (json-text)
  (let ((decoded (handler-case
                     (j:decode json-text)
                   (error ()
                     nil))))
    (unless (hash-table-p decoded)
      (error "The page script did not return a result."))
    (let ((error-text (json-string decoded "error")))
      (when error-text
        (error "~a" error-text)))
    (multiple-value-bind (value present-p) (json-field decoded "value")
      (if present-p value :null))))

(-> extension-execute-script (t string boolean) t)
(defun extension-execute-script (tab-id code statements-p)
  "Evaluate CODE in the page of TAB-ID and return the JSON value.
STATEMENTS-P is accepted so a file of statements and a function expression share
this entry point."
  (declare (ignore statements-p))
  (let ((buffer (find-extension-tab tab-id)))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (let* ((script (extension-eval-javascript code))
           (contents (cef:web-contents buffer)))
      (unless contents
        (error "Tab ~a has no page." tab-id))
      (let ((returned (cef:execute-javascript-synchronous contents script)))
        (unless (stringp returned)
          (error "Tab ~a did not return a script result." tab-id))
        (extension-script-value returned)))))

(-> extension-insert-css (t string) (eql :null))
(defun extension-insert-css (tab-id css)
  (let ((buffer (find-extension-tab tab-id)))
    (unless buffer
      (error "No tab with id: ~a." tab-id))
    (let ((contents (cef:web-contents buffer)))
      (unless contents
        (error "Tab ~a has no page." tab-id))
      (cef:insert-css contents css))
    :null))

(-> download-file-name (t) string)
(defun download-file-name (filename)
  (let ((base (if (stringp filename)
                  (file-namestring (uiop:parse-native-namestring filename))
                  "")))
    (if (or (null base) (uiop:emptyp base))
        "download"
        base)))

(-> extension-start-download ((or null hash-table)) integer)
(defun extension-start-download (options)
  "Save the requested URL into the download directory and return its id."
  (let* ((url-text (json-string options "url"))
         (name (download-file-name (json-string options "filename")))
         (directory (cef-download-directory))
         (path (uiop:merge-pathnames* name directory)))
    (unless (and url-text (not (uiop:emptyp url-text)))
      (error "A download needs a URL."))
    (when (and (uiop:file-exists-p path)
               (string= (or (json-string options "conflictAction") "") "overwrite"))
      (delete-file path))
    (dexador:fetch url-text path :if-exists :supersede)
    (bt:with-lock-held (*extension-download-lock*)
      (let ((download (make-extension-download
                       :id (incf *extension-download-counter*)
                       :url url-text
                       :filename (uiop:native-namestring path)
                       :state "complete")))
        (setf (gethash (extension-download-id download) *extension-downloads*) download)
        (extension-download-id download)))))

(-> download-description (extension-download) hash-table)
(defun download-description (download)
  (json-object "id" (extension-download-id download)
               "url" (extension-download-url download)
               "filename" (extension-download-filename download)
               "state" (extension-download-state download)
               "paused" (and (extension-download-paused download) t)
               "exists" (and (uiop:file-exists-p (extension-download-filename download)) t)))

(-> announce-download (string extension-download) extension-download)
(defun announce-download (extension-id download)
  "Tell EXTENSION-ID that DOWNLOAD was created and finished."
  (let ((item (download-description download)))
    (enqueue-event extension-id (extension-event "downloads.onCreated" item))
    (enqueue-event extension-id
                   (extension-event "downloads.onChanged"
                                    (json-object "id" (extension-download-id download)
                                                 "filename" (json-object "current"
                                                                          (extension-download-filename download))
                                                 "state" (json-object "current"
                                                                      (extension-download-state download))))))
  download)

(-> extension-search-downloads ((or null hash-table)) vector)
(-> download-matches-p (extension-download hash-table) boolean)
(defun download-matches-p (download query)
  (let ((id (json-integer (nth-value 0 (json-field query "id"))))
        (state (json-string query "state"))
        (terms (json-list (nth-value 0 (json-field query "query")))))
    (and (or (null id) (eql id (extension-download-id download)))
         (or (null state) (string= state (extension-download-state download)))
         (every (lambda (term)
                  (or (not (stringp term))
                      (search term (extension-download-filename download) :test #'char-equal)
                      (search term (extension-download-url download) :test #'char-equal)))
                terms))))

(defun extension-search-downloads (query)
  (let ((query (or query (json-object))))
    (bt:with-lock-held (*extension-download-lock*)
      (coerce (loop for download being the hash-value of *extension-downloads*
                    when (download-matches-p download query)
                    collect (download-description download))
              'vector))))

(-> extension-cancel-download (t) (eql :null))
(defun extension-cancel-download (id)
  (let ((download-id (json-integer id)))
    (bt:with-lock-held (*extension-download-lock*)
      (let ((download (gethash download-id *extension-downloads*)))
        (when download
          (setf (extension-download-state download) "interrupted"))))
    :null))

(-> extension-remove-download-file (t) (eql :null))
(defun extension-remove-download-file (id)
  (let ((download-id (json-integer id)))
    (bt:with-lock-held (*extension-download-lock*)
      (let ((download (gethash download-id *extension-downloads*)))
        (when (and download (uiop:file-exists-p (extension-download-filename download)))
          (delete-file (extension-download-filename download)))))
    :null))

(-> extension-erase-downloads ((or null hash-table)) vector)
(defun extension-erase-downloads (query)
  (let ((id (json-integer (nth-value 0 (json-field query "id"))))
        (erased '()))
    (bt:with-lock-held (*extension-download-lock*)
      (maphash (lambda (key download)
                 (when (or (null id) (eql id (extension-download-id download)))
                   (push key erased)
                   (remhash key *extension-downloads*)))
               *extension-downloads*))
    (coerce (nreverse erased) 'vector)))

(-> remember-menu-item (string string string) extension-menu-item)
(defun remember-menu-item (extension-id menu-id title)
  (bt:with-lock-held (*extension-menu-lock*)
    (setf *extension-menu-items*
          (remove-if (lambda (item)
                       (and (string= (extension-menu-item-extension-id item) extension-id)
                            (string= (extension-menu-item-menu-id item) menu-id)))
                     *extension-menu-items*))
    (let ((item (make-extension-menu-item :extension-id extension-id
                                          :menu-id menu-id
                                          :title (if (uiop:emptyp title) menu-id title))))
      (push item *extension-menu-items*)
      item)))

(-> menu-click-event (string string) hash-table)
(defun menu-click-event (extension-id menu-id)
  (declare (ignore extension-id))
  (let ((tab (or (current-extension-tab) :null)))
    (extension-event "contextMenus.onClicked"
                     (json-object "menuItemId" menu-id
                                  "editable" nil
                                  "pageUrl" (if (hash-table-p tab)
                                                (or (json-string tab "url") "")
                                                ""))
                     tab)))

(defvar *extension-click-at* (make-hash-table :test 'equal)
  "Last time an extension click key was delivered, in internal time units.")

(defvar *extension-click-lock* (bt:make-lock "extension clicks"))

(-> repeat-extension-click-p (string) boolean)
(defun repeat-extension-click-p (key)
  "Return T when KEY was delivered in the last second.
A context-menu or toolbar click can be reported twice for one gesture."
  (bt:with-lock-held (*extension-click-lock*)
    (let ((now (get-internal-real-time))
          (previous (gethash key *extension-click-at*)))
      (setf (gethash key *extension-click-at*) now)
      (and previous
           (< (- now previous) internal-time-units-per-second)))))

(-> extension-context-menu-entries () list)
(defun extension-context-menu-entries ()
  "Return context-menu rows for items extensions have created."
  (bt:with-lock-held (*extension-menu-lock*)
    (mapcar (lambda (item)
              (let ((extension-id (extension-menu-item-extension-id item))
                    (menu-id (extension-menu-item-menu-id item)))
                (cons (extension-menu-item-title item)
                      (lambda ()
                        (unless (repeat-extension-click-p
                                 (uiop:strcat extension-id " " menu-id))
                          (enqueue-event extension-id
                                         (menu-click-event extension-id menu-id)))))))
            (reverse *extension-menu-items*))))

(-> note-extension-tab (buffer) (or null hash-table))
(-> navigation-details (buffer) hash-table)
(defun navigation-details (buffer)
  "Return the webNavigation details object for BUFFER's main frame."
  (json-object "tabId" (id buffer)
               "url" (tab-url buffer)
               "frameId" 0
               "parentFrameId" -1
               "processId" 0
               "timeStamp" (round (* 1000 (time:timestamp-to-unix (time:now))))
               "transitionType" "link"))

(defun note-extension-tab (buffer)
  "Tell extension workers that BUFFER's load status changed."
  (when (extension-tab-p buffer)
    (let* ((tab (tab-description buffer))
           (tab-id (id buffer))
           (status (tab-status buffer))
           (details (navigation-details buffer)))
      (unless (gethash tab-id *extension-known-tabs*)
        (setf (gethash tab-id *extension-known-tabs*) t)
        (broadcast-extension-event (extension-event "tabs.onCreated" tab)))
      (when (and (tab-active-p buffer)
                 (not (eql *extension-active-tab-id* tab-id)))
        (setf *extension-active-tab-id* tab-id)
        (broadcast-extension-event
         (extension-event "tabs.onActivated"
                          (json-object "tabId" tab-id "windowId" 1)))
        (broadcast-extension-event
         (extension-event "tabs.onHighlighted"
                          (json-object "windowId" 1 "tabIds" (vector tab-id)))))
      (broadcast-extension-event
       (extension-event "tabs.onUpdated"
                        tab-id
                        (json-object "status" status
                                     "url" (tab-url buffer))
                        tab))
      (cond
        ((string= status "loading")
         (broadcast-extension-event (extension-event "webNavigation.onBeforeNavigate" details))
         (broadcast-extension-event (extension-event "webNavigation.onCommitted" details)))
        ((string= status "complete")
         (broadcast-extension-event (extension-event "webNavigation.onDOMContentLoaded" details))
         (broadcast-extension-event (extension-event "webNavigation.onCompleted" details))
         (broadcast-extension-event
          (extension-event "history.onVisited"
                           (json-object "id" (princ-to-string tab-id)
                                        "url" (tab-url buffer)
                                        "title" (or (title buffer) "")
                                        "lastVisitTime" (round (* 1000 (time:timestamp-to-unix (time:now))))
                                        "visitCount" 1
                                        "typedCount" 0))))))))

(-> deliver-extension-action (web-extension &optional (or null buffer)) (or null hash-table))
(defun deliver-extension-action (extension &optional buffer)
  "Deliver chrome.action.onClicked for EXTENSION.
BUFFER is the tab the click is about. Opening an extension page is the click
Nyxt can make, because this window has no toolbar button."
  (when (and extension
             (plusp (length (extension-id extension)))
             (not (repeat-extension-click-p
                   (uiop:strcat (extension-id extension) " action"))))
    (enqueue-event (extension-id extension)
                   (extension-event "action.onClicked"
                                    (if (extension-tab-p buffer)
                                        (tab-description buffer)
                                        (or (current-extension-tab) :null))))))

(-> bridge-request (hash-table) hash-table)
(defun bridge-request (request)
  (let ((op (or (json-string request "op") ""))
        (extension-id (or (json-string request "extensionId") "")))
    (cond
      ((string= op "events")
       (json-object "ok" t
                    "result" (take-extension-events extension-id 20)))
      ((string= op "tabs.query")
       (json-object "ok" t
                    "result" (extension-query-tabs (let ((query (nth-value 0 (json-field request "query"))))
                                                     (when (hash-table-p query) query)))))
      ((string= op "tabs.create")
       (json-object "ok" t
                    "result" (extension-create-tab
                              (let ((properties (nth-value 0 (json-field request "properties"))))
                                (when (hash-table-p properties) properties)))))
      ((string= op "tabs.get")
       (let ((buffer (find-extension-tab (nth-value 0 (json-field request "tabId")))))
         (unless buffer
           (error "No tab with id: ~a." (json-field request "tabId")))
         (json-object "ok" t "result" (tab-description buffer))))
      ((string= op "tabs.remove")
       (extension-delete-tab (nth-value 0 (json-field request "tabId")))
       (json-object "ok" t "result" :null))
      ((string= op "tabs.sendMessage")
       (extension-send-message (nth-value 0 (json-field request "tabId"))
                               (nth-value 0 (json-field request "message"))
                               (or (json-string request "token") "")
                               extension-id)
       (json-object "ok" t "result" :null))
      ((string= op "scripting.executeScript")
       (json-object "ok" t
                    "result" (extension-execute-script
                              (nth-value 0 (json-field request "tabId"))
                              (or (json-string request "code") "")
                              (json-bool request "statements"))))
      ((string= op "scripting.insertCSS")
       (extension-insert-css (nth-value 0 (json-field request "tabId"))
                             (or (json-string request "css") ""))
       (json-object "ok" t "result" :null))
      ((string= op "contextMenus.create")
       (let ((item (nth-value 0 (json-field request "item"))))
         (when (hash-table-p item)
           (remember-menu-item extension-id
                               (json-identifier (nth-value 0 (json-field item "id")))
                               (or (json-string item "title") ""))))
       (json-object "ok" t "result" :null))
      ((string= op "contextMenus.activate")
       (let* ((menu-id (json-identifier (nth-value 0 (json-field request "menuItemId"))))
              (event (menu-click-event extension-id menu-id)))
         (enqueue-event extension-id event)
         (json-object "ok" t "result" :null)))
      ((string= op "downloads.download")
       (let* ((id (extension-start-download
                   (let ((options (nth-value 0 (json-field request "options"))))
                     (when (hash-table-p options) options))))
              (download (bt:with-lock-held (*extension-download-lock*)
                          (gethash id *extension-downloads*))))
         (when download
           (announce-download extension-id download))
         (json-object "ok" t "result" id)))
      ((string= op "downloads.search")
       (json-object "ok" t
                    "result" (extension-search-downloads
                              (let ((query (nth-value 0 (json-field request "query"))))
                                (when (hash-table-p query) query)))))
      ((string= op "downloads.cancel")
       (extension-cancel-download (nth-value 0 (json-field request "id")))
       (json-object "ok" t "result" :null))
      ((string= op "downloads.removeFile")
       (extension-remove-download-file (nth-value 0 (json-field request "id")))
       (json-object "ok" t "result" :null))
      ((string= op "downloads.erase")
       (json-object "ok" t
                    "result" (extension-erase-downloads
                              (let ((query (nth-value 0 (json-field request "query"))))
                                (when (hash-table-p query) query)))))
      (t
       (let ((handler (gethash op *extension-ops*)))
         (if handler
             (funcall handler request extension-id)
             (error "Unknown extension bridge request ~a." op)))))))

(-> read-http-headers (stream) list)
(defun read-http-headers (stream)
  (loop for line = (read-line stream nil nil)
        while line
        for trimmed = (string-trim '(#\return #\space #\tab) line)
        until (zerop (length trimmed))
        collect trimmed))

(-> header-value (list string) (or null string))
(defun header-value (headers name)
  (let ((prefix (uiop:strcat (string-downcase name) ":")))
    (loop for header in headers
          when (str:starts-with-p prefix (string-downcase header))
            return (string-trim '(#\space #\tab) (subseq header (length prefix))))))

(-> read-http-body (stream list) string)
(defun read-http-body (stream headers)
  (let ((length (parse-integer (or (header-value headers "content-length") "0")
                               :junk-allowed t)))
    (if (or (null length) (not (plusp length)) (> length 16000000))
        ""
        (let ((buffer (make-string length)))
          (read-sequence buffer stream)
          buffer))))

(-> write-http-json (stream integer string) null)
(defun write-http-json (stream status text)
  (format stream "HTTP/1.1 ~d ~a~c~c" status (if (= status 200) "OK" "Error") #\return #\newline)
  (format stream "Content-Type: application/json; charset=utf-8~c~c" #\return #\newline)
  (format stream "Content-Length: ~d~c~c" (length text) #\return #\newline)
  (format stream "Connection: close~c~c~c~c" #\return #\newline #\return #\newline)
  (write-string text stream)
  (finish-output stream)
  nil)

(-> handle-bridge-client (stream) null)
(defun handle-bridge-client (stream)
  (handler-case
      (let* ((headers (read-http-headers stream))
             (body (read-http-body stream headers))
             (token (or (header-value headers "x-nyxt-token") "")))
        (unless (and (plusp (length *extension-bridge-token*))
                     (string= token *extension-bridge-token*))
          (write-http-json stream 403 (j:encode (json-object "ok" nil "error" "Forbidden.")))
          (return-from handle-bridge-client nil))
        (let* ((request (handler-case
                            (j:decode body)
                          (error ()
                            nil)))
               (response (handler-case
                             (if (hash-table-p request)
                                 (bridge-request request)
                                 (error "The bridge request is not an object."))
                           (error (condition)
                             (json-object "ok" nil "error" (princ-to-string condition))))))
          (write-http-json stream 200 (j:encode response))))
    (end-of-file ()
      nil)
    (error ()
      nil)))

(-> bridge-token () string)
(defun bridge-token ()
  (with-output-to-string (out)
    (loop repeat 32
          do (write-char (char "0123456789abcdef" (random 16)) out))))

(-> extension-bridge-file () pathname)
(defun extension-bridge-file ()
  (uiop:xdg-cache-home "nyxt/extension-bridge"))

(-> saved-extension-bridge () (values (or null integer) (or null string)))
(defun saved-extension-bridge ()
  "Return the port and token written for this profile, when both are present."
  (let ((file (extension-bridge-file)))
    (unless (uiop:file-exists-p file)
      (return-from saved-extension-bridge (values nil nil)))
    (let ((lines (handler-case
                     (uiop:read-file-lines file)
                   (error ()
                     '()))))
      (if (and (stringp (first lines))
               (stringp (second lines))
               (plusp (length (first lines)))
               (= 32 (length (second lines)))
               (every #'digit-char-p (first lines))
               (every (lambda (char) (digit-char-p char 16)) (second lines)))
          (values (parse-integer (first lines)) (second lines))
          (values nil nil)))))

(-> remember-extension-bridge (integer string) pathname)
(defun remember-extension-bridge (port token)
  (let ((file (extension-bridge-file)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
      (format out "~a~%~a~%" port token))
    file))

(-> start-extension-bridge (&optional integer) integer)
(defun start-extension-bridge (&optional (port 0))
  "Listen on 127.0.0.1 for extension shim requests and return the port."
  (let ((server (iolib:make-socket :address-family :internet
                                   :type :stream
                                   :connect :passive
                                   :local-host "127.0.0.1"
                                   :local-port port
                                   :reuse-address t
                                   :backlog 32)))
    (setf *extension-bridge-port* (iolib:local-port server))
    (bt:make-thread
     (lambda ()
       (loop for client = (handler-case
                              (iolib:accept-connection server)
                            (error ()
                              nil))
             while client
             do (let ((connection client))
                  (bt:make-thread
                   (lambda ()
                     (unwind-protect
                          (handle-bridge-client connection)
                       (handler-case
                           (close connection)
                         (error ()
                           nil))))
                   :name "extension bridge"))))
     :name "extension bridge accept")
    *extension-bridge-port*))

(-> ensure-extension-bridge () integer)
(defun ensure-extension-bridge ()
  "Start the extension bridge once and return its port."
  (unless *extension-bridge-started*
    (multiple-value-bind (port token) (saved-extension-bridge)
      (setf *extension-bridge-token* (or token (bridge-token)))
      (handler-case
          (start-extension-bridge (or port 0))
        (error ()
          (setf *extension-bridge-token* (bridge-token))
          (start-extension-bridge 0))))
    (remember-extension-bridge *extension-bridge-port* *extension-bridge-token*)
    (setf *extension-bridge-started* t))
  *extension-bridge-port*)

(-> shim-text () string)
(defun shim-text ()
  (let ((source (extension-shim-javascript)))
    (setf source (str:replace-all "__NYXT_PORT__" (princ-to-string *extension-bridge-port*) source))
    (str:replace-all "__NYXT_TOKEN__" *extension-bridge-token* source)))

(-> write-extension-text (pathname string) pathname)
(defun write-extension-text (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
    (write-string text out))
  path)

(-> nyxt-worker-p (t) boolean)
(defun nyxt-worker-p (name)
  (and (stringp name)
       (> (length name) 14)
       (string= "nyxt-" (subseq name 0 5))
       (string= "-background.js" (subseq name (- (length name) 15)))))

(-> manifest-shimmed-p (hash-table) boolean)
(defun manifest-shimmed-p (manifest)
  (let ((background (gethash "background" manifest)))
    (when (hash-table-p background)
      (or (nyxt-worker-p (gethash "service_worker" background))
          (find "_nyxt/shim.js" (json-list (gethash "scripts" background)) :test #'string=)))))

(-> source-manifest (pathname hash-table string) hash-table)
(defun source-manifest (directory manifest text)
  "Return the manifest as it was before Nyxt added its shim."
  (let ((source (uiop:merge-pathnames* #p"_nyxt/source-manifest.json"
                                       (uiop:ensure-directory-pathname directory))))
    (cond
      ((uiop:file-exists-p source)
       (j:decode (uiop:read-file-string source)))
      ((manifest-shimmed-p manifest)
       manifest)
      (t
       (write-extension-text source text)
       (j:decode text)))))

(-> vector-add (t t &key (:front boolean)) vector)
(defun vector-add (value item &key front)
  (let ((items (json-list value)))
    (coerce (if (member item items :test #'equal)
                items
                (if front
                    (cons item items)
                    (append items (list item))))
            'vector)))

(-> content-bridge-entry () hash-table)
(defun content-bridge-entry ()
  (json-object "matches" #("<all_urls>")
               "js" #("_nyxt/content.js")
               "run_at" "document_start"
               "all_frames" t
               "match_about_blank" t))

(-> content-bridge-present-p (t) boolean)
(defun content-bridge-present-p (scripts)
  (some (lambda (entry)
          (and (hash-table-p entry)
               (find "_nyxt/content.js" (json-list (gethash "js" entry)) :test #'string=)))
        (json-list scripts)))

(-> install-background-shim (pathname hash-table) null)
(defun install-background-shim (directory manifest)
  (let ((background (gethash "background" manifest)))
    (unless (hash-table-p background)
      (return-from install-background-shim nil))
    (let* ((worker (json-string background "service_worker"))
           (module-p (equal (json-string background "type") "module")))
      (when (and worker (not (nyxt-worker-p worker)))
        (write-extension-text
         (uiop:merge-pathnames* #p"nyxt-background.js"
                                (uiop:ensure-directory-pathname directory))
         (background-worker-javascript worker module-p))
        (setf (gethash "service_worker" background) "nyxt-background.js")))
    (let ((scripts (json-list (gethash "scripts" background))))
      (when scripts
        (setf (gethash "scripts" background)
              (vector-add scripts "_nyxt/shim.js" :front t)))))
  nil)

(-> install-permission (hash-table string string) null)
(defun install-permission (manifest key item)
  (setf (gethash key manifest) (vector-add (gethash key manifest) item))
  nil)

(-> relative-shim-src (pathname pathname) string)
(defun relative-shim-src (html directory)
  (substitute #\/ #\\
              (namestring
               (enough-namestring
                (uiop:merge-pathnames* #p"_nyxt/shim.js"
                                       (uiop:ensure-directory-pathname directory))
                (uiop:pathname-directory-pathname html)))))

(-> inject-shim-tag (pathname pathname) null)
(defun inject-shim-tag (html directory)
  (let ((text (handler-case
                  (uiop:read-file-string html)
                (error ()
                  nil))))
    (when (and text
               (search "<head" text)
               (not (search "_nyxt/shim.js" text)))
      (let* ((start (search "<head" text))
             (end (position #\> text :start start))
             (src (relative-shim-src html directory)))
        (when end
          (write-extension-text
           html
           (uiop:strcat (subseq text 0 (1+ end))
                        (format nil "<script src=\"~a\"></script>" src)
                        (subseq text (1+ end))))))))
  nil)

(-> extension-html-files (pathname) list)
(defun extension-html-files (directory)
  (let ((files '()))
    (labels ((walk (path)
               (dolist (file (uiop:directory-files path))
                 (when (member (pathname-type file) '("html" "htm") :test #'string-equal)
                   (push file files)))
               (dolist (child (uiop:subdirectories path))
                 (unless (string= (first (last (pathname-directory child))) "_nyxt")
                   (walk child)))))
      (walk (uiop:ensure-directory-pathname directory)))
    files))

(-> install-extension-shim (pathname) (or null pathname))
(defun install-extension-shim (directory)
  "Copy the Nyxt shim into DIRECTORY and point the manifest at it.
The original manifest is kept at _nyxt/source-manifest.json."
  (let ((file (manifest-file directory)))
    (unless (uiop:file-exists-p file)
      (return-from install-extension-shim nil))
    (handler-case
        (let* ((text (uiop:read-file-string file))
               (current (j:decode text)))
          (unless (hash-table-p current)
            (return-from install-extension-shim nil))
          (ensure-extension-bridge)
          (let ((manifest (source-manifest directory current text))
                (root (uiop:ensure-directory-pathname directory)))
            (write-extension-text (uiop:merge-pathnames* #p"_nyxt/shim.js" root) (shim-text))
            (write-extension-text (uiop:merge-pathnames* #p"_nyxt/content.js" root)
                                  (extension-content-javascript))
            (install-background-shim root manifest)
            (unless (content-bridge-present-p (gethash "content_scripts" manifest))
              (setf (gethash "content_scripts" manifest)
                    (vector-add (gethash "content_scripts" manifest)
                                (content-bridge-entry)
                                :front t)))
            (install-permission manifest "host_permissions" "http://127.0.0.1/*")
            (unless (eql (json-integer (gethash "manifest_version" manifest)) 3)
              (install-permission manifest "permissions" "http://127.0.0.1/*"))
            (write-extension-text file (j:encode manifest))
            (when *extension-static-rules-hook*
              (funcall *extension-static-rules-hook* root manifest))
            (dolist (html (extension-html-files root))
              (inject-shim-tag html root))
            file))
      (error (condition)
        (log:warn "Could not install the extension shim in ~a: ~a." directory condition)
        nil))))

(-> toggleable-web-extensions () list)
(defun toggleable-web-extensions ()
  "Return loaded extensions whose toolbar button this window can press."
  (remove-if-not #'extension-loaded-p (listed-web-extensions)))

(define-command-global toggle-web-extension (&optional extension)
  "Press an extension's toolbar button.
This window has no toolbar. Choosing an extension here is that click.
Dark Mode's click switches its theme; reload the page to see it."
  (handler-case
      (let ((extension (cond
                         ((web-extension-p extension) extension)
                         ((stringp extension) (web-extension-named extension))
                         (t (let ((extensions (toggleable-web-extensions)))
                              (unless extensions
                                (echo "No web extensions are loaded.")
                                (return-from toggle-web-extension nil))
                              (prompt1 :prompt "Toggle web extension"
                                       :sources (make-instance 'web-extension-source
                                                               :constructor extensions)))))))
        (cond
          ((null extension)
           nil)
          ((not (extension-loaded-p extension))
           (echo "~a starts on the next launch." (extension-name extension))
           nil)
          (t
           (deliver-extension-action extension (current-buffer))
           (echo "Toggled ~a. Reload the page to see it." (extension-name extension))
           (extension-name extension))))
    (prompt-buffer-canceled ()
      nil)
    (error (condition)
      (echo-warning "Could not toggle the web extension: ~a" condition)
      nil)))

(setf *extension-tab-hook* #'note-extension-tab)
(setf *extension-context-menu-hook* #'extension-context-menu-entries)
