;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/migrate
  (:documentation "Import history and bookmarks from other browsers."))
(in-package :nyxt/mode/migrate)

(define-class browser-profile ()
  ((profile-name "")
   (profile-path #p""
                 :type pathname)
   (default-p nil
              :type boolean))
  (:export-class-name-p nil)
  (:export-accessor-names-p nil)
  (:documentation "A Firefox or Chrome profile to import from."))

(defmethod prompter:object-attributes ((profile browser-profile) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(profile-name profile))
    ("Directory" ,(uiop:native-namestring (profile-path profile)))))

(-> firefox-ini-files () (list-of pathname))
(defun firefox-ini-files ()
  "Return profiles.ini files for native, Flatpak, and Snap Firefox."
  (remove-if-not
   #'uiop:file-exists-p
   (mapcar (lambda (relative)
             (uiop:merge-pathnames* relative (user-homedir-pathname)))
           '(".mozilla/firefox/profiles.ini"
             ".var/app/org.mozilla.firefox/.mozilla/firefox/profiles.ini"
             "snap/firefox/common/.mozilla/firefox/profiles.ini"))))

(-> ini-sections (pathname) list)
(defun ini-sections (path)
  "Return PATH as an alist of section names to key alists."
  (let ((sections '())
        (current nil))
    (with-open-file (stream path :external-format :utf-8)
      (loop for raw = (read-line stream nil nil)
            while raw
            for line = (string-trim '(#\Space #\Tab #\Return) raw)
            do (cond
                 ((or (zerop (length line))
                      (char= (char line 0) #\#)
                      (char= (char line 0) #\;)))
                 ((and (char= (char line 0) #\[)
                       (char= (char line (1- (length line))) #\]))
                  (setf current (subseq line 1 (1- (length line))))
                  (push (list current) sections))
                 (current
                  (let ((separator (position #\= line)))
                    (when separator
                      (let ((section (assoc current sections :test #'string=)))
                        (push (cons (subseq line 0 separator)
                                    (subseq line (1+ separator)))
                              (cdr section)))))))))
    (nreverse sections)))

(-> ini-value (list string string) (or string null))
(defun ini-value (sections section key)
  (cdr (assoc key
              (cdr (assoc section sections :test #'string=))
              :test #'string=)))

(-> firefox-directory (pathname string boolean) pathname)
(defun firefox-directory (ini-file path relative-p)
  "Resolve PATH against INI-FILE when RELATIVE-P is true.
A trailing slash is added so names such as \"profile.default\" stay one
directory component."
  (let* ((text (if (uiop:string-suffix-p path "/")
                   path
                   (uiop:strcat path "/")))
         (parsed (uiop:ensure-pathname text :want-pathname t :ensure-directory t)))
    (if relative-p
        (uiop:merge-pathnames* parsed (uiop:pathname-directory-pathname ini-file))
        parsed)))

(-> directory-key (pathname) string)
(defun directory-key (path)
  (let ((true (uiop:truename* path)))
    (uiop:native-namestring
     (uiop:ensure-directory-pathname (or true path)))))

(-> profiles-in-ini (pathname) (values list list &optional))
(defun profiles-in-ini (ini-file)
  "Return the profiles in INI-FILE and the install-default directories.
Install defaults are the profiles Firefox itself opens for each installation."
  (let ((sections (ini-sections ini-file))
        (profiles '())
        (install-paths '()))
    (dolist (section sections)
      (let ((name (first section)))
        (cond
          ((uiop:string-prefix-p "Profile" name)
           (let ((path (ini-value sections name "Path"))
                 (relative (ini-value sections name "IsRelative")))
             (when (and path relative)
               (push (make-instance
                      'browser-profile
                      :profile-name (let ((label (ini-value sections name "Name")))
                                      (if (or (null label) (str:emptyp label))
                                          (let ((directory (pathname-directory
                                                            (firefox-directory
                                                             ini-file path
                                                             (string= relative "1")))))
                                            (substitute #\Space #\-
                                                        (string (car (last directory)))))
                                          (substitute #\Space #\- label)))
                      :profile-path (firefox-directory ini-file path
                                                       (string= relative "1"))
                      :default-p (string= (ini-value sections name "Default") "1"))
                     profiles))))
          ((uiop:string-prefix-p "Install" name)
           (let ((path (ini-value sections name "Default")))
             (when path
               (push (firefox-directory ini-file path
                                        (not (uiop:string-prefix-p "/" path)))
                     install-paths)))))))
    (values (nreverse profiles) (nreverse install-paths))))

(-> choose-browser-profile (list list) (or browser-profile keyword null))
(defun choose-browser-profile (profiles default-paths)
  "Pick the profile to import.
A single profile is used as-is. Otherwise one directory in DEFAULT-PATHS wins,
then the profile marked default. :CHOOSE means the caller should ask."
  (let* ((keys (remove-duplicates (mapcar #'directory-key default-paths) :test #'string=))
         (matches (remove-if-not
                   (lambda (profile)
                     (member (directory-key (profile-path profile)) keys :test #'string=))
                   profiles)))
    (cond
      ((null profiles) nil)
      ((null (rest profiles)) (first profiles))
      ((and matches (null (rest matches))) (first matches))
      (matches :choose)
      ((find-if #'default-p profiles))
      (t :choose))))

(-> firefox-profile-candidates () (values list list &optional))
(defun firefox-profile-candidates ()
  (let ((profiles '())
        (install-paths '()))
    (dolist (ini (firefox-ini-files))
      (multiple-value-bind (found defaults) (profiles-in-ini ini)
        (setf profiles (append profiles found)
              install-paths (append install-paths defaults))))
    (values (delete-duplicates
             (remove-if-not (lambda (profile)
                              (uiop:directory-exists-p (profile-path profile)))
                            profiles)
             :key (lambda (profile) (directory-key (profile-path profile)))
             :test #'string=
             :from-end t)
            install-paths)))

(-> json-object (pathname) (or hash-table null))
(defun json-object (path)
  "Return the JSON object in PATH, or NIL when PATH is missing or unreadable."
  (when (uiop:file-exists-p path)
    (handler-case
        (let ((value (j:decode (uiop:read-file-string path :external-format :utf-8))))
          (when (hash-table-p value)
            value))
      (error () nil))))

(-> json-string (t string) (or string null))
(defun json-string (object key)
  (when (hash-table-p object)
    (let ((value (j:get key object)))
      (when (and (stringp value) (not (str:emptyp value)))
        value))))

(-> json-strings (t string) list)
(defun json-strings (object key)
  "Return the string elements of the JSON array at KEY."
  (let ((value (and (hash-table-p object) (j:get key object))))
    (if (vectorp value)
        (loop for element across value
              when (and (stringp element) (not (str:emptyp element)))
                collect element)
        '())))

(-> chrome-user-data-directories () (list-of pathname))
(defun chrome-user-data-directories ()
  "Return user-data directories for native, Flatpak, and Snap Chrome."
  (remove-if-not
   #'uiop:directory-exists-p
   (append
    (mapcar (lambda (name)
              (uiop:xdg-config-home (uiop:strcat name "/")))
            '("google-chrome" "google-chrome-beta" "google-chrome-unstable"))
    (mapcar (lambda (relative)
              (uiop:merge-pathnames* relative (user-homedir-pathname)))
            '(".var/app/com.google.Chrome/config/google-chrome/"
              ".var/app/com.google.ChromeDev/config/google-chrome-unstable/"
              "snap/google-chrome/common/google-chrome/")))))

(-> chrome-profile-path (pathname string) pathname)
(defun chrome-profile-path (user-data directory-name)
  (uiop:ensure-directory-pathname
   (uiop:merge-pathnames* (uiop:strcat directory-name "/") user-data)))

(-> ephemeral-profile-p (t) boolean)
(defun ephemeral-profile-p (info)
  (and (hash-table-p info)
       (eq t (j:get "is_ephemeral" info))))

(-> chrome-profile-name (t string) string)
(defun chrome-profile-name (info directory-name)
  (or (json-string info "name")
      (substitute #\Space #\- directory-name)))

(-> chrome-default-paths (pathname hash-table) list)
(defun chrome-default-paths (user-data profile)
  "Return the profile directories Chrome itself would open.
`last_used' wins, then the profiles active last, then the first of
`profiles_order'."
  (let ((last-used (json-string profile "last_used"))
        (active (json-strings profile "last_active_profiles"))
        (order (json-strings profile "profiles_order")))
    (mapcar (lambda (name) (chrome-profile-path user-data name))
            (cond
              (last-used (list last-used))
              (active active)
              (order (list (first order)))
              (t '())))))

(-> profiles-in-chrome (pathname) (values list list &optional))
(defun profiles-in-chrome (user-data)
  "Return the profiles under USER-DATA and Chrome's default profile directories.
Ephemeral profiles are left out. When Local State has no profiles, directories
that contain History or Bookmarks are used."
  (let* ((state (json-object (uiop:merge-pathnames* "Local State" user-data)))
         (profile (and state (j:get "profile" state)))
         (cache (and (hash-table-p profile) (j:get "info_cache" profile)))
         (profiles '())
         (defaults (if (hash-table-p profile)
                       (chrome-default-paths user-data profile)
                       '())))
    (if (hash-table-p cache)
        (maphash
         (lambda (directory-name info)
           (when (and (stringp directory-name)
                      (not (ephemeral-profile-p info)))
             (let ((path (chrome-profile-path user-data directory-name)))
               (when (uiop:directory-exists-p path)
                 (push (make-instance 'browser-profile
                                      :profile-name (chrome-profile-name info directory-name)
                                      :profile-path path)
                       profiles)))))
         cache)
        (dolist (directory (uiop:subdirectories user-data))
          (when (or (uiop:file-exists-p (uiop:merge-pathnames* "History" directory))
                    (uiop:file-exists-p (uiop:merge-pathnames* "Bookmarks" directory)))
            (let ((directory-name (car (last (pathname-directory directory)))))
              (when (stringp directory-name)
                (push (make-instance 'browser-profile
                                     :profile-name (substitute #\Space #\- directory-name)
                                     :profile-path directory)
                      profiles))))))
    (values (sort profiles #'string< :key #'profile-name) defaults)))

(-> chrome-profile-candidates () (values list list &optional))
(defun chrome-profile-candidates ()
  (let ((profiles '())
        (defaults '()))
    (dolist (user-data (chrome-user-data-directories))
      (multiple-value-bind (found default-paths) (profiles-in-chrome user-data)
        (setf profiles (append profiles found)
              defaults (append defaults default-paths))))
    (values (delete-duplicates
             (remove-if-not (lambda (profile)
                              (uiop:directory-exists-p (profile-path profile)))
                            profiles)
             :key (lambda (profile) (directory-key (profile-path profile)))
             :test #'string=
             :from-end t)
            defaults)))

(-> prompt-browser-profile (list string string) (or browser-profile null))
(defun prompt-browser-profile (profiles prompt source-name)
  (handler-case
      (prompt1 :prompt prompt
               :sources (make-instance
                         'prompter:source
                         :name source-name
                         :constructor (lambda (source)
                                        (declare (ignore source))
                                        profiles)))
    (prompt-buffer-canceled () nil)))

(-> selected-browser-profile (list list string string) (or browser-profile null))
(defun selected-browser-profile (profiles default-paths prompt source-name)
  (let ((profile (choose-browser-profile profiles default-paths)))
    (if (eq profile :choose)
        (prompt-browser-profile profiles prompt source-name)
        profile)))

(-> selected-firefox-profile () (or browser-profile null))
(defun selected-firefox-profile ()
  (multiple-value-bind (profiles install-paths) (firefox-profile-candidates)
    (selected-browser-profile profiles install-paths
                              "Firefox profile" "Firefox profiles")))

(-> selected-chrome-profile () (or browser-profile null))
(defun selected-chrome-profile ()
  (multiple-value-bind (profiles default-paths) (chrome-profile-candidates)
    (selected-browser-profile profiles default-paths
                              "Chrome profile" "Chrome profiles")))

(-> importable-url (t) (or quri:uri null))
(defun importable-url (value)
  "Return VALUE as a URL an import should keep.
Internal browser pages are left out: wyciwyg, place, about, chrome, and
extension URLs."
  (when (and (stringp value) (not (str:emptyp value)))
    (let ((url (handler-case (quri:uri value)
                 (error () nil))))
      (when (and (quri:uri-p url)
                 (quri:uri-scheme url)
                 (not (member (quri:uri-scheme url)
                              '("wyciwyg" "place" "about" "chrome"
                                "chrome-extension" "chrome-untrusted"
                                "chrome-search" "devtools")
                              :test #'string-equal))
                 (or (not (member (quri:uri-scheme url)
                                  '("http" "https")
                                  :test #'string-equal))
                     (quri:uri-host url))
                 (not (url-empty-p url)))
        url))))

(-> ensure-history-loaded () t)
(defun ensure-history-loaded ()
  (when (and *browser* (zerop (length (history-vector *browser*))))
    (let ((existing (handler-case (files:content (history-file *browser*))
                      (error () nil))))
      (when (and existing (plusp (length existing)))
        (setf (history-vector *browser*)
              (make-array (length existing)
                          :fill-pointer t
                          :adjustable t
                          :initial-contents existing)))))
  t)

(-> query-rows (pathname string) list)
(defun query-rows (database-path query)
  (sqlite:with-open-database (database (uiop:native-namestring database-path)
                              :busy-timeout 2000)
    (sqlite:execute-to-list database query)))

(-> import-history-rows (list) integer)
(defun import-history-rows (rows)
  "Insert one history entry per new URL in ROWS.
ROWS are (url-string title). URLs already in `history-vector' are left in place."
  (ensure-history-loaded)
  (let ((seen (make-hash-table :test #'equal))
        (count 0))
    (loop for entry across (history-vector *browser*)
          for rendered = (render-url (url entry))
          unless (str:emptyp rendered)
            do (setf (gethash rendered seen) t))
    (loop for (url-string title) in rows
          for url = (importable-url url-string)
          for rendered = (and url (render-url url))
          when (and rendered (not (gethash rendered seen)))
            do (setf (gethash rendered seen) t)
               (incf count)
               (vector-push-extend
                (make-instance 'history-entry
                               :url url
                               :title (if (stringp title) title ""))
                (history-vector *browser*)))
    (when (plusp count)
      (files:with-file-content (history (history-file *browser*))
        (setf history (history-vector *browser*))))
    count))

(-> import-firefox-history (pathname) integer)
(defun import-firefox-history (database-path)
  "Import one history entry per URL from a Firefox places database.
Visits of type embed, redirect, download, framed link, and reload are left
out, matching Chrome. URLs already in `history-vector' are left in place."
  (import-history-rows
   (query-rows database-path
               "SELECT h.url, MAX(h.title)
                FROM moz_places h
                JOIN moz_historyvisits v ON h.id = v.place_id
                WHERE v.visit_type <= 3 AND h.url IS NOT NULL AND h.url != ''
                GROUP BY h.url")))

(-> import-chrome-history (pathname) integer)
(defun import-chrome-history (database-path)
  "Import one history entry per URL from a Chrome History database.
Subframe, reload, and redirect visits are left out. URLs already in
`history-vector' are left in place."
  (import-history-rows
   (query-rows
    database-path
    (format nil
            "SELECT u.url, MAX(u.title)
             FROM urls u
             JOIN visits v ON u.id = v.url
             WHERE (v.transition & 255) NOT IN (3, 4, 8)
               AND (v.transition & ~d) = 0
               AND u.url IS NOT NULL AND u.url != ''
             GROUP BY u.url"
            #xC0000000))))

(-> import-time (t &key (:epoch keyword)) (or time:timestamp null))
(defun import-time (microseconds &key (epoch :unix))
  "Return MICROSECONDS since EPOCH as a timestamp.
EPOCH is :UNIX for Firefox and :WINDOWS for Chrome. Chrome stores microseconds
since 1601-01-01, often as a string."
  (let ((value (typecase microseconds
                 (real microseconds)
                 (string (handler-case (parse-integer microseconds)
                           (error () nil)))
                 (t nil))))
    (when (and (realp value) (plusp value))
      (let* ((seconds (floor value 1000000))
             (unix (ecase epoch
                     (:unix seconds)
                     (:windows (- seconds 11644473600)))))
        (when (plusp unix)
          (time:unix-to-timestamp unix))))))

(-> id-set (list) hash-table)
(defun id-set (rows)
  (let ((ids (make-hash-table)))
    (dolist (row rows)
      (setf (gethash (first row) ids) t))
    ids))

(-> annotation-ids (t string) hash-table)
(defun annotation-ids (database annotation)
  (handler-case
      (id-set (sqlite:execute-to-list
               database
               "SELECT b.item_id
                FROM moz_anno_attributes a
                JOIN moz_items_annos b ON a.id = b.anno_attribute_id
                WHERE a.name = ?"
               annotation))
    (sqlite:sqlite-error ()
      (make-hash-table))))

(-> bookmark-roots (t) list)
(defun bookmark-roots (database)
  "Return (id include-title-p) for the bookmark roots Chrome imports.
The menu root's own title is omitted. Mobile bookmarks are included."
  (loop for (guid include-title) in '(("toolbar_____" t)
                                      ("menu________" nil)
                                      ("unfiled_____" t)
                                      ("mobile______" t))
        for id = (sqlite:execute-single
                  database
                  "SELECT id FROM moz_bookmarks WHERE guid = ?"
                  guid)
        when id
          collect (list id include-title)))

(-> bookmark-nodes (t) hash-table)
(defun bookmark-nodes (database)
  (let ((nodes (make-hash-table)))
    (loop for (id type parent title url date)
            in (sqlite:execute-to-list
                database
                "SELECT b.id, b.type, b.parent,
                        COALESCE(b.title, h.title, ''),
                        IFNULL(h.url, ''),
                        IFNULL(b.dateAdded, 0)
                 FROM moz_bookmarks b
                 LEFT JOIN moz_places h ON b.fk = h.id
                 WHERE b.type IN (1, 2)")
          do (setf (gethash id nodes) (list parent type title url date)))
    nodes))

(-> bookmark-tags ((or integer null) hash-table list hash-table)
    (values list boolean &optional))
(defun bookmark-tags (parent nodes roots livemarks)
  "Walk PARENT up to a bookmark root.
Return the folder titles and T when a root is reached. A livemark, a cycle,
or a chain that never reaches a root returns NIL and NIL."
  (let ((tags '())
        (seen (make-hash-table))
        (current parent))
    (loop
      (cond
        ((or (null current) (gethash current seen))
         (return (values nil nil)))
        (t
         (setf (gethash current seen) t)
         (let ((root (assoc current roots))
               (node (gethash current nodes)))
           (cond
             ((null node)
              (return (values nil nil)))
             ((gethash current livemarks)
              (return (values nil nil)))
             (root
              (when (and (second root)
                         (stringp (third node))
                         (not (str:emptyp (third node))))
                (push (third node) tags))
              (return (values tags t)))
             (t
              (when (and (stringp (third node))
                         (not (str:emptyp (third node))))
                (push (third node) tags))
              (setf current (first node))))))))))

(-> store-bookmark (list quri:uri string (or time:timestamp null) list) list)
(defun store-bookmark (bookmarks url title date tags)
  (if (or (url-empty-p url)
          (string= "about:blank" (render-url url)))
      bookmarks
      (multiple-value-bind (entries rest)
          (sera:partition (sera:partial #'url-equal url) bookmarks :key #'url)
        (let ((entry (if entries
                         (first entries)
                         (make-instance 'nyxt/mode/bookmark:bookmark-entry
                                        :url url))))
          (unless (str:emptyp title)
            (setf (title entry) title))
          (setf (nyxt/mode/bookmark:tags entry)
                (sort (delete-duplicates
                       (remove "" tags :test #'string=)
                       :test #'string=)
                      #'string<))
          (when date
            (setf (nyxt/mode/bookmark:date entry) date))
          (push entry rest)))))

(-> import-bookmark-items (list t) integer)
(defun import-bookmark-items (items bookmarks-file)
  "Merge ITEMS into BOOKMARKS-FILE.
Each item is (url title date tags). Return how many items were merged."
  (let ((count 0))
    (files:with-file-content (bookmarks bookmarks-file)
      (unless (listp bookmarks)
        (setf bookmarks (coerce bookmarks 'list)))
      (dolist (item items)
        (setf bookmarks (apply #'store-bookmark bookmarks item))
        (incf count)))
    count))

(-> import-firefox-bookmarks (pathname t &key (:default-urls list)) integer)
(defun import-firefox-bookmarks (database-path bookmarks-file &key (default-urls '()))
  "Import Firefox bookmarks from DATABASE-PATH into BOOKMARKS-FILE.
Nested folder names are stored as tags. Default bookmarks, live bookmarks,
and POST keyword bookmarks are skipped."
  (let ((defaults (make-hash-table :test #'equal))
        (items '()))
    (dolist (url default-urls)
      (let ((parsed (importable-url url)))
        (when parsed
          (setf (gethash (render-url parsed) defaults) t))))
    (sqlite:with-open-database (database (uiop:native-namestring database-path)
                                :busy-timeout 2000)
      (let* ((nodes (bookmark-nodes database))
             (roots (bookmark-roots database))
             (livemarks (annotation-ids database "livemark/feedURI"))
             (post-keywords (annotation-ids database "bookmarkProperties/POSTData")))
        (maphash
         (lambda (id node)
           (destructuring-bind (parent type title url date) node
             (declare (ignore parent))
             (let ((parsed (importable-url url)))
               (when (and (= type 1)
                          parsed
                          (not (gethash id post-keywords))
                          (not (gethash (render-url parsed) defaults)))
                 (multiple-value-bind (tags found-p)
                     (bookmark-tags (first node) nodes roots livemarks)
                   (when found-p
                     (push (list parsed
                                 (if (stringp title) title "")
                                 (import-time date)
                                 tags)
                           items)))))))
         nodes)))
    (import-bookmark-items items bookmarks-file)))

(-> chrome-bookmark-items (t list) list)
(defun chrome-bookmark-items (node tags)
  "Return bookmark items under NODE.
Folder names, including the root folder, are stored as tags. A bookmark's own
name is its title."
  (when (hash-table-p node)
    (let* ((name (json-string node "name"))
           (type (json-string node "type"))
           (here (if (and name type (string= type "folder"))
                     (append tags (list name))
                     tags)))
      (cond
        ((and type (string= type "url"))
         (let ((url (importable-url (json-string node "url"))))
           (if url
               (list (list url
                           (or name "")
                           (import-time (j:get "date_added" node) :epoch :windows)
                           here))
               '())))
        ((and type (string= type "folder"))
         (let ((children (j:get "children" node)))
           (if (vectorp children)
               (loop for child across children
                     nconc (chrome-bookmark-items child here))
               '())))
        (t '())))))

(-> import-chrome-bookmarks (pathname t) integer)
(defun import-chrome-bookmarks (bookmarks-path bookmarks-file)
  "Import Chrome bookmarks from BOOKMARKS-PATH into BOOKMARKS-FILE.
Nested folder names are stored as tags."
  (let ((value (j:decode (uiop:read-file-string bookmarks-path :external-format :utf-8))))
    (unless (hash-table-p value)
      (error "Chrome bookmarks file ~a is not a JSON object."
             (uiop:native-namestring bookmarks-path)))
    (let ((roots (j:get "roots" value))
          (items '()))
      (when (hash-table-p roots)
        (maphash (lambda (name node)
                   (declare (ignore name))
                   (setf items (append items (chrome-bookmark-items node '()))))
                 roots))
      (import-bookmark-items items bookmarks-file))))

(-> default-firefox-bookmark-urls (t) list)
(defun default-firefox-bookmark-urls (app-directory)
  (let ((file (and app-directory
                   (uiop:merge-pathnames* "defaults/profile/bookmarks.html"
                                          (uiop:ensure-directory-pathname app-directory)))))
    (if (and file (uiop:file-exists-p file))
        (handler-case
            (with-open-file (stream file :external-format :utf-8)
              (mapcar (lambda (element) (plump:attribute element "href"))
                      (plump:get-elements-by-tag-name (plump:parse stream) "a")))
          (error () '()))
        '())))

(-> compatibility-app-directory (pathname) (or pathname null))
(defun compatibility-app-directory (profile-directory)
  (let ((file (uiop:merge-pathnames* "compatibility.ini" profile-directory)))
    (when (uiop:file-exists-p file)
      (with-open-file (stream file :external-format :utf-8)
        (loop for raw = (read-line stream nil nil)
              while raw
              for line = (string-trim '(#\Space #\Tab #\Return) raw)
              when (uiop:string-prefix-p "LastAppDir=" line)
                return (let ((value (subseq line (length "LastAppDir="))))
                         (unless (str:emptyp value)
                           (uiop:ensure-directory-pathname
                            (uiop:parse-native-namestring value)))))))))

(-> firefox-binary-directory () (or pathname null))
(defun firefox-binary-directory ()
  (when-let ((executable (sera:resolve-executable "firefox")))
    (uiop:pathname-directory-pathname
     (uiop:resolve-symlinks (uiop:ensure-pathname executable)))))

(-> firefox-app-directory (pathname) (or pathname null))
(defun firefox-app-directory (profile-directory)
  (or (compatibility-app-directory profile-directory)
      (firefox-binary-directory)))

(-> copy-import-file (pathname string pathname) t)
(defun copy-import-file (directory name destination)
  (let ((source (uiop:merge-pathnames* name directory)))
    (when (uiop:file-exists-p source)
      (uiop:copy-file source (uiop:merge-pathnames* name destination))))
  t)

(-> copy-firefox-profile (pathname pathname) t)
(defun copy-firefox-profile (profile destination)
  (dolist (name '("places.sqlite" "places.sqlite-wal" "places.sqlite-shm"
                  "places.sqlite-journal"))
    (copy-import-file profile name destination))
  t)

(-> copy-chrome-profile (pathname pathname) t)
(defun copy-chrome-profile (profile destination)
  (dolist (name '("History" "History-wal" "History-shm" "History-journal"
                  "Bookmarks"))
    (copy-import-file profile name destination))
  t)

(-> call-with-temp-directory (function) t)
(defun call-with-temp-directory (function)
  (let ((directory (uiop:ensure-directory-pathname
                    (uiop:merge-pathnames*
                     (format nil "nyxt-import-~a-~a/"
                             (get-universal-time)
                             (random 1000000))
                     (uiop:temporary-directory)))))
    (ensure-directories-exist directory)
    (unwind-protect
         (funcall function directory)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
  t)

(-> import-bookmarks-file () t)
(defun import-bookmarks-file ()
  (let ((buffer (current-buffer)))
    (unless (find-submode 'nyxt/mode/bookmark:bookmark-mode buffer)
      (enable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer))
    (nyxt/mode/bookmark:bookmarks-file buffer)))

(-> import-firefox-profile (browser-profile) t)
(defun import-firefox-profile (profile)
  (let ((source (profile-path profile))
        (app (firefox-app-directory (profile-path profile))))
    (call-with-temp-directory
     (lambda (directory)
       (copy-firefox-profile source directory)
       (let ((places (uiop:merge-pathnames* "places.sqlite" directory)))
         (if (uiop:file-exists-p places)
             (progn
               (echo "Imported ~a history entries from Firefox."
                     (handler-case (import-firefox-history places)
                       (sqlite:sqlite-error (condition)
                         (echo-warning "Could not read Firefox history: ~a" condition)
                         0)))
               (echo "Imported ~a bookmarks from Firefox."
                     (handler-case
                         (import-firefox-bookmarks
                          places
                          (import-bookmarks-file)
                          :default-urls (default-firefox-bookmark-urls app))
                       (sqlite:sqlite-error (condition)
                         (echo-warning "Could not read Firefox bookmarks: ~a" condition)
                         0))))
             (echo-warning "This Firefox profile has no places.sqlite."))))))
  t)

(-> import-chrome-profile (browser-profile) t)
(defun import-chrome-profile (profile)
  (let ((source (profile-path profile)))
    (call-with-temp-directory
     (lambda (directory)
       (copy-chrome-profile source directory)
       (let ((history (uiop:merge-pathnames* "History" directory))
             (bookmarks (uiop:merge-pathnames* "Bookmarks" directory)))
         (if (uiop:file-exists-p history)
             (echo "Imported ~a history entries from Chrome."
                   (handler-case (import-chrome-history history)
                     (sqlite:sqlite-error (condition)
                       (echo-warning "Could not read Chrome history: ~a" condition)
                       0)))
             (echo-warning "This Chrome profile has no History database."))
         (if (uiop:file-exists-p bookmarks)
             (echo "Imported ~a bookmarks from Chrome."
                   (handler-case (import-chrome-bookmarks bookmarks (import-bookmarks-file))
                     (error (condition)
                       (echo-warning "Could not read Chrome bookmarks: ~a" condition)
                       0)))
             (echo-warning "This Chrome profile has no Bookmarks file."))))))
  t)

(define-command-global import-from-firefox ()
  "Import history and bookmarks from Firefox."
  (if-let ((profile (selected-firefox-profile)))
    (import-firefox-profile profile)
    (echo-warning "No Firefox profile found.")))

(define-command-global import-from-chrome ()
  "Import history and bookmarks from Chrome."
  (if-let ((profile (selected-chrome-profile)))
    (import-chrome-profile profile)
    (echo-warning "No Chrome profile found.")))
