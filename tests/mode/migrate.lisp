;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(defun write-test-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :external-format :utf-8)
    (write-string contents stream))
  path)

(define-test firefox-profile-selection ()
  (let* ((ini (uiop:ensure-directory-pathname
               (uiop:merge-pathnames*
                (format nil "nyxt-firefox-test-~a/" (random 1000000))
                (uiop:temporary-directory))))
         (path (uiop:merge-pathnames* "profiles.ini" ini)))
    (ensure-directories-exist ini)
    (unwind-protect
         (progn
           (write-test-file
            path
            "[Profile0]
Name=old-default
IsRelative=1
Path=old.default
Default=1

[Profile1]
Name=daily
IsRelative=1
Path=daily.default-release

[Profile2]
Name=absolute
IsRelative=0
Path=/tmp/absolute.firefox/

[InstallABC]
Default=daily.default-release
Locked=1
")
           (multiple-value-bind (profiles defaults)
               (nyxt/mode/migrate::profiles-in-ini path)
             (assert-eq 3 (length profiles))
             (assert-eq 1 (length defaults))
             (assert-string= "old default"
                             (nyxt/mode/migrate::profile-name (first profiles)))
             (assert-true (nyxt/mode/migrate::default-p (first profiles)))
             (let ((chosen (nyxt/mode/migrate::choose-browser-profile profiles defaults)))
               (assert-string= "daily" (nyxt/mode/migrate::profile-name chosen))
               (assert-string= (namestring (uiop:ensure-directory-pathname
                                            (uiop:merge-pathnames*
                                             "daily.default-release/" ini)))
                               (namestring (nyxt/mode/migrate::profile-path chosen))))
             (let ((absolute (third profiles)))
               (assert-string= "/tmp/absolute.firefox/"
                               (uiop:native-namestring
                                (nyxt/mode/migrate::profile-path absolute))))
             (assert-eq :choose
                        (nyxt/mode/migrate::choose-browser-profile
                         profiles
                         (list (nyxt/mode/migrate::profile-path (first profiles))
                               (nyxt/mode/migrate::profile-path (second profiles)))))
             (assert-string= "old default"
                             (nyxt/mode/migrate::profile-name
                              (nyxt/mode/migrate::choose-browser-profile profiles '())))
             (assert-eq (first profiles)
                        (nyxt/mode/migrate::choose-browser-profile
                         (list (first profiles))
                         '()))))
      (uiop:delete-directory-tree ini :validate t :if-does-not-exist :ignore))))

(defun call-with-firefox-places (thunk)
  (uiop:with-temporary-file (:pathname database)
    (sqlite:with-open-database (db (uiop:native-namestring database))
      (dolist (statement
               '("CREATE TABLE moz_places (
                    id INTEGER PRIMARY KEY,
                    url TEXT,
                    title TEXT,
                    visit_count INTEGER,
                    hidden INTEGER,
                    typed INTEGER,
                    last_visit_date INTEGER)"
                 "CREATE TABLE moz_historyvisits (
                    id INTEGER PRIMARY KEY,
                    place_id INTEGER,
                    visit_date INTEGER,
                    visit_type INTEGER)"
                 "CREATE TABLE moz_bookmarks (
                    id INTEGER PRIMARY KEY,
                    type INTEGER,
                    fk INTEGER,
                    parent INTEGER,
                    position INTEGER,
                    title TEXT,
                    dateAdded INTEGER,
                    guid TEXT)"
                 "CREATE TABLE moz_anno_attributes (
                    id INTEGER PRIMARY KEY,
                    name TEXT)"
                 "CREATE TABLE moz_items_annos (
                    id INTEGER PRIMARY KEY,
                    item_id INTEGER,
                    anno_attribute_id INTEGER)"))
        (sqlite:execute-non-query db statement))
      (dolist (row '(("https://example.com/a" "Alpha" 1 0 1)
                     ("place:foo" "Place" 1 0 0)
                     ("about:config" "About" 1 0 0)
                     ("https://example.com/redirect" "Redirect" 5 0 0)
                     ("https://example.com/both" "Both" 1 0 0)
                     ("https://example.com/both" "Both" 5 0 0)
                     ("https://hidden.example/x" "Hidden" 1 1 0)
                     ("https://example.com/menu" "Menu" 1 0 0)
                     ("https://example.com/live" "Live" 1 0 0)
                     ("https://example.com/default" "Default" 1 0 0)
                     ("https://example.com/post" "Post" 1 0 0)))
        (sqlite:execute-non-query
         db
         "INSERT INTO moz_places (url, title, visit_count, hidden, typed, last_visit_date)
          VALUES (?, ?, 1, ?, ?, 1600000000000000)"
         (first row) (second row) (fourth row) (fifth row)))
      (loop for (url title type) in '(("https://example.com/a" "Alpha" 1)
                                      ("place:foo" "Place" 1)
                                      ("about:config" "About" 1)
                                      ("https://example.com/redirect" "Redirect" 5)
                                      ("https://example.com/both" "Both" 1)
                                      ("https://example.com/both" "Both" 5)
                                      ("https://hidden.example/x" "Hidden" 1))
            for id from 1
            do (sqlite:execute-non-query
                db
                "INSERT INTO moz_historyvisits (place_id, visit_date, visit_type)
                 VALUES (?, 1600000000000000, ?)"
                id type))
      (dolist (row '((1 2 0 0 "root" 0 "root________")
                     (2 2 0 1 "menu" 0 "menu________")
                     (3 2 0 1 "toolbar" 0 "toolbar_____")
                     (4 2 0 1 "unfiled" 0 "unfiled_____")
                     (5 2 0 3 "Work" 0 "")
                     (6 1 1 5 "Example" 1600000000000000 "")
                     (7 1 8 2 "Menu Item" 1600000000000000 "")
                     (8 1 2 2 "Skip" 0 "")
                     (9 2 0 3 "Live" 0 "")
                     (10 1 9 9 "Live Item" 0 "")
                     (11 1 10 4 "Default" 0 "")
                     (12 1 11 4 "Post" 0 "")))
        (apply #'sqlite:execute-non-query
               db
               "INSERT INTO moz_bookmarks (id, type, fk, parent, position, title, dateAdded, guid)
                VALUES (?, ?, ?, ?, 0, ?, ?, ?)"
               row))
      (sqlite:execute-non-query
       db
       "INSERT INTO moz_anno_attributes (id, name) VALUES (1, 'livemark/feedURI')")
      (sqlite:execute-non-query
       db
       "INSERT INTO moz_anno_attributes (id, name) VALUES (2, 'bookmarkProperties/POSTData')")
      (sqlite:execute-non-query
       db
       "INSERT INTO moz_items_annos (item_id, anno_attribute_id) VALUES (9, 1)")
      (sqlite:execute-non-query
       db
       "INSERT INTO moz_items_annos (item_id, anno_attribute_id) VALUES (12, 2)"))
    (funcall thunk database)))

(define-test import-firefox-history-and-bookmarks ()
  (let* ((*browser* (make-instance
                     'browser
                     :history-file (make-instance
                                    'history-file
                                    :profile (make-instance 'files:virtual-profile))
                     :history-vector (make-array 1 :fill-pointer t :adjustable t
                                                   :initial-contents
                                                   (list (make-instance 'history-entry
                                                                        :url "https://example.com/a"
                                                                        :title "Kept")))))
         (bookmarks-file (make-instance 'nyxt/mode/bookmark:bookmarks-file
                                        :profile (make-instance 'files:virtual-profile))))
    (call-with-firefox-places
     (lambda (database)
       (assert-eq 2 (nyxt/mode/migrate::import-firefox-history database))
       (assert-eq 3 (length (history-vector *browser*)))
       (assert-string= "Kept" (title (aref (history-vector *browser*) 0)))
       (assert-eq 0 (nyxt/mode/migrate::import-firefox-history database))
       (assert-eq 3 (length (history-vector *browser*)))
       (let ((urls (mapcar (lambda (entry) (render-url (url entry)))
                           (coerce (history-vector *browser*) 'list))))
         (assert-true (find "https://example.com/both" urls :test #'string=))
         (assert-true (find "https://hidden.example/x" urls :test #'string=))
         (assert-false (find "place:foo" urls :test #'string=))
         (assert-false (find "about:config" urls :test #'string=))
         (assert-false (find "https://example.com/redirect" urls :test #'string=)))
       (assert-eq 2 (nyxt/mode/migrate::import-firefox-bookmarks
                     database bookmarks-file
                     :default-urls '("https://example.com/default")))
       (let ((entries (files:content bookmarks-file)))
         (assert-eq 2 (length entries))
         (let ((example (find "https://example.com/a" entries
                              :key (lambda (entry) (render-url (url entry)))
                              :test #'string=))
               (menu (find "https://example.com/menu" entries
                           :key (lambda (entry) (render-url (url entry)))
                           :test #'string=)))
           (assert-string= "Example" (title example))
           (assert-equal '("Work" "toolbar") (nyxt/mode/bookmark:tags example))
           (assert-eq 2020 (time:timestamp-year (nyxt/mode/bookmark:date example)))
           (assert-string= "Menu Item" (title menu))
           (assert-equal '() (nyxt/mode/bookmark:tags menu))))))))

(define-test importable-url ()
  (assert-false (nyxt/mode/migrate::importable-url "place:sort=1"))
  (assert-false (nyxt/mode/migrate::importable-url "about:config"))
  (assert-false (nyxt/mode/migrate::importable-url "chrome://settings"))
  (assert-false (nyxt/mode/migrate::importable-url "chrome-extension://abc/page.html"))
  (assert-true (nyxt/mode/migrate::importable-url "https://example.com/a")))

(define-test chrome-profile-selection ()
  (let* ((root (uiop:ensure-directory-pathname
                (uiop:merge-pathnames*
                 (format nil "nyxt-chrome-test-~a/" (random 1000000))
                 (uiop:temporary-directory))))
         (state (uiop:merge-pathnames* "Local State" root))
         (plain (uiop:merge-pathnames* "plain/" root)))
    (ensure-directories-exist (uiop:merge-pathnames* "Default/" root))
    (ensure-directories-exist (uiop:merge-pathnames* "Profile 1/" root))
    (ensure-directories-exist (uiop:merge-pathnames* "Guest Profile/" root))
    (ensure-directories-exist (uiop:merge-pathnames* "Default/" plain))
    (unwind-protect
         (progn
           (write-test-file
            state
            (uiop:strcat
             "{\"profile\":{\"info_cache\":{"
             "\"Default\":{\"name\":\"Person 1\",\"is_ephemeral\":false},"
             "\"Profile 1\":{\"name\":\"Work\",\"is_ephemeral\":false},"
             "\"Guest Profile\":{\"name\":\"Guest\",\"is_ephemeral\":true}},"
             "\"last_used\":\"Profile 1\","
             "\"profiles_order\":[\"Default\",\"Profile 1\"]}}"))
           (multiple-value-bind (profiles defaults)
               (nyxt/mode/migrate::profiles-in-chrome root)
             (assert-eq 2 (length profiles))
             (assert-eq 1 (length defaults))
             (assert-false (find "Guest" profiles
                                 :key #'nyxt/mode/migrate::profile-name
                                 :test #'string=))
             (let ((chosen (nyxt/mode/migrate::choose-browser-profile profiles defaults)))
               (assert-string= "Work" (nyxt/mode/migrate::profile-name chosen))
               (assert-true (search "Profile 1"
                                    (uiop:native-namestring
                                     (nyxt/mode/migrate::profile-path chosen)))))
             (assert-eq :choose
                        (nyxt/mode/migrate::choose-browser-profile profiles '())))
           (let ((ordered (uiop:merge-pathnames* "ordered/" root)))
             (ensure-directories-exist (uiop:merge-pathnames* "Default/" ordered))
             (ensure-directories-exist (uiop:merge-pathnames* "Profile 1/" ordered))
             (write-test-file
              (uiop:merge-pathnames* "Local State" ordered)
              (uiop:strcat
               "{\"profile\":{\"info_cache\":{"
               "\"Default\":{\"name\":\"Person 1\"},"
               "\"Profile 1\":{\"name\":\"Work\"}},"
               "\"profiles_order\":[\"Profile 1\",\"Default\"]}}"))
             (multiple-value-bind (profiles defaults)
                 (nyxt/mode/migrate::profiles-in-chrome ordered)
               (assert-string= "Work"
                               (nyxt/mode/migrate::profile-name
                                (nyxt/mode/migrate::choose-browser-profile
                                 profiles defaults)))))
           (write-test-file (uiop:merge-pathnames* "Default/History" plain) "")
           (multiple-value-bind (profiles defaults)
               (nyxt/mode/migrate::profiles-in-chrome plain)
             (assert-eq 1 (length profiles))
             (assert-eq 0 (length defaults))
             (assert-string= "Default"
                             (nyxt/mode/migrate::profile-name (first profiles)))
             (assert-eq (first profiles)
                        (nyxt/mode/migrate::choose-browser-profile profiles defaults))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun call-with-chrome-profile (thunk)
  (uiop:with-temporary-file (:pathname database)
    (sqlite:with-open-database (db (uiop:native-namestring database))
      (dolist (statement
               '("CREATE TABLE urls (
                    id INTEGER PRIMARY KEY,
                    url LONGVARCHAR,
                    title LONGVARCHAR,
                    visit_count INTEGER DEFAULT 0 NOT NULL,
                    typed_count INTEGER DEFAULT 0 NOT NULL,
                    last_visit_time INTEGER NOT NULL,
                    hidden INTEGER DEFAULT 0 NOT NULL)"
                 "CREATE TABLE visits (
                    id INTEGER PRIMARY KEY,
                    url INTEGER NOT NULL,
                    visit_time INTEGER NOT NULL,
                    from_visit INTEGER,
                    transition INTEGER DEFAULT 0 NOT NULL)"))
        (sqlite:execute-non-query db statement))
      (loop for (url title hidden) in '(("https://example.com/a" "Alpha" 0)
                                        ("chrome://settings" "Settings" 0)
                                        ("chrome-extension://abcdef/page.html" "Extension" 0)
                                        ("https://example.com/redirect" "Redirect" 0)
                                        ("https://example.com/frame" "Frame" 0)
                                        ("https://example.com/reload" "Reload" 0)
                                        ("https://example.com/both" "Both" 0)
                                        ("https://example.com/typed" "Typed" 0)
                                        ("https://hidden.example/x" "Hidden" 1))
            for id from 1
            do (sqlite:execute-non-query
                db
                "INSERT INTO urls
                 (id, url, title, visit_count, typed_count, last_visit_time, hidden)
                 VALUES (?, ?, ?, 1, 0, 13244473600000000, ?)"
                id url title hidden))
      (loop for (url transition) in '((1 0)
                                      (2 0)
                                      (3 0)
                                      (4 2147483648)
                                      (5 3)
                                      (6 8)
                                      (7 0)
                                      (7 2147483648)
                                      (8 1)
                                      (9 0))
            for id from 1
            do (sqlite:execute-non-query
                db
                "INSERT INTO visits (id, url, visit_time, transition)
                 VALUES (?, ?, 13244473600000000, ?)"
                id url transition)))
    (uiop:with-temporary-file (:pathname bookmarks)
      (write-test-file
       bookmarks
       (uiop:strcat
        "{\"roots\":{"
        "\"bookmark_bar\":{\"name\":\"Bookmarks bar\",\"type\":\"folder\",\"children\":["
        "{\"name\":\"Work\",\"type\":\"folder\",\"children\":["
        "{\"name\":\"Example\",\"type\":\"url\",\"url\":\"https://example.com/a\","
        "\"date_added\":\"13244473600000000\"}]},"
        "{\"name\":\"Skip\",\"type\":\"url\",\"url\":\"chrome://bookmarks/\"}]},"
        "\"other\":{\"name\":\"Other bookmarks\",\"type\":\"folder\",\"children\":["
        "{\"name\":\"Other Item\",\"type\":\"url\",\"url\":\"https://example.com/other\","
        "\"date_added\":\"13244473600000000\"}]},"
        "\"synced\":{\"name\":\"Mobile bookmarks\",\"type\":\"folder\",\"children\":[]}}}"))
      (funcall thunk database bookmarks))))

(define-test import-chrome-history-and-bookmarks ()
  (let* ((*browser* (make-instance
                     'browser
                     :history-file (make-instance
                                    'history-file
                                    :profile (make-instance 'files:virtual-profile))
                     :history-vector (make-array 1 :fill-pointer t :adjustable t
                                                   :initial-contents
                                                   (list (make-instance 'history-entry
                                                                        :url "https://example.com/a"
                                                                        :title "Kept")))))
         (bookmarks-file (make-instance 'nyxt/mode/bookmark:bookmarks-file
                                        :profile (make-instance 'files:virtual-profile))))
    (call-with-chrome-profile
     (lambda (database bookmarks)
       (assert-eq 3 (nyxt/mode/migrate::import-chrome-history database))
       (assert-eq 4 (length (history-vector *browser*)))
       (assert-string= "Kept" (title (aref (history-vector *browser*) 0)))
       (assert-eq 0 (nyxt/mode/migrate::import-chrome-history database))
       (let ((urls (mapcar (lambda (entry) (render-url (url entry)))
                           (coerce (history-vector *browser*) 'list))))
         (assert-true (find "https://example.com/both" urls :test #'string=))
         (assert-true (find "https://example.com/typed" urls :test #'string=))
         (assert-true (find "https://hidden.example/x" urls :test #'string=))
         (assert-false (find "chrome://settings" urls :test #'string=))
         (assert-false (find "chrome-extension://abcdef/page.html" urls :test #'string=))
         (assert-false (find "https://example.com/redirect" urls :test #'string=))
         (assert-false (find "https://example.com/frame" urls :test #'string=))
         (assert-false (find "https://example.com/reload" urls :test #'string=)))
       (assert-eq 2 (nyxt/mode/migrate::import-chrome-bookmarks bookmarks bookmarks-file))
       (let ((entries (files:content bookmarks-file)))
         (assert-eq 2 (length entries))
         (let ((example (find "https://example.com/a" entries
                              :key (lambda (entry) (render-url (url entry)))
                              :test #'string=))
               (other (find "https://example.com/other" entries
                            :key (lambda (entry) (render-url (url entry)))
                            :test #'string=)))
           (assert-string= "Example" (title example))
           (assert-equal '("Bookmarks bar" "Work") (nyxt/mode/bookmark:tags example))
           (assert-eq 2020 (time:timestamp-year (nyxt/mode/bookmark:date example)))
           (assert-string= "Other Item" (title other))
           (assert-equal '("Other bookmarks") (nyxt/mode/bookmark:tags other))))))))
