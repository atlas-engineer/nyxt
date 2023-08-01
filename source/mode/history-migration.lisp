;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/history-migration
  (:documentation "Package for `history-migration-mode', mode to import history from other
browsers."))
(in-package :nyxt/mode/history-migration)

(define-mode history-migration-mode ()
  "Mode for importing history from other browsers."
  ((visible-in-status-p nil)
   (rememberable-p nil))
  (:toggler-command-p nil))

(define-class external-browser-history-source (prompter:source)
  ((prompter:name "History files")
   (browser-lambda :accessor browser-lambda :initarg :browser-lambda)
   (prompter:constructor
    (lambda (source)
      (echo "Searching for history file. This may take some time.")
      (or (let ((results '()))
            (uiop:collect-sub*directories
             (user-homedir-pathname)
             (constantly t)
             (lambda (subdir)
               (equal (iolib/os:file-kind subdir) :directory))
             (lambda (subdir)
               (let ((browser-history-file (funcall* (browser-lambda source) subdir)))
                 (when browser-history-file
                   (push browser-history-file results)))))
            results)
          (echo-warning "No history files found."))))))

(defmacro define-history-import-command (name docstring &key sql-query file-path)
  "Shorthand to define a global command for importing history from a browser.
Make sure that the sql-query is a SELECT statement that selects:
 - url
 - title
 - last-access (unix time in seconds)
 - visits
Or the equivalent columns for the browser in question."
  `(define-command-global ,name ()
     ,docstring
     (let ((db-path ,file-path))
       (files:with-file-content (history (history-file (current-buffer))
                                 :default (nyxt::make-history-tree))
         (handler-bind ((sqlite:sqlite-error
                          (lambda (_)
                            (declare (ignore _))
                            (echo-warning "Please close the browser you wish to import history from before running this command.")
                            (invoke-restart 'abort))))
           (sqlite:with-open-database (db db-path)
             (echo "Importing history from ~a." db-path)
             (loop for (url title last-access visits) in (sqlite:execute-to-list db ,sql-query)
                   do (unless (url-empty-p url)
                        (htree:add-entry history
                                         (make-instance 'history-entry
                                                        :url url
                                                        :title title
                                                        :implicit-visits visits)
                                         (local-time:unix-to-timestamp last-access))))
             (echo "History import finished.")))))))

(define-history-import-command import-history-from-firefox
  "Import history from Mozilla Firefox."
  :sql-query "SELECT url, title, last_visit_date/1000000, visit_count FROM moz_places WHERE last_visit_date not null"
  :file-path (prompt1 :prompt "Choose Mozilla Firefox places.sqlite file"
                      :sources (make-instance 'external-browser-history-source
                                              :browser-lambda (lambda (subdir)
                                                                (when (uiop:file-exists-p (uiop:merge-pathnames* "places.sqlite" subdir))
                                                                  (uiop:merge-pathnames* "places.sqlite" subdir))))))

(define-history-import-command import-history-from-google-chrome
  "Import history from Google Chrome."
  :sql-query "SELECT url, title, last_visit_time/1000000-11644473600, visit_count FROM urls"
  :file-path (prompt1 :prompt "Choose Google Chrome History file"
                      :sources (make-instance 'external-browser-history-source
                                              :browser-lambda (lambda (subdir)
                                                                (when (and (string= "google-chrome"
                                                                                    (nfiles:basename (nfiles:parent subdir)))
                                                                           (uiop:file-exists-p (uiop:merge-pathnames* "History" subdir)))
                                                                  (uiop:merge-pathnames* "History" subdir))))))

(define-history-import-command import-history-from-chromium
  "Import history from Chromium."
  :sql-query "SELECT url, title, last_visit_time/1000000-11644473600, visit_count FROM urls"
  :file-path (prompt1 :prompt "Choose Chromium History file"
                      :sources (make-instance 'external-browser-history-source
                                              :browser-lambda (lambda (subdir)
                                                                (when (and (string= "chromium"
                                                                                    (nfiles:basename (nfiles:parent subdir)))
                                                                           (uiop:file-exists-p (uiop:merge-pathnames* "History" subdir)))
                                                                  (uiop:merge-pathnames* "History" subdir))))))

(define-history-import-command import-history-from-brave
  "Import history from Brave."
  :sql-query "SELECT url, title, last_visit_time/1000000-11644473600, visit_count FROM urls"
  :file-path (prompt1 :prompt "Choose Brave History file"
                      :sources (make-instance 'external-browser-history-source
                                              :browser-lambda (lambda (subdir)
                                                                (when (and (string= "Brave-Browser"
                                                                                    (nfiles:basename (nfiles:parent subdir)))
                                                                           (uiop:file-exists-p (uiop:merge-pathnames* "History" subdir)))
                                                                  (uiop:merge-pathnames* "History" subdir))))))

(define-history-import-command import-history-from-vivaldi
  "Import history from Vivaldi."
  :sql-query "SELECT url, title, last_visit_time/1000000-11644473600, visit_count FROM urls"
  :file-path (prompt1 :prompt "Choose Vivaldi History file"
                      :sources (make-instance 'external-browser-history-source
                                              :browser-lambda (lambda (subdir)
                                                                (when (and (str:starts-with-p "vivaldi"
                                                                                              (nfiles:basename (nfiles:parent subdir)))
                                                                           (uiop:file-exists-p (uiop:merge-pathnames* "History" subdir)))
                                                                  (uiop:merge-pathnames* "History" subdir))))))
