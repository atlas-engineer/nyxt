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
   (browser-pattern :accessor browser-pattern :initarg :browser-pattern)
   (prompter:constructor
    (lambda (source)
      (echo "Searching for history file. This may take some time.")
      (or (sera:filter #'str:non-empty-string-p
                       (str:split #\newline
                                  (nth-value 0
                                             (uiop:run-program (list "find" (uiop:native-namestring "~/")
                                                                     "-regex" (browser-pattern source))
                                                               :output :string))))
          (echo-warning "No history files found."))))))

(defmacro define-history-import-command (name docstring sql-expr file-path)
  "Shorthand to define a global command for importing history from a browser.
Make sure that the sql-expr is a SELECT statement that selects:
 - url
 - title
 - lastaccess
 - visits
Rename the columns in SELECT statement to match if necessary."
  `(define-command-global ,name ()
     ,docstring
     (let ((db-path ,file-path))
       (files:with-file-content (history (history-file (current-buffer))
                                 :default (nyxt::make-history-tree))
         (handler-bind ((dbi.error:dbi-database-error
                          (lambda (_)
                            (declare (ignore _))
                            (echo-warning "Please close the browser you wish to import history from before running this command.")
                            (invoke-restart 'abort))))
           (dbi:with-connection (conn :sqlite3 :database-name db-path)
             (let* ((query (dbi:prepare conn ,sql-expr))
                    (query (dbi:execute query)))
               (echo "Importing history from ~a." db-path)
               (loop for row = (dbi:fetch query)
                     while row
                     do (unless (url-empty-p (getf row :|url|))
                          (htree:add-entry history
                                           (make-instance 'history-entry
                                                          :url (getf row :|url|)
                                                          :title (getf row :|title|)
                                                          :implicit-visits (getf row :|visits|))
                                           (local-time:unix-to-timestamp (getf row :|lastaccess|)))))))))
       (echo "History import finished."))))

(define-history-import-command import-history-from-firefox
  "Import history from Mozilla Firefox."
  "SELECT url, title, last_visit_date/1000000 as lastaccess, visit_count as visits FROM moz_places WHERE last_visit_date not null"
  (prompt1 :prompt "Choose Mozilla Firefox places.sqlite file"
           :sources (make-instance 'external-browser-history-source
                                   :browser-pattern ".*/places\.sqlite$")))

(define-history-import-command import-history-from-google-chrome
  "Import history from Google Chrome."
  "SELECT url, title, last_visit_time/1000000-11644473600 as lastaccess, visit_count as visits FROM urls"
  (prompt1 :prompt "Choose Google Chrome History file"
           :sources (make-instance 'external-browser-history-source
                                   :browser-pattern ".*/google-chrome/Default/History$")))

(define-history-import-command import-history-from-chromium
  "Import history from Chromium."
  "SELECT url, title, last_visit_time/1000000-11644473600 as lastaccess, visit_count as visits FROM urls"
  (prompt1 :prompt "Choose Chromium History file"
           :sources (make-instance 'external-browser-history-source
                                   :browser-pattern ".*/chromium/Default/History$")))

(define-history-import-command import-history-from-brave
  "Import history from Brave."
  "SELECT url, title, last_visit_time/1000000-11644473600 as lastaccess, visit_count as visits FROM urls"
  (prompt1 :prompt "Choose Brave History file"
           :sources (make-instance 'external-browser-history-source
                                   :browser-pattern ".*/BraveSoftware/Brave-Browser/Default/History$")))

(define-history-import-command import-history-from-vivaldi
  "Import history from Vivaldi."
  "SELECT url, title, last_visit_time/1000000-11644473600 as lastaccess, visit_count as visits FROM urls"
  (prompt1 :prompt "Choose Vivaldi History file"
           :sources (make-instance 'external-browser-history-source
                                   :browser-pattern ".*/vivaldi(-snapshot)?/Default/History$")))
