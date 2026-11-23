;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test history-entry-url-and-equality ()
  (let ((from-string (make-instance 'history-entry :url "https://example.com/a"))
        (from-uri (make-instance 'history-entry :url (quri:uri "https://example.com/a")))
        (other (make-instance 'history-entry :url "https://example.com/b")))
    (assert-true (quri:uri-p (url from-string)))
    (assert-equality #'quri:uri=
                     (quri:uri "https://example.com/a")
                     (url from-string))
    (assert-true (equals from-string from-uri))
    (assert-false (equals from-string other))))

(define-test serialize-history-entry-skips-empty-url ()
  (assert-string= ""
                  (with-output-to-string (out)
                    (nyxt::serialize-history-entry (make-instance 'history-entry) out))))

(define-test history-entry-round-trip ()
  (let* ((entry (make-instance 'history-entry
                               :url "https://example.com/a"
                               :title "Example"))
         (text (with-output-to-string (out)
                 (nyxt::serialize-history-entry entry out))))
    (with-input-from-string (in (format nil "(~a)" text))
      (let ((entries (files:deserialize (make-instance 'nyxt-profile)
                                        (make-instance 'history-file)
                                        in)))
        (assert-eq 1 (length entries))
        (assert-equality #'quri:uri=
                         (quri:uri "https://example.com/a")
                         (url (first entries)))
        (assert-string= "Example" (title (first entries)))))))

(define-test recent-distinct-history-entries-looks-past-duplicates ()
  (let* ((browser (make-instance 'browser))
         (entries (list
                   (make-instance 'history-entry :url "https://b.example" :title "b")
                   (make-instance 'history-entry :url "https://a.example" :title "a1")
                   (make-instance 'history-entry :url "https://a.example" :title "a2")
                   (make-instance 'history-entry :url "https://a.example" :title "a3"))))
    (setf (history-vector browser)
          (make-array 4 :fill-pointer t :adjustable t
                        :initial-contents entries))
    (let ((recent (recent-distinct-history-entries 2 browser)))
      (assert-eq 2 (length recent))
      (assert-string= "a3" (title (first recent)))
      (assert-string= "b" (title (second recent))))))

(define-test recent-history-entry-order ()
  (let* ((browser (make-instance 'browser))
         (older (make-instance 'history-entry :url "https://a.example" :title "a"))
         (middle (make-instance 'history-entry :url "https://b.example" :title "b"))
         (newer (make-instance 'history-entry :url "https://a.example" :title "a2")))
    (setf (history-vector browser)
          (make-array 3 :fill-pointer t :adjustable t
                        :initial-contents (list older middle newer)))
    (let ((recent (recent-history-entries 2 browser)))
      (assert-eq 2 (length recent))
      (assert-string= "a2" (title (first recent)))
      (assert-string= "b" (title (second recent))))
    (let ((deduped (recent-history-entries 3 browser :deduplicate-p t)))
      ;; `remove-duplicates' on the history vector keeps the later visit.
      ;; The result is then newest-first.
      (assert-eq 2 (length deduped))
      (assert-string= "a2" (title (first deduped)))
      (assert-string= "b" (title (second deduped))))))

(define-test history-deserialization-resilience ()
  (with-input-from-string (empty "")
    (assert-false (files:deserialize (make-instance 'nyxt-profile)
                                     (make-instance 'history-file)
                                     empty)))
  (with-input-from-string
      (legacy "((:url \"https://good.example/\" :title \"Good\" :implicit-visits 4)
                (:url \"http://[::1]:bad\" :title \"Broken\")
                (:url \"https://second.example/\" :title \"Second\"))")
    (let ((entries (files:deserialize (make-instance 'nyxt-profile)
                                      (make-instance 'history-file)
                                      legacy)))
      (assert-eql 2 (length entries))
      (assert-string= "Good" (title (first entries)))
      (assert-string= "Second" (title (second entries))))))

(define-test history-persistence-and-corrupt-backup ()
  (let* ((old-data (uiop:getenv "XDG_DATA_HOME"))
         (old-browser *browser*)
         (old-repl nyxt::*run-from-repl-p*)
         (tmp-dir (uiop:ensure-directory-pathname
                   (format nil "/tmp/nyxt-test-hist-~a/" (get-universal-time)))))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf (uiop:getenv "XDG_DATA_HOME") (uiop:native-namestring tmp-dir)
                 nyxt::*run-from-repl-p* nil)
           (nfiles::clear-cache)
           (setf *browser* (make-instance 'browser))
           (let ((hpath (files:expand (history-file *browser*))))
             (ensure-directories-exist hpath)
             (with-open-file (out hpath :direction :output :if-exists :supersede)
               (format out "(~%")
               (dotimes (i 20)
                 (format out "(:url \"https://seed-~d.example/\" :title \"Seed ~d\")~%" i i))
               (format out ")~%")))
           (nfiles::clear-cache)
           (ensure-history-loaded *browser*)
           (let* ((buf (make-instance 'modable-buffer))
                  (mode (make-instance 'nyxt/mode/history:history-mode :buffer buf))
                  (threads '()))
             (dotimes (w 4)
               (let ((w w))
                 (push (bt:make-thread
                        (lambda ()
                          (dotimes (j 10)
                            (nyxt/mode/history::add-url-to-history
                             (quri:uri (format nil "https://conc-~d-~d.example/" w j))
                             mode
                             :title (format nil "C ~d ~d" w j)))))
                       threads)))
             (mapc #'bt:join-thread threads)
             (files:join-workers)
             (assert-eql 60 (length (history-vector *browser*)))
             (nfiles::clear-cache)
             (assert-eql 60 (length (files:content (make-instance 'history-file)))))
           (let* ((bfile (make-instance 'nyxt/mode/bookmark:bookmarks-file))
                  (bpath (files:expand bfile)))
             (ensure-directories-exist bpath)
             (alex:write-string-into-file "((:url \"https://keep.example/\" BROKEN"
                                          bpath :if-exists :supersede)
             (nfiles::clear-cache)
             (assert-false (files:content bfile))
             (assert-true
              (find-if (lambda (p) (search "-backup-" (namestring p)))
                       (uiop:directory-files (uiop:pathname-directory-pathname bpath))))))
      (setf *browser* old-browser
            nyxt::*run-from-repl-p* old-repl)
      (if old-data
          (setf (uiop:getenv "XDG_DATA_HOME") old-data)
          (setf (uiop:getenv "XDG_DATA_HOME") ""))
      (nfiles::clear-cache)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t :if-does-not-exist :ignore)))))
