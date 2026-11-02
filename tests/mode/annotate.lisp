;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-annotate-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/annotate:annotate-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/annotate:annotate-mode buffer)))))

(define-test save-and-remove-annotation ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'modable-buffer))
         (file (make-instance 'nyxt/mode/annotate:annotations-file
                              :profile (make-instance 'files:virtual-profile)))
         (date (time:now))
         (kept (make-instance 'nyxt/mode/annotate:url-annotation
                              :url (quri:uri "https://example.com/kept")
                              :page-title "Kept"
                              :data "keep"
                              :tags '("old")
                              :date date))
         (dropped (make-instance 'nyxt/mode/annotate:snippet-annotation
                                 :url (quri:uri "https://example.com/dropped")
                                 :page-title "Dropped"
                                 :snippet "passage"
                                 :data "drop"
                                 :tags '("tag")
                                 :date date)))
    (with-current-buffer buffer
      (enable-modes* 'nyxt/mode/annotate:annotate-mode buffer)
      (setf (nyxt/mode/annotate:annotations-file
             (find-submode 'nyxt/mode/annotate:annotate-mode buffer))
            file)
      (nyxt/mode/annotate::annotation-add kept)
      (nyxt/mode/annotate::annotation-add dropped)
      (nyxt/mode/annotate::save-annotation kept :data "kept" :tags '("" "zeta" "tag" "tag"))
      (let ((entries (files:content file)))
        (assert-eq 2 (length entries))
        (assert-string= "kept" (nyxt/mode/annotate::data kept))
        (assert-equal '("zeta" "tag") (nyxt/mode/annotate:tags kept)))
      (nyxt/mode/annotate::remove-annotation
       (make-instance 'nyxt/mode/annotate:snippet-annotation
                      :url (quri:uri "https://example.com/dropped")
                      :page-title "Dropped"
                      :snippet "passage"
                      :data "drop"
                      :tags '("tag")
                      :date date))
      (let ((entries (files:content file)))
        (assert-eq 1 (length entries))
        (assert-eq kept (first entries)))
      (nyxt/mode/annotate::remove-annotation kept)
      (assert-eq 0 (length (or (files:content file) '()))))))
