;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test heading-section-text-stops-at-the-next-heading ()
  (let* ((root (plump:parse "<h1>Intro</h1><p>Welcome to Nyxt.</p>
<h2>Install</h2><p>Download the package.</p><script>secretToken</script><p>Run it.</p>
<div><h3>Linux</h3><p>Use the package manager.</p></div>
<p>Then reboot.</p>
<h2>Usage</h2><p>Open a buffer.</p>"))
         (headings (coerce (clss:ordered-select "h1, h2, h3, h4, h5, h6" root) 'list))
         (texts (loop for (heading . rest) on headings
                      collect (nyxt/mode/document::heading-section-text heading (first rest)))))
    (assert-equal 4 (length texts))
    (assert-equal "Welcome to Nyxt." (first texts))
    (assert-equal "Download the package. Run it." (second texts))
    (assert-equal "Use the package manager. Then reboot." (third texts))
    (assert-equal "Open a buffer." (fourth texts))
    (assert-false (search "secretToken" (second texts)))
    (assert-false (search "reboot" (second texts)))))

(define-test heading-section-text-crosses-wrappers ()
  (let* ((root (plump:parse "<div><h2>Install</h2><p>Download the package.</p></div>
<p>Then reboot.</p><h2>Usage</h2><p>Open a buffer.</p>"))
         (headings (coerce (clss:ordered-select "h1, h2, h3, h4, h5, h6" root) 'list))
         (texts (loop for (heading . rest) on headings
                      collect (nyxt/mode/document::heading-section-text heading (first rest)))))
    (assert-equal "Download the package. Then reboot." (first texts))
    (assert-equal "Open a buffer." (second texts))))

(define-test heading-keywords-describe-that-section ()
  (let* ((root (plump:parse "<h1>Intro</h1><p>Welcome to Nyxt.</p>
<h2>Install</h2><p>Download the package.</p><script>secretToken</script><p>Run it.</p>
<h2>Usage</h2><p>Open a buffer.</p>"))
         (headings (coerce (clss:ordered-select "h1, h2, h3, h4, h5, h6" root) 'list))
         (keywords (nyxt/mode/document::heading-keyword-lists headings))
         (words (lambda (index) (mapcar #'car (elt keywords index)))))
    (assert-equal keywords (nyxt/mode/document::heading-keyword-lists headings))
    (assert-true (member "nyxt" (funcall words 0) :test #'string=))
    (assert-false (member "package" (funcall words 0) :test #'string=))
    (assert-true (member "package" (funcall words 1) :test #'string=))
    (assert-false (search "secret" (format nil "~:{~a~^ ~}" (elt keywords 1))))
    (assert-true (member "buffer" (funcall words 2) :test #'string=))
    (assert-false (member "buffer" (funcall words 1) :test #'string=))))

(define-test toggle-document-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/document:document-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/document:document-mode buffer)))))

(define-test zoom-host-follows-the-site ()
  ;; Firefox keys site zoom by the full hostname, ignoring scheme and port.
  (assert-string= "www.example.com"
                  (zoom-host "https://www.example.com/a"))
  (assert-string= "www.example.com"
                  (zoom-host "http://WWW.Example.COM:8080/b?q=1#here"))
  (assert-string= "docs.example.com"
                  (zoom-host "https://docs.example.com/guide"))
  (assert-string= "127.0.0.1"
                  (zoom-host "http://127.0.0.1/local"))
  (assert-false (zoom-host "nyxt:new"))
  (assert-false (zoom-host "about:blank"))
  (assert-false (zoom-host "file:///tmp/notes.html")))

(define-test zoom-is-remembered-for-the-host ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'document-buffer :no-hook-p t))
         (other-browser (make-instance 'browser)))
    (setf (zoom-ratio-default buffer) 1.25)
    (remember-zoom "https://www.example.com/a" 1.5 :persist-p nil)
    (assert-equality #'=
                     1.5
                     (remembered-zoom "http://www.example.com/other"))
    (assert-false (remembered-zoom "https://docs.example.com/"))
    (assert-equality #'=
                     1.5
                     (zoom-ratio-for-url "https://www.example.com/z" buffer))
    (assert-equality #'=
                     1.25
                     (zoom-ratio-for-url "https://docs.example.com/" buffer))
    ;; A zoom left on the buffer must not follow the user onto another host.
    (setf (slot-value buffer 'zoom-ratio) 2.0)
    (assert-equality #'=
                     1.25
                     (zoom-ratio-for-url "https://docs.example.com/" buffer))
    (assert-equality #'=
                     2.0
                     (zoom-ratio-for-url (nyxt-url 'new) buffer))
    (let ((*browser* other-browser))
      (assert-false (remembered-zoom "https://www.example.com/a")))
    (remember-zoom "https://www.example.com/a" nil :persist-p nil)
    (assert-false (remembered-zoom "https://www.example.com/a"))
    (assert-equality #'=
                     1.25
                     (zoom-ratio-for-url "https://www.example.com/a" buffer))
    (remember-zoom "https://www.example.com/" 0 :persist-p nil)
    (assert-false (remembered-zoom "https://www.example.com/"))))

(define-test zoom-ratios-restore-from-entries ()
  (let ((table (make-hash-table :test 'equal)))
    (nyxt::replace-zoom-ratios
     table
     '(("WWW.Example.COM" . 1.5)
       ("nyxt.atlas.engineer" . 0)
       ("not-a-zoom")
       (12 . 1.2)))
    (assert-equality #'= 1.5 (gethash "www.example.com" table))
    (assert-false (gethash "nyxt.atlas.engineer" table))
    (assert-eq 1 (hash-table-count table))))
