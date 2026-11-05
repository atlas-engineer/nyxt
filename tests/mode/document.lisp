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
