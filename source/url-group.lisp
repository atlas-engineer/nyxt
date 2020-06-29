(in-package :nyxt)

;;; url-group.lisp: provide a macro that the user can use to define
;;; groups of URLs to open by a shortcut command. For example, the
;;; user may define: (define-url-group xyz ("abc.com" "xyz.com"))
;;; Then, when the user invokes the command open-group-xyz, Nyxt will
;;; make two new buffers with abc.com and xyz.com

(export-always 'define-url-group)
(defmacro define-url-group (name urls)
  "Define a URL group to be opened by a command. URL Groups are
shortcuts to open up a group of URLs in a set of new buffers."
  (let ((name (intern (str:concat "OPEN-GROUP-" (symbol-name name)))))
    `(define-command ,name ()
       "Open a group of URLs as specified by define-url-group."
       (loop for url in ,urls do
                (make-buffer :url url)))))


