;; download-mode.lisp -- Display list of downloads.

(in-package :next)

(define-mode download-mode ()
    "Display list of downloads."
    ())

(define-command download-list (root-mode &optional (interface *interface*))
  "Display a buffer listing all downloads."
  (let* ((download-buffer (or (find-buffer 'download-mode)
                              (make-buffer
                               :name "*Downloads*"
                               :default-modes (cons 'download-mode
                                                    (get-default 'buffer 'default-modes)))))
         (contents (cl-markup:markup
                    (:h1 "Downloads")
                    (:span              ; TODO: Do we need this span?
                     (loop for d in (downloads interface)
                           collect
                           (cl-markup:markup
                            (:p (format nil "~a (~a bytes, ~a%) as ~a"
                                        (quri:render-uri (download-manager:resolved-uri d))
                                        (download-manager:total-bytes d) ; TODO: Print human size.
                                        (floor (* 100 (download-manager:progress d)))
                                        (download-manager:file d))))))))
         (insert-content (ps:ps (setf (ps:@ document Body |innerHTML|)
                                      (ps:lisp contents)))))
    (%%buffer-evaluate-javascript *interface* download-buffer insert-content)
    (set-active-buffer *interface* download-buffer)))
