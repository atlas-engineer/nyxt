;; download-mode.lisp -- Display list of downloads.

(in-package :next)

(define-mode download-mode ()
    "Display list of downloads."
    ())

(defun download-refresh (&optional (interface *interface*))
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
                            (:p
                             (:progress :background "red" :value (format nil "~a" (download-manager:downloaded-bytes d))
                                        :max (format nil "~a" (download-manager:total-bytes d))
                                        :style (if (download-manager:finished-p d)
                                                   "border: 2px solid" "")
                                        nil)
                             (format nil " (~a% of ~,,' :d bytes) "
                                     (floor (* 100 (download-manager:progress d)))
                                     ;; TODO: Print human size?
                                     (download-manager:total-bytes d))
                             (:u (quri:render-uri (download-manager:resolved-uri d)))
                             " as "
                             (:b (file-namestring (download-manager:file d)))))))))
         (insert-content (ps:ps (setf (ps:@ document Body |innerHTML|)
                                      (ps:lisp contents)))))
    (%%buffer-evaluate-javascript interface download-buffer insert-content)
    download-buffer))

(define-command download-list (root-mode &optional (interface *interface*))
  "Display a buffer listing all downloads."
  (set-active-buffer interface (download-refresh)))
