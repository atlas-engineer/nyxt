(in-package :nyxt/web-mode)

(define-internal-page-command-global summarize-buffer (&key (summary-length 5) (id (id (current-buffer))))
  (output (format nil "*Summary ~a*" (title (nyxt::buffers-get id))) 'base-mode)
  "Summarize the current buffer by creating a new summary buffer."
  (let ((buffer (nyxt::buffers-get id)))
    (let ((contents
            (serapeum:string-join
             (map 'list (lambda (e) (plump:text e))
                  (clss:select "p" (document-model buffer)))
             " ")))
      (spinneret:with-html-string
        (:nstyle (style output))
        (:h1 "Summary for: " (title buffer))
        (:ul
         (loop for point in (analysis:summarize-text contents :summary-length summary-length)
               collect (:li point)))))))
