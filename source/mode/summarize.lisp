(in-package :nyxt/web-mode)

(nyxt::define-internal-page-command summarize-buffer (&key (summary-length 5) (buffer (current-buffer)))
  (output (format nil "*Summary ~a*" (title buffer)) 'base-mode)
  "Summarize the current buffer by creating a new summary buffer."
  (let ((contents
          (serapeum:string-join
           (map 'list (lambda (e) (plump:text e))
                (clss:select "p" (document-model buffer)))
           " ")))
    (spinneret:with-html-string
      (:style (style output))
      (:h1 "Summary for: " (title buffer))
      (:ul
       (loop for point in (analysis:summarize-text contents :summary-length summary-length)
             collect (:li point))))))
