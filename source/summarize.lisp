(in-package :nyxt/web-mode)

(define-command summarize-buffer (&key (summary-length 5) (buffer (current-buffer)))
  "Summarize the current buffer by creating a new summary buffer."
  (with-current-html-buffer (output (format nil "*Summary ~a*" (title buffer)) 'base-mode)
    (let ((contents
            (serapeum:string-join
             (map 'list (lambda (e) (plump:text e))
                  (clss:select "p" (document-model (current-mode 'web))))
             " ")))
      (spinneret:with-html-string
        (:style (style output))
        (:h1 "Summary for: " (title buffer))
        (:ul
         (loop for point in (analysis:summarize-text contents :summary-length summary-length)
               collect (:li point)))))))
