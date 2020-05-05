;; download-mode.lisp -- Display list of downloads.

(in-package :next)

(define-mode download-mode ()
  "Display list of downloads."
  ())

(defun download-refresh ()
  "Display a buffer listing all downloads."
  (let* ((download-buffer (or (find-buffer 'download-mode)
                              (download-mode :activate t
                                             :buffer (make-buffer :title "*Downloads*"))))
         (contents (markup:markup
                    (:h1 "Downloads")
                    (:p (:b "Directory: ") (ensure-parent-exists (download-directory *browser*)))
                    (:span              ; TODO: Do we need this span?  We need something because of the loop.
                     (loop for d in (downloads *browser*)
                           collect
                           (markup:markup
                            (:p
                             (:progress :background "red" :value (format nil "~a" (download-manager:bytes-fetched d))
                                        :max (format nil "~a" (download-manager:bytes-total d))
                                        :style (if (download-manager:finished-p d)
                                                   "border: 2px solid" "")
                                        nil)
                             ;; TODO: Print proxy in use?
                             " ("
                             (format nil "~a/s, " (sera:format-file-size-human-readable
                                                   nil
                                                   (download-manager:last-update-speed d)
                                                   :flavor :iec))
                             (sera:format-file-size-human-readable
                              nil
                              (download-manager:bytes-fetched d)
                              :flavor :iec)
                             (if (= 0 (download-manager:bytes-total d))
                                 ", out of unknown total"
                                 (format nil "/~a, ~a%"
                                         (sera:format-file-size-human-readable
                                          nil
                                          (download-manager:bytes-total d)
                                          :flavor :iec)
                                         ;; TODO: No need for percentage?
                                         (floor (* 100 (download-manager:progress d)))))
                             ") "
                             (:u (quri:render-uri (download-manager:resolved-uri d)))
                             " as "
                             (:b (file-namestring (download-manager:file d)))))))
                    (:p (:em "Open a file with " (:code (format nil "~a download-open-file" (binding-keys 'execute-command))) "."))))
         (insert-content (ps:ps (setf (ps:@ document Body |innerHTML|)
                                      (ps:lisp contents)))))
    (ffi-buffer-evaluate-javascript download-buffer insert-content)
    download-buffer))

(define-command download-list ()
  "Display a buffer listing all downloads."
  (unless (download-watcher *browser*)
    (setf (download-watcher *browser*) (bt:make-thread #'download-watch)))
 (set-current-buffer (download-refresh)))

(define-command download-url ()
  "Download the page or file of the current buffer."
  (download (url (current-buffer)))
  (unless (find-buffer 'download-mode)
    (download-list)))

(define-command download-hint-url ()
  "Download the file under the URL hinted by the user."
  (query-hints "Download link URL:" (lambda (selected-link)
                                      (download selected-link)
                                      (unless (find-buffer 'download-mode)
                                        (download-list)))))

(defun get-downloaded-filenames ()
  "Return the list of downloaded filenames of the current session, as strings."
  (mapcar #'download-manager:filename (downloads *browser*)))

(defun downloaded-files-completion-filter ()
  (let ((filenames (get-downloaded-filenames)))
    (lambda (input)
      (fuzzy-match input filenames))))

(define-command download-open-file ()
  "Open a downloaded file.
Ask the user to choose one of the downloaded files of the current session.
See also `open-file'."
  (with-result (filename (read-from-minibuffer
                          (make-minibuffer
                           :input-prompt "Open file"
                           :completion-function (downloaded-files-completion-filter))))
    (next/file-manager-mode:open-file-function filename)))
