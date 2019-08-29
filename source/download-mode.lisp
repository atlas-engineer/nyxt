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
                    (:p (:b "Directory: ") (namestring (or (download-directory *interface*)
                                                           (download-manager:default-download-directory))))
                    (:span              ; TODO: Do we need this span?  We need something because of the loop.
                     (loop for d in (downloads interface)
                           collect
                           (cl-markup:markup
                            (:p
                             (:progress :background "red" :value (format nil "~a" (download-manager:bytes-fetched d))
                                        :max (format nil "~a" (download-manager:bytes-total d))
                                        :style (if (download-manager:finished-p d)
                                                   "border: 2px solid" "")
                                        nil)
                             ;; TODO: Print proxy in use?
                             " ("
                             (format nil "~a/s, " (file-size-human-readable
                                                   (download-manager:last-update-speed d)
                                                   'iec))
                             (format nil "~a" (file-size-human-readable
                                               (download-manager:bytes-fetched d)
                                               'iec))
                             (if (= 0 (download-manager:bytes-total d))
                                 ", out of unknown total"
                                 (format nil "/~a, ~a%"
                                         (file-size-human-readable
                                          (download-manager:bytes-total d)
                                          'iec)
                                         ;; TODO: No need for percentage?
                                         (floor (* 100 (download-manager:progress d)))))
                             ") "
                             (:u (quri:render-uri (download-manager:resolved-uri d)))
                             " as "
                             (:b (file-namestring (download-manager:file d)))))))
                    (:p (:em "Open a file with " (:code "M-x download-open-file") "."))))
         (insert-content (ps:ps (setf (ps:@ document Body |innerHTML|)
                                      (ps:lisp contents)))))
    (rpc-buffer-evaluate-javascript interface download-buffer insert-content)
    download-buffer))

(define-command download-list (&optional (interface *interface*))
  "Display a buffer listing all downloads."
  (unless (download-watcher interface)
    (setf (download-watcher interface) (bt:make-thread #'download-watch)))
  (set-active-buffer interface (download-refresh)))

(define-command download-url (&optional (interface *interface*))
  "Download the page or file of the current buffer."
  (with-result (url (buffer-get-url))
    (download interface url)
    (unless (find-buffer 'download-mode)
      (download-list))))

(define-command download-hint-url (&optional (interface *interface*))
  "Download the file under the URL hinted by the user."
  (query-hints "Download link URL:" (selected-link)
    (download interface selected-link)
    (unless (find-buffer 'download-mode)
      (download-list))))

(defun get-downloaded-filenames (interface)
  "Return the list of downloaded filenames of the current session, as strings."
  (mapcar #'download-manager:filename (downloads interface)))

(defun downloaded-files-completion-fn (interface)
  (let ((filenames (get-downloaded-filenames interface)))
    (lambda (input)
      (fuzzy-match input filenames))))

(define-command download-open-file (&optional (interface *interface*))
  "Open a downloaded file.
Ask the user to choose one of the downloaded files of the current session.
See also `open-file'."
  (with-result (filename (read-from-minibuffer
                          (minibuffer *interface*)
                          :input-prompt "Open file:"
                          :completion-function (downloaded-files-completion-fn interface)))
    (open-file-function filename)))
