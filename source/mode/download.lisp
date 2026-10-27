;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/download
  (:documentation "Package for `download-mode', mode to manage downloads and the
downloads page."))
(in-package :nyxt/mode/download)

(export-always 'renderer-download)
(defclass renderer-download ()
  ()
  (:metaclass interface-class)
  (:documentation "A basis for renderer-specific download objects.
Should be redefined by the renderer."))

(define-class download (renderer-download)
  ((url
    (error "URL required.")
    :documentation "A string representation of the URL.")
   (status
    :unloaded
    :export t
    :reader status
    :type (member :unloaded :loading :finished :failed :canceled)
    :documentation "Status of the download.")
   (status-text
    (make-instance 'user-interface:paragraph)
    :export nil)
   (completion-percentage
    0.0
    :reader t
    :export t
    :type float
    :documentation "The download complete percentage.")
   (bytes-downloaded
    "-"
    :reader t
    :export t
    :documentation "The number of bytes downloaded.")
   (bytes-text
    (make-instance 'user-interface:paragraph)
    :export nil
    :documentation "The interface element that shows `bytes-downloaded'.")
   (destination-path
    #p""
    :export t
    :type pathname
    :documentation "The path where the download is written to.")
   (before-download-hook
    (make-instance 'hook-download)
    :type hook-download
    :documentation "Hook run before downloading a file.
The handlers take the `download' instance as argument.

Example: echo the file name to be downloaded

\(define-configuration nyxt/mode/download:download
  ((nyxt/mode/download:before-download-hook
    (hooks:add-hook %slot-value%
      (make-instance 'hooks:handler
                     :fn (lambda (download)
                           (echo \"The URL for the download is ~a\"
                             (render-url (url download))))
                     :name 'echo-name)))))")
   (after-download-hook
    (make-instance 'hook-download)
    :type hook-download
    :documentation "Hook run after a download has completed.
The handlers take the `download' instance as argument.

Example: open the loaded files with XDG-open
\(define-configuration nyxt/mode/download:download
  ((nyxt/mode/download:after-download-hook
    (hooks:add-hook
     %slot-value%
     (make-instance
       'hooks:handler
       :fn (lambda (download)
             (uiop:launch-program
              (list \"xdg-open\"
                 (uiop:native-namestring
                   (nyxt/mode/download:destination-path download)))))
       :name 'xdg-open-download)))))")
   (cancel-function
    nil
    :export t
    :type (or null function)
    :documentation "The function that cancels the download.
It can be set by the download engine.")
   (progress-text
    (make-instance 'user-interface:paragraph)
    :export nil)
   (progress
    (make-instance 'user-interface:progress-bar)
    :export nil))
  (:metaclass user-class)
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:documentation "This class is used to represent a download.
The `downloads' slot is populated by a list of these objects."))

(hooks:define-hook-type download (function (download))
  "Hook acting on `download' objects.")

(defmethod initialize-instance :after ((download download)
                                       &key &allow-other-keys)
  (hooks:run-hook (before-download-hook download) download)
  (list-downloads))

(defmethod cancel-download ((download download))
  "Call `cancel-function' with URL as argument."
  (funcall (cancel-function download))
  (echo "Download canceled: ~a." (url download))
  (buffer-load-internal-page-focus 'list-downloads))

(-> format-unit (real string) (values string &optional))
(defun format-unit (value unit)
  "Return VALUE with UNIT, dropping a trailing \".0\"."
  (let ((text (format nil "~,1f" value)))
    (format nil "~a ~a"
            (if (str:ends-with-p ".0" text)
                (format nil "~d" (round value))
                text)
            unit)))

(-> format-bytes (real) (values string &optional))
(defun format-bytes (bytes)
  "Return BYTES as a short size such as \"1.2 MB\"."
  (let ((bytes (max 0 (round bytes))))
    (cond
      ((< bytes 1024)
       (format nil "~d B" bytes))
      ((< bytes (expt 1024 2))
       (format-unit (/ bytes 1024.0) "KB"))
      ((< bytes (expt 1024 3))
       (format-unit (/ bytes (expt 1024.0 2)) "MB"))
      (t
       (format-unit (/ bytes (expt 1024.0 3)) "GB")))))

(-> download-filename (download) (values string &optional))
(defun download-filename (download)
  "Return the file name DOWNLOAD is saved as.
The URL's last segment is used when the destination is not known yet."
  (let ((filename (file-namestring (destination-path download))))
    (if (not (str:emptyp filename))
        filename
        (or (ignore-errors
              (let ((base (file-namestring
                           (or (quri:uri-path (quri:uri (url download))) ""))))
                (unless (str:emptyp base)
                  base)))
            (let ((url (url download)))
              (etypecase url
                (string url)
                (quri:uri (quri:render-uri url))))))))

(-> download-summary (download) (values string &optional))
(defun download-summary (download)
  "Return the short line under DOWNLOAD's name.
Progress while it is running, the destination once the file is ready,
or the status word when it failed or was canceled."
  (ecase (status download)
    (:finished
     (let ((path (uiop:native-namestring (destination-path download))))
       (if (str:emptyp path)
           "Finished"
           path)))
    (:failed "Failed")
    (:canceled "Canceled")
    ((:loading :unloaded)
     (let* ((percentage (completion-percentage download))
            (percent (when (and (realp percentage)
                                (plusp percentage)
                                (<= percentage 100))
                       (format nil "~d%" (round percentage))))
            (bytes (bytes-downloaded download))
            (size (when (and (realp bytes) (plusp bytes))
                    (format-bytes bytes)))
            (parts (remove nil (list percent size))))
       (if parts
           (str:join " · " parts)
           "Starting")))))

(-> refresh-download-summary (download) (values string &optional))
(defun refresh-download-summary (download)
  "Update the line under DOWNLOAD's name from its current state."
  (setf (user-interface:text (status-text download))
        (download-summary download)))

(defmethod (setf status) (value (download download))
  (setf (slot-value download 'status) value)
  (refresh-download-summary download)
  ;; Refresh the downloads page and show it when the file is ready.
  (when (eq value :finished)
    (buffer-load-internal-page-focus 'list-downloads)
    (hooks:run-hook (after-download-hook download) download)))

(defmethod (setf completion-percentage) (percentage (download download))
  (setf (slot-value download 'completion-percentage) percentage)
  (setf (user-interface:percentage (progress download))
        (if (and (realp percentage) (<= 0 percentage 100))
            percentage
            0))
  (refresh-download-summary download))

(defmethod (setf bytes-downloaded) (bytes (download download))
  (setf (slot-value download 'bytes-downloaded) bytes)
  (refresh-download-summary download))

(defmethod connect ((download download) buffer)
  "Connect DOWNLOAD's summary and progress bar to BUFFER.
Later updates of those values redraw on BUFFER."
  (user-interface:connect (status-text download) buffer)
  (user-interface:connect (progress download) buffer))

(define-mode download-mode ()
  "Display list of downloads."
  ()
  (:toggler-command-p nil))

(define-internal-page-command-global list-downloads ()
    (buffer "*Downloads*" 'download-mode)
  "Display a list of downloads for this session."
  ;; One list, the same rows as history: the file name over one short line.
  ;; That line is the progress, or the destination path when the file is
  ;; ready. A 2px bar sits on the hairline while the download is running.
  ;; Cancel is the hover control used on the buffer and bookmark lists.
  (labels ((download-row (download)
             "Present DOWNLOAD as its file name and a short status line."
             ;; Fresh binding so each row's callback closes over its own download.
             (let* ((download download)
                    (url (url download))
                    (status (status download))
                    (path (uiop:native-namestring (destination-path download)))
                    (finished-p (and (eq status :finished)
                                     (not (str:emptyp path))))
                    (active-p (member status '(:unloaded :loading))))
               (setf (user-interface:text (status-text download))
                     (download-summary download)
                     (user-interface:percentage (progress download))
                     (let ((percentage (completion-percentage download)))
                       (if (and (realp percentage) (<= 0 percentage 100))
                           percentage
                           0)))
               (connect download buffer)
               (spinneret:with-html
                 (:li :class (string-downcase (symbol-name status))
                      (:a :href (if finished-p "#" url)
                          :title (if finished-p path url)
                          :onclick (when finished-p
                                     (ps:ps
                                       (ps:chain event (prevent-default))
                                       (nyxt/ps:lisp-eval
                                        (:title "open-download"
                                         :buffer buffer)
                                        (nyxt/mode/file-manager:default-open-file-function
                                         (destination-path download)))
                                       nil))
                          (:span (download-filename download))
                          (:span :id (user-interface:id (status-text download))
                                 (user-interface:text (status-text download))))
                      (user-interface:to-html (progress download))
                      (when active-p
                        (:button :class "close"
                                 :type "button"
                                 :title "Cancel"
                                 :onclick (ps:ps
                                            (nyxt/ps:lisp-eval
                                             (:title "cancel-download"
                                              :buffer buffer)
                                             (cancel-download download)))
                                 "×")))))))
    (spinneret:with-html-string
      ;; Cancel is 28px inside the 17px inset. 17 + 28 + 17 = 62.
      (:nstyle
        '("ul.list > li.loading > a, ul.list > li.unloaded > a"
          :padding-right "62px")
        `("ul.list > li:hover > a"
          :background-color ,theme:background-color-
          :opacity "1")
        '("ul.list > li:hover::after"
          :background-color "transparent")
        '("ul.list > li:has(+ li:hover)::after"
          :background-color "transparent")
        '("ul.list > li.loading::after, ul.list > li.unloaded::after"
          :background-color "transparent")
        `("ul.list button.close"
          :position "absolute"
          :z-index "1"
          :top "13px"
          :right "17px"
          :display "flex"
          :align-items "center"
          :justify-content "center"
          :box-sizing "border-box"
          :width "28px"
          :height "28px"
          :margin "0"
          :padding "0"
          :border "none"
          :border-radius "14px"
          :appearance "none"
          :background-color "transparent"
          :color ,theme:primary-color
          :font-size "17px"
          :line-height "17px"
          :cursor "pointer"
          :opacity "0"
          :transition "opacity 120ms ease, background-color 120ms ease")
        '("ul.list > li:hover > button.close, ul.list button.close:focus-visible"
          :opacity "1")
        `("ul.list button.close:hover"
          :background-color ,theme:background-color
          :color ,theme:on-background-color)
        `("ul.list button.close:focus-visible"
          :outline "2px solid"
          :outline-color ,theme:action-color
          :outline-offset "2px")
        `("ul.list > li.failed a span:last-of-type"
          :color ,theme:warning-color)
        `("ul.list .progress-bar-base"
          :position "absolute"
          :z-index "1"
          :right "17px"
          :bottom "0"
          :left "17px"
          :height "2px"
          :pointer-events "none"
          :background-color ,theme:background-color-)
        `("ul.list .progress-bar-fill"
          :height "100%"
          :background-color ,theme:action-color)
        '("ul.list > li:not(.loading):not(.unloaded) .progress-bar-base"
          :display "none")
        `("p.empty"
          :margin "0"
          :font-size "17px"
          :line-height "24px"
          :color ,theme:primary-color))
      (render-menu 'download-mode buffer)
      (:h1 "Downloads")
      (if (downloads *browser*)
          (:ul :class "list"
               (dolist (download (downloads *browser*))
                 (download-row download)))
          (:p :class "empty" "No downloads.")))))

(define-command-global download-url ()
  "Download the page or file of the current buffer."
  (ffi-buffer-download (current-buffer) (render-url (url (current-buffer)))))

(define-command-global download-hint-url ()
  "Prompt for element hints and download them."
  (let ((buffer (current-buffer)))
    (nyxt/mode/hint:query-hints
     "Download link URL"
     (lambda (selected-links)
       (loop for link in selected-links
             do (ffi-buffer-download buffer (render-url (url link)))))
     :selector "a")))
