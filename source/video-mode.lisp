(uiop:define-package :next/video
    (:use :common-lisp :next)
  (:export :download)
  (:documentation "Download videos on the Web.

Command defined in this package: `download-video'.

It tries by default to use the program `youtube-dl', that you must
have in your path.

You can also write a new function that takes a URL as parameter, and
bind it to `next/video:*download-function*'.  In doing so, you can
rely on the `next/video:download' function, that does error handling
and process management.

***********************************************************************
*Disclaimer*: this feature is meant to grow with Next 1.4 and onwards!
***********************************************************************

;XXX: show progress
;XXX: make it appear in the recent downloaded files
;XXX: ask any destination

What can be done: automatically install the required program, better
notifications, choose videos, etc.
"))

(in-package :next/video)

(defparameter *download-program* "youtube-dl"
  "The external program to download videos with. Defaults to youtube-dl.")

(defparameter *download-args* nil
  "Default arguments for the download command. See also `download-arguments' which adds more.")

(defparameter *preferred-download-directories* (list download-manager::*default-download-directory*)
  "List of favorite directories to save videos to.")


(defun download-arguments (url target-dir)
  "Return a list of arguments for the download command.

Appends `*download-args' and -o /target/directory/%(title)s.%(ext)s (for youtube-dl)."
  (declare (ignorable url))
  (append *download-args* (list "-o" (format nil "~a/%(title)s.%(ext)s" target-dir))))

(defun resolve-download-directory (target-dir)
  (or target-dir
      (first *preferred-download-directories*)
      download-manager::*default-download-directory*))

(defun download-command (url &optional target-dir)
  (let ((target-dir (resolve-download-directory target-dir)))
    (setf target-dir (string-right-trim (list #\/) (namestring target-dir)))
    (append (list *download-program*
                  url)
            (download-arguments url target-dir))))

(defun download (url &optional target-dir)
  "Download asynchronously."
  (handler-case
      (progn
        (unless target-dir
          (setf target-dir (resolve-download-directory target-dir)))
        (echo "Starting download of ~a to ~a." url target-dir)
        (log:info "Starting download of ~a to ~a." url target-dir)
        ;XXX notify progress.
        (launch-and-notify (download-command url target-dir)
                                :success-msg (format nil "Video downloaded to ~a." target-dir)
                                :error-msg (format nil "Failed to download video.~&")))
    (error (c)
      (log:warn "Error downloading ~a to ~a: ~a" url target-dir c))))

(defparameter *download-function* #'download
  "Default download function.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :next)

(define-command download-video ()
  "Download the video of the current URL with an external program."
  (let* ((url (url (current-buffer)))
         (uri (quri:uri (url (current-buffer)))))
    (cond
      ((null next/video::*preferred-download-directories*)
       ;XXX: ask destination.
       (next/video:download url))
      ((= 1 (length next/video::*preferred-download-directories*))
       (next/video:download url (first next/video::*preferred-download-directories*)))
      (t
       (with-result (target-dir (read-from-minibuffer
                                 (make-instance 'minibuffer
                                                :input-prompt "Target directory:"
                                                :completion-function
                                                (lambda (input)
                                                  (file-completion-function input next/video::*preferred-download-directories*)))))
         (next/video::download uri target-dir))))))
