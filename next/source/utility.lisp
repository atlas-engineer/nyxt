;;; utility.lisp --- utility classes and functions

(in-package :next)

;; data node used to represent tree history
(defstruct node
  parent
  children
  data)

(defcommand load-file ()
  (with-result (file-name-input (read-from-minibuffer
                                 (mode *minibuffer*)))
    (load file-name-input :if-does-not-exist nil)))

(defun reload-init ()
  (load-file *init-file-path*))

(defun start-swank ()
  (#+ccl ccl::call-in-event-process
   #-ccl progn
   #'(lambda ()
       (swank:create-server :port *swank-port* :dont-close t))))

(defun parse-url (input-url)
  (if (equalp "s" (nth 0 (cl-strings:split input-url)))
      (generate-search-query (subseq input-url 2))
      (handler-case
          ;; puri:parse-uri fails on crazy inputs like:
          ;; - hello world
          ;; - https://www.google.com/search?q=hello world
          (let ((url (puri:parse-uri input-url)))
            (if (puri:uri-scheme url)
                input-url
                (concatenate 'string "https://" input-url)))
        (puri:uri-parse-error ()
          input-url))))

(defun generate-search-query (search-string)
  (let* ((encoded-search-string
	  (cl-string-match:replace-re "  *" "+" search-string :all t))
	 (url (concatenate 'string "https://duckduckgo.com/?q=" encoded-search-string)))
    url))
