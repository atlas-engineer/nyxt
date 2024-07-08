(in-package :password)

;; bitwarden password integration for nyxt

;; this (really quite naive) code works for me, and it might work for you too if you jump through some hoops.

;; 1. download the bitwarden command line client from https://bitwarden.com/download/#downloads-command-line-interface

;; 2. start the command line client and login to it (see https://bitwarden.com/help/cli/#using-email-and-password). i did it like this:

;;         bw login <email> <password> --method 0 --code <code>

;; 3. start the command line client in server mode:

;;         bw serve --port <number> --hostname <hostname> # note i needed to use 127.0.0.1 as the hostname

;;    the default value for `--port` at time of writing is `8087`.

;; now you can move `password-bitwarden.lisp` somewhere where nyxt will find it when it starts. i have this in `.config/nyxt/config.lisp`

;;     (define-nyxt-user-system-and-load nyxt-user/basic-config
;;         :components ("password"))

;; there's one parameter you can set: `*bitwarden-host*`. default value is `http://localhost:8087`. 


(define-class bitwarden-interface (password-interface)
  ((password :accessor password :type string :initform "")
   (sessionid :accessor sessionid :type string :initform ""))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(push 'bitwarden-interface *interfaces*)

(defparameter *bitwarden-host* "http://localhost:8087")

(defun get-password ()
  (let ((p1 (nyxt:prompt :prompt "enter password" :sources (make-instance 'prompter:raw-source) :invisible-input-p t))
	(p2 (nyxt:prompt :prompt "please confirm" :sources (make-instance 'prompter:raw-source)
			 :invisible-input-p t)))
    (if (string= (first p1) (first p2)) (first p1)
	(progn
	  (nyxt:echo-warning "passwords do not match")
	  (get-password)))))

(defun get-sessionid (output)
  (let ((j (njson:decode output)))
    (gethash "raw" (gethash "data" j))))

(defun get-password-list (output)
  (let ((json-ht (njson:decode output)))
    (coerce (gethash "data" (gethash "data" json-ht)) 'list)))

(defun my/get-passwords (sessionid)
  (format *error-output* "reading passwords over rest~%")
  (handler-case
      (let* ((output
	       (dex:get (format nil "~a/list/object/items" *bitwarden-host*)
			:headers `(("sessionid" . sessionid)
				   ("Content-Type" . "application/json"))))
	     (passwords (get-password-list output)))
	(format *error-output* "~a passwords read~%" (length passwords))
	passwords)
    (error (c)
      (nyxt:echo-warning (format nil "could not list-passwords: ~a~%" c)))))

(defun get-password-names (passwords)
  (mapcar (lambda (ht) (gethash "name" ht)) passwords))

(defmethod connect ((password-interface bitwarden-interface))
  (with-slots (password sessionid) password-interface
    (format *error-output* "calling connect. current value of session-id: ~a~%" sessionid)
    (if (not (zerop (length sessionid)))
	t
	(handler-case (let ((output (dex:post (format nil "~a/unlock" *bitwarden-host*)
					      :content (format nil "{\"password\": \"~a\"}" (get-password))
					      :headers '(("Content-Type" . "application/json")))))
			(format *error-output* output)
			(setf sessionid (get-sessionid output))
			(format *error-output* "session-id: ~a~%" sessionid) 
			t)
	  (error (c)
	    (nyxt:echo-warning "login failed: " c))))))

(defmethod list-passwords ((password-interface bitwarden-interface))
  (when (connect password-interface)
    (with-slots (sessionid) password-interface
      (handler-case (let* ((password-seq (my/get-passwords sessionid)))
		      (get-password-names password-seq))
	(error (c)
	  (nyxt:echo-warning (format nil "could not list-passwords: ~a~%" c)))))))

(defun get-login-field (pw field)
  (let* ((login-block (gethash "login" pw)))
    (gethash field login-block)))
		       
(defun name-matcher (name)
  (lambda (pw)
    (let* ((other-name (gethash "name" pw)))
      (string= name other-name))))


(defun login-field-matcher (field name)
  (lambda (pw)
    (let* ((password-name (get-login-field pw field)))
      (string= name password-name))))

(defmethod clip-password ((password-interface bitwarden-interface) &key password-name service)
  (declare (ignore service))
  (format *error-output* "getting password for entry with name ~a~%" password-name)
  (when (connect password-interface)
    (format *error-output* "connection successful~%")
    (handler-case
	(with-slots (sessionid) password-interface
	  (let* ((passwords (my/get-passwords sessionid))
		 (matching (remove-if-not (name-matcher password-name) passwords))
		 (password (get-login-field (first matching) "password")))
	    (clip-password-string password-interface password)))
      (error (c)
	(format *error-output* "could not clip-password: ~a~%" c)
	(nyxt:echo-warning (format nil "could not clip-password: ~a~%" c))))))


(defmethod clip-username ((password-interface bitwarden-interface) &key password-name service)
  (declare (ignore service))
  (when (connect password-interface)
    (handler-case
	(with-slots (sessionid) password-interface
	  (let* ((passwords (my/get-passwords sessionid))
		 (matching (remove-if-not (name-matcher password-name) passwords))
		 (username (get-login-field (first matching) "username")))
	    (trivial-clipboard:text username)))
      (error (c)
	(format *error-output* "could not clip-username: ~a~%" c)
	(nyxt:echo-warning (format nil "could not clip-username: ~a~%" c))))))


(defmethod save-password ((password-interface bitwarden-interface)
                          &key password-name username password service)
  (declare (ignore service))
  (nyxt:echo-warning "saving passwords not yet supported"))

(defmethod password-correct-p ((password-interface bitwarden-interface))
  (connect password-interface)
  t)
