(in-package :next)

(setf *port* (make-instance 'port))

(defmethod run-program ((port port))
  (let* ((path (uiop/image:argv0))
                (path (cl-strings:shorten path (- (length path) 4) :truncate-string ""))
         (path (concatenate 'string path "cocoa-webkit")))
    (setf (running-process port) (uiop:launch-program path))))

(define-key *global-map* (key "S-t") 'make-visible-new-buffer)
(define-key *global-map* (key "S-n") 'make-window)
(define-key *global-map* (key "S-w") 'delete-window)
(define-key *global-map* (key "S-q") 'kill)
(define-key *global-map* (key "C-TAB") 'switch-buffer-next)
(define-key *global-map* (key "C-s-TAB") 'switch-buffer-previous)
