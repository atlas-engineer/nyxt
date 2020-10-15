(in-package :ospama)

(defun guix-eval (form &rest more-forms)
  ;; TODO: "guix repl" is a reliable way to execute Guix code, sadly it does not
  ;; seem to support standard input.  Report upstream?  Alternatively, use Guile
  ;; the way emacs-guix.el does it, but it does not seem reliable.
  (let ((*print-case* :downcase))
    (uiop:with-temporary-file (:pathname p)
      (with-open-file (s p :direction :output :if-exists :append)
        (dolist (f (cons form more-forms))
          (write-string
           ;; Escaped symbols (e.g. '\#t) are printed as '|NAME| but should be
           ;; printed as NAME.
           (ppcre:regex-replace-all "'\\|([^|]*)\\|" (format nil "~s" f) "\\1")
           s)))
      (uiop:run-program `("guix" "repl" ,(namestring p))
                        :output '(:string :stripped t)
                        :error-output :output))))

(defun generate-database ()
  (guix-eval
   '(use-modules
     (guix packages)
     (guix licenses)
     (guix utils)
     (gnu packages))

   '(define (ensure-list l)
     (if (list? l)
         l
         (list l)))

   '(display
     (with-output-to-string
         (lambda ()
           (format '\#t "(~&")
           (fold-packages
            (lambda (package count)
              (let ((location (package-location package)))
                (format '\#t "(~s (:version ~s :outputs ~s :supported-systems ~s :inputs ~s :propagated-inputs ~s :native-inputs ~s :location ~s :home-page ~s :licenses ~s :synopsis ~s :description ~s))~&"
                        (package-name package)
                        (package-version package)
                        (package-outputs package)
                        (package-supported-systems package)
                        (map car (package-inputs package))
                        (map car (package-propagated-inputs package))
                        (map car (package-native-inputs package))
                        (string-join (list (location-file location)
                                           (number->string (location-line location))
                                           (number->string (location-column location)))
                                     ":")
                        (or (package-home-page package) 'nil) ; #f must be turned to NIL for Common Lisp.
                        (map license-name (ensure-list (package-license package)))
                        (package-synopsis package)
                        (package-description package)))
              (+ 1 count))
            1)
           (format '\#t "~&)~&"))))))

(defvar *guix-database* nil)

(defun find-os-package (name)
  (unless *guix-database*
    (setf *guix-database* (read-from-string (generate-database))))
  (getf (second (assoc name *guix-database* :test #'string=)) :version))

(define-class guix-package (os-package)
  ((outputs '())
   (supported-systems '())
   (inputs '())
   (propagated-inputs '())
   (native-inputs '())
   (description "")))
