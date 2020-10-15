(defvar *db* (read-from-string (uiop:read-file-string "/home/ambrevar/projects/nyxt/libraries/ospama/guix-database.lisp")))
(getf (second (assoc "emacs" *db* :test #'string=)) :version)

(defun guix-eval (form &rest more-forms)
  ;; TODO: "guix repl" is a reliable way to execute Guix code, sadly it does not
  ;; seem to support standard input.  Report upstream?  Alternatively, use Guile
  ;; the way emacs-guix.el does it, but it does not seem reliable.
  (let ((*print-case* :downcase))
    (uiop:with-temporary-file (:pathname p)
      (with-open-file (s p :direction :output :if-exists :append)
        (dolist (f (cons form more-forms))
          (format s "~s" f)))
      (uiop:run-program `("guix" "repl" ,(namestring p))
                        :output '(:string :stripped t)))))

(guix-eval
 '(define (ensure-list l)
   (if (list? l)
       l
       (list l)))
 '(display 17))

(define-class guix-package (package)
  ((outputs)))
