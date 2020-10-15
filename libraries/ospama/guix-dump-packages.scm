(use-modules
 (guix packages)
 (guix licenses)
 (guix utils)
 (gnu packages))

(define (ensure-list l)
  (if (list? l)
      l
      (list l)))

(with-output-to-file "/home/ambrevar/projects/nyxt/libraries/ospama/guix-database.lisp"
  (lambda ()
    (format #t "(~&")
    (fold-packages
     (lambda (package count)
       (let ((location (package-location package)))
         (format #t "(~s (:version ~s :outputs ~s :supported-systems ~s :inputs ~s :propagated-inputs ~s :native-inputs ~s :location ~s :home-page ~s :licenses ~s :synopsis ~s :description ~s))~&"
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
    (format #t "~&)~&")))
