;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(defun source1-suggestions (prompter)
  (mapcar #'prompter:value (prompter:suggestions
                            (first (prompter:sources prompter)))))

(defun all-source-suggestions (prompter)
  (mapcar #'prompter:value (alex:mappend #'prompter:suggestions
                                         (prompter:sources prompter))))

(defun prompter-thread-p (thread)
  (str:starts-with-p "Prompter" (bt:thread-name thread)))

(defun all-live-prompter-threads ()
  (sera:filter (alex:conjoin #'prompter-thread-p #'bt:thread-alive-p) (bt:all-threads)))

(defun join-thread* (thread)
  "Like `bt:join-thread' but don't fail on already aborted threads."
  ;; CCL's `join-thread' works with aborted threads, but SBCL emits a
  ;; `join-thread-error' condition.
  (handler-case (bt:join-thread thread)
    #+sbcl
    (sb-thread:join-thread-error ()
      nil)))

#-ccl
(defmacro with-report-dangling-threads (&body body)
  `(unwind-protect (progn ,@body)
     (let ((remaining-threads (all-live-prompter-threads)))
       ;; Time out should be correlated to the number of threads since it may
       ;; take some time to destroy them on weak hardware.
       (handler-case (bt:with-timeout ((* 2 (length remaining-threads)))
                       (mapc #'join-thread* remaining-threads))
         (t (c)
           (error "Error when joining ~a: ~a" remaining-threads c))))
     ;; No dangling threads
     (assert-false (all-live-prompter-threads))))

;; CCL randomly fails here.  TODO: There may be a race condition.
#+ccl
(defmacro with-report-dangling-threads (&body body)
  `(progn ,@body))

(define-test prompter-init ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make :sources (make-instance 'prompter:source
                                                           :name "Test source"
                                                           :constructor '("foo" "bar")))))
      (when (prompter:all-ready-p prompter)
        ;; Found suggestion in dummy prompter
        (assert-true (find "foo" (prompter:suggestions
                                  (first (prompter:sources prompter)))
                           :test #'string=
                           :key #'prompter:value))))))

(define-test prompter-matching ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (make-instance 'prompter:source
                                             :name "Test source"
                                             :constructor '("foo" "bar")))))
      (setf (prompter:input prompter) "foo")
      (when (prompter:all-ready-p prompter)
        (assert-equal '("foo")
                      (source1-suggestions prompter)))
      (setf (prompter:input prompter) "bar")
      (when (prompter:all-ready-p prompter)
        (assert-equal '("bar")
                      (source1-suggestions prompter)))
      (setf (prompter:input prompter) "")
      (when (prompter:all-ready-p prompter)
        (assert-equal '("foo" "bar")
                      (source1-suggestions prompter))))))

(class-star:define-class url ()
  ((url "")
   (title ""))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((url url) (source prompter:source))
  (declare (ignore source))
  `(("URL" ,(url url))
    ("Title" ,(title url))))

(define-test multi-attribute-matching ()
  (with-report-dangling-threads
    (let* ((url1 (make-instance 'url :url "http://example.org" :title "Example"))
           (url2 (make-instance 'url :url "http://nyxt.atlas.engineer" :title "Nyxt homepage"))
           (prompter (prompter:make
                      :sources (make-instance 'prompter:source
                                              :name "Test source"
                                              :constructor (list url1 url2)))))
      (setf (prompter:input prompter) "nyxt")
      (when (prompter:all-ready-p prompter)
        (let ((filtered-suggestions (prompter:suggestions
                                     (first (prompter:sources prompter)))))
          (assert-equal (list url2)
                        (mapcar #'prompter:value filtered-suggestions))))
      (setf (prompter:active-attributes-keys (prompter:selected-source prompter))
            '("URL"))
      (assert-equal '("URL")
                    (prompter:active-attributes-keys (prompter:selected-source prompter)))
      (assert-equal `(("URL" ,(url url2) ))
                    (prompter:active-attributes
                     (prompter:selected-suggestion prompter)
                     :source (prompter:selected-source prompter))))))

(defvar *prompter-suggestion-update-interval* 1.5)

(defun slow-identity-match (suggestion source input)
  (declare (ignore source input))
  (sleep *prompter-suggestion-update-interval*)
  suggestion)

(define-test asynchronous-suggestion-computation ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (make-instance 'prompter:source
                                             :name "Test source"
                                             :constructor '("foo" "bar")
                                             :filter #'slow-identity-match))))
      (setf (prompter:input prompter) "foo")
      (when (prompter:all-ready-p prompter)
        (let ((filtered-suggestions (prompter:suggestions
                                     (first (prompter:sources prompter)))))
          (assert-equal '("foo")
                        (mapcar #'prompter:value filtered-suggestions)))))))

(define-test asynchronous-suggestion-notifications ()
  (with-report-dangling-threads
    (let* ((suggestion-values '("foobar" "foobaz"))
           (source (make-instance 'prompter:source
                                  :name "Test source"
                                  :constructor suggestion-values
                                  :filter #'slow-identity-match))
           (prompter (prompter:make :sources source)))
      (setf (prompter:input prompter) "foo")
      (sera:nlet query-suggestions ((computed-count 1))
        (calispel:fair-alt
          ((calispel:? (prompter::ready-channel source))
           (assert-eq (length suggestion-values)
                      (length (prompter:suggestions source))))
          ((calispel:? (prompter:update-notifier source))
           (assert-eq computed-count
                      (length (prompter:suggestions source)))
           (query-suggestions (1+ computed-count))))))))

(define-test asynchronous-suggestion-interrupt ()
  (with-report-dangling-threads
    (let* ((suggestion-values '("foobar" "foobaz"))
           (source (make-instance 'prompter:source
                                  :name "Test source"
                                  :constructor suggestion-values
                                  :filter #'slow-identity-match))
           (prompter (prompter:make :sources source)))
      (let ((before-input (get-internal-real-time)))
        (setf (prompter:input prompter) "foo")
        (setf (prompter:input prompter) "bar")
        (setf (prompter:input prompter) "baz")
        ;; Consecutive inputs happened fast enough
        (assert-equality #'<
                         0.01
                         (/ (- (get-internal-real-time) before-input)
                            internal-time-units-per-second))
        (prompter:all-ready-p prompter)))))

(define-test yes-no-prompt ()
  (with-report-dangling-threads
    (let* ((source (make-instance 'prompter:yes-no-source
                                  :constructor '("no" "yes")))
           (prompter (prompter:make :sources source)))
      (assert-equal '("no" "yes")
                    (mapcar #'prompter:value (prompter:suggestions
                                              (first (prompter:sources prompter)))))
      (setf (prompter:input prompter) "y")
      (assert-true (prompter:all-ready-p prompter))
      (let ((filtered-suggestions (prompter:suggestions
                                   (first (prompter:sources prompter)))))
        (assert-equal '("yes" "no")
                      (mapcar #'prompter:value filtered-suggestions))))))

(define-test return-result ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (make-instance 'prompter:source
                                             :name "Test source"
                                             :constructor '("foo" "bar")))))
      (setf (prompter:input prompter) "bar")
      (when (prompter:all-ready-p prompter)
        (prompter:return-selection prompter)
        (assert-equal '("bar")
                      (calispel:? (prompter:result-channel prompter)))))))

(define-test multi-sources ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:source
                                                   :name "Test source 1"
                                                   :constructor '("foo" "bar"))
                                    (make-instance 'prompter:source
                                                   :name "Test source 2"
                                                   :constructor '("100 foo" "200"))))))
      (setf (prompter:input prompter) "foo")
      (when (prompter:all-ready-p prompter)
        (assert-equal '("foo" "100 foo")
                      (all-source-suggestions prompter)))
      (setf (prompter:input prompter) "200")
      (let ((ready-source1 (prompter:next-ready-p prompter))
            (ready-source2 (prompter:next-ready-p prompter)))
        ;; Found first ready source
        (assert-true (find ready-source1 (prompter:sources prompter)))
        ;; Found second ready source
        (assert-true (find ready-source2 (prompter:sources prompter)))
        ;; Ready sources are not the same
        (assert-eq nil
                   (eq ready-source1 ready-source2))
        (assert-equal '("foo" "bar" "200")
                      (all-source-suggestions prompter))))))

(define-test raw-source ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make :sources 'prompter:raw-source)))
      (setf (prompter:input prompter) "foo")
      (when (prompter:all-ready-p prompter)
        (assert-equal '("foo")
                      (all-source-suggestions prompter))))
    (let ((prompter (prompter:make :input "foo"
                                   :sources 'prompter:raw-source)))
      (when (prompter:all-ready-p prompter)
        (assert-equal '("foo")
                      (all-source-suggestions prompter))))))

(define-test alist-plist-hash-source ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list
                               (make-instance 'prompter:source
                                              :name "Plist source"
                                              :constructor '((:a 17 :b 18)
                                                             (:a "foo" :b "bar")))
                               (make-instance 'prompter:source
                                              :name "Alist source"
                                              :constructor '(((key1 101) ("key2" 102))
                                                             ((key1 "val1") ("key2" "val2"))))
                               (make-instance 'prompter:source
                                              :name "Dotted alist source"
                                              :constructor '(((key1 . 101) ("key2" . 102))
                                                             ((key1 . "val1") ("key2" . "val2"))))
                               (make-instance 'prompter:source
                                              :name "Hash table source"
                                              :constructor (list (sera:dict :b 200 "a" 300 17 400)
                                                                 (sera:dict :b 2000 "a" 3000 17 4000)))))))
      (assert-eq 4
                 (length (prompter:sources prompter)))
      (assert-equal '(2 2 2 2)
                    (mapcar (lambda (s) (length (prompter:suggestions s))) (prompter:sources prompter)))
      (assert-equal '((("A" "17") ("B" "18"))
                      (("A" "foo") ("B" "bar")))
                    (mapcar #'prompter:attributes
                            (prompter:suggestions (first (prompter:sources prompter)))))
      (assert-equal '((("KEY1" "101") ("key2" "102"))
                      (("KEY1" "val1") ("key2" "val2")))
                    (mapcar #'prompter:attributes
                            (prompter:suggestions (second (prompter:sources prompter)))))
      (assert-equal '((("KEY1" "101") ("key2" "102"))
                      (("KEY1" "val1") ("key2" "val2")))
                    (mapcar #'prompter:attributes
                            (prompter:suggestions (third (prompter:sources prompter)))))
      (assert-equal '((("17" "400") ("B" "200") ("a" "300"))
                      (("17" "4000") ("B" "2000") ("a" "3000")))
                    (mapcar #'prompter:attributes
                            (prompter:suggestions (fourth (prompter:sources prompter)))))
      (prompter:all-ready-p prompter))))

(define-test history ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make :sources (make-instance 'prompter:source
                                                           :name "Test source"
                                                           :constructor '("foo" "bar")))))
      (flet ((history ()
               (containers:container->list (prompter:history prompter)))
             (sync ()
               (prompter::add-input-to-history prompter)))
        (setf (prompter:input prompter) "banana")
        (sync)
        (assert-equal '("banana")
                      (history))
        (setf (prompter:input prompter) "jackfruit")
        (sync)
        (assert-equal '("jackfruit" "banana")
                      (history))
        (assert-equality 'string= "jackfruit"
                         (containers:first-item (prompter:history prompter)))
        (setf (prompter:input prompter) "banana")
        (sync)
        (assert-equal '("banana" "jackfruit")
                      (history))
        (prompter:all-ready-p prompter)))))

(define-test select ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:source
                                                   :name "Test source"
                                                   :constructor '("foo" "bar"))
                                    (make-instance 'prompter:source
                                                   :name "Test source 2"
                                                   :constructor '("100 foo" "200")
                                                   :filter-preprocessor #'prompter:filter-exact-matches)))))
      (flet ((selection-value ()
               (prompter:value (prompter:selected-suggestion prompter))))
        (prompter:all-ready-p prompter)
        (prompter:select-next prompter)
        (assert-equality 'string= "bar"
                         (selection-value))
        (prompter:select-next prompter)
        (assert-equality 'string= "100 foo"
                         (selection-value))
        (prompter:select-next prompter)
        (assert-equality 'string= "200"
                         (selection-value))
        (prompter:select-next prompter)
        (assert-equality 'string= "200"
                         (selection-value))
        (prompter:select-previous prompter)
        (assert-equality 'string= "100 foo"
                         (selection-value))
        (prompter:select-first prompter)
        (assert-equality 'string= "foo"
                         (selection-value))
        (prompter:select-previous prompter)
        (assert-equality 'string= "foo"
                         (selection-value))
        (prompter:select-last prompter)
        (assert-equality 'string= "200"
                         (selection-value))
        (prompter:select-previous-source prompter)
        (assert-equality 'string= "bar"
                         (selection-value))
        (prompter:select-previous-source prompter)
        (assert-equality 'string= "bar"
                         (selection-value))
        (prompter:select-next-source prompter)
        (assert-equality 'string= "100 foo"
                         (selection-value))
        (prompter:select-next-source prompter)
        (assert-equality 'string= "100 foo"
                         (selection-value))

        (setf (prompter:input prompter) "bar")
        (prompter:all-ready-p prompter)
        (assert-equality 'string= "bar"
                         (selection-value))
        (assert-equal '("bar")
                      (all-source-suggestions prompter))
        (prompter:select-next prompter)
        (assert-equality 'string= "bar"
                         (selection-value))
        (prompter:select-next-source prompter)
        (assert-equality 'string= "bar"
                         (selection-value)))
      (prompter:all-ready-p prompter))))

(define-test select-with-steps ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:source
                                                   :name "Test source"
                                                   :constructor '("foo" "bar"))
                                    (make-instance 'prompter:source
                                                   :name "Test source 2"
                                                   :constructor '("100 foo" "200")
                                                   :filter-preprocessor #'prompter:filter-exact-matches)))))
      (flet ((selection-value ()
               (prompter:value (prompter:selected-suggestion prompter))))
        (prompter:all-ready-p prompter)
        (prompter:select-next prompter 2)
        (assert-equality 'string= "100 foo"
                         (selection-value))
        (prompter:select-next prompter -2)
        (assert-equality 'string= "foo"
                         (selection-value))
        (prompter:select-next prompter 99)
        (assert-equality 'string= "200"
                         (selection-value)))
      (prompter:all-ready-p prompter))))

(define-test select-with-wrap-over ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:source
                                                   :name "Test source"
                                                   :constructor '("foo" "bar"))
                                    (make-instance 'prompter:source
                                                   :name "Test source 2"
                                                   :constructor '("100 foo" "200")
                                                   :filter-preprocessor #'prompter:filter-exact-matches)))))
      (flet ((selection-value ()
               (prompter:value (prompter:selected-suggestion prompter))))
        (prompter:all-ready-p prompter)
        (prompter:select-last prompter)
        (assert-equality 'string= "200"
                         (selection-value))
        (prompter:select-next prompter)
        (assert-equality 'string= "200"
                         (selection-value))
        (prompter::select prompter 1 :wrap-over-p t)
        (assert-equality 'string= "foo"
                         (selection-value))
        (prompter::select prompter -1 :wrap-over-p t)
        (assert-equality 'string= "200"
                         (selection-value))
        (prompter::select prompter 2 :wrap-over-p t)
        (assert-equality 'string= "bar"
                         (selection-value))
        (prompter::select prompter -3 :wrap-over-p t)
        (assert-equality 'string= "100 foo"
                         (selection-value)))
      (prompter:all-ready-p prompter))))

(class-star:define-class buffer ()
  ((title "")
   (keywords '("foo" "bar")))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((buffer buffer) (source prompter:source))
  (declare (ignore source))
  `(("Title" ,(title buffer))
    ("Keywords" ,(lambda (buffer) (sleep 1) (write-to-string (keywords buffer))))))

(define-test async-attribute-computation ()
  (with-report-dangling-threads
    (let* ((buffer1 (make-instance 'buffer :title "buffer1" :keywords '("foo1" "bar1")))
           (buffer2 (make-instance 'buffer :title "buffer2" :keywords '("foo2" "bar2")))
           (prompter (prompter:make
                      :sources (make-instance 'prompter:source
                                              :name "Test source"
                                              :constructor (list buffer1 buffer2)
                                              :active-attributes-keys '("Title")))))
      (assert-equal `(("Title" ,(title buffer1)))
                    (prompter:active-attributes
                     (prompter:selected-suggestion prompter)
                     :source (prompter:selected-source prompter)))
      (setf (prompter:active-attributes-keys (prompter:selected-source prompter))
            '("Title" "Keywords"))

      (assert-equality 'string= ""
                       (first (alex:assoc-value (prompter:active-attributes
                                                 (prompter:selected-suggestion prompter)
                                                 :source (prompter:selected-source prompter))
                                                "Keywords" :test 'equal)))
      (sleep 2)

      (assert-equal `(("Title" ,(title buffer1))
                      ("Keywords" ,(write-to-string (keywords buffer1))))
                    (prompter:active-attributes
                     (prompter:selected-suggestion prompter)
                     :source (prompter:selected-source prompter))))))

(define-test error-handling ()
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (make-instance 'prompter:source
                                             :name "Test source"
                                             :constructor '("foo" "bar")
                                             :filter-postprocessor
                                             (lambda (suggestions source input)
                                               (declare (ignore suggestions source))
                                               (/ 1 input))))))
      (flet ((selection-value ()
               (prompter:value (prompter:selected-suggestion prompter))))
        (prompter:all-ready-p prompter)
        (assert-equality 'string= "foo"
                         (selection-value))
        (setf (prompter:input prompter) "bar")
        (prompter:all-ready-p prompter)
        (assert-equality 'string= "bar"
                         (selection-value))
        (prompter:all-ready-p prompter)))))
