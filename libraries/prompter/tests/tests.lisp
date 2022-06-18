;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(prove:plan nil)

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
           (prove:fail (format nil "Error when joining ~a: ~a"
                               remaining-threads c)))))
     (prove:is (all-live-prompter-threads)
               nil
               "No dangling threads")))

;; CCL randomly fails here.  TODO: There may be a race condition.
#+ccl
(defmacro with-report-dangling-threads (&body body)
  `(progn ,@body))

(prove:subtest "Prompter init"
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (make-instance 'prompter:source
                                             :name "Test source"
                                             :constructor '("foo" "bar")))))
      (when (prompter:all-ready-p prompter)
        (prove:ok (find "foo" (prompter:suggestions
                               (first (prompter:sources prompter)))
                        :test #'string=
                        :key #'prompter:value)
                  "Found suggestion in dummy prompter")))))

(prove:subtest "Prompter matching"
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:source
                                                   :name "Test source"
                                                   :constructor '("foo" "bar"))))))
      (setf (prompter:input prompter) "foo")
      (when (prompter:all-ready-p prompter)
        (prove:is (source1-suggestions prompter)
                  '("foo")))
      (setf (prompter:input prompter) "bar")
      (when (prompter:all-ready-p prompter)
        (prove:is (source1-suggestions prompter)
                  '("bar")))
      (setf (prompter:input prompter) "")
      (when (prompter:all-ready-p prompter)
        (prove:is (source1-suggestions prompter)
                  '("foo" "bar"))))))

(class-star:define-class url ()
  ((url "")
   (title ""))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((url url) (source t))
  (declare (ignore source))
  `(("URL" ,(url url))
    ("Title" ,(title url))))

(prove:subtest "Multi-attribute matching"
  (with-report-dangling-threads
    (let* ((url1 (make-instance 'url :url "http://example.org" :title "Example"))
           (url2 (make-instance 'url :url "http://nyxt.atlas.engineer" :title "Nyxt homepage"))
           (prompter (prompter:make
                      :sources (list (make-instance 'prompter:source
                                                    :name "Test source"
                                                    :constructor (list url1 url2))))))
      (setf (prompter:input prompter) "nyxt")
      (when (prompter:all-ready-p prompter)
        (let ((filtered-suggestions (prompter:suggestions
                                     (first (prompter:sources prompter)))))
          (prove:is (mapcar #'prompter:value filtered-suggestions)
                    (list url2))))
      (setf (prompter:active-attributes-keys (prompter:selected-source prompter))
            '("URL"))
      (prove:is (prompter:active-attributes-keys (prompter:selected-source prompter))
                '("URL"))
      (prove:is (prompter:active-attributes
                 (prompter:selected-suggestion prompter)
                 :source (prompter:selected-source prompter))
                `(("URL" ,(url url2) ))))))

(defvar *prompter-suggestion-update-interval* 1.5)

(defun slow-identity-match (suggestion source input)
  (declare (ignore source input))
  (sleep *prompter-suggestion-update-interval*)
  suggestion)

(prove:subtest "Asynchronous suggestion computation"
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:source
                                                   :name "Test source"
                                                   :constructor '("foo" "bar")
                                                   :filter #'slow-identity-match)))))
      (setf (prompter:input prompter) "foo")
      (when (prompter:all-ready-p prompter)
        (let ((filtered-suggestions (prompter:suggestions
                                     (first (prompter:sources prompter)))))
          (prove:is (mapcar #'prompter:value filtered-suggestions)
                    '("foo")))))))

(prove:subtest "Asynchronous suggestion notifications"
  (with-report-dangling-threads
    (let* ((suggestion-values '("foobar" "foobaz"))
           (source (make-instance 'prompter:source
                                  :name "Test source"
                                  :constructor suggestion-values
                                  :filter #'slow-identity-match))
           (prompter (prompter:make
                      :sources (list source))))
      (setf (prompter:input prompter) "foo")
      (sera:nlet query-suggestions ((computed-count 1))
        (calispel:fair-alt
          ((calispel:? (prompter::ready-channel source))
           (prove:is (length (prompter:suggestions source))
                     (length suggestion-values)))
          ((calispel:? (prompter:update-notifier source))
           (prove:is (length (prompter:suggestions source))
                     computed-count)
           (query-suggestions (1+ computed-count))))))))

(prove:subtest "Asynchronous suggestion interrupt"
  (with-report-dangling-threads
    (let* ((suggestion-values '("foobar" "foobaz"))
           (source (make-instance 'prompter:source
                                  :name "Test source"
                                  :constructor suggestion-values
                                  :filter #'slow-identity-match))
           (prompter (prompter:make
                      :sources (list source))))
      (let ((before-input (get-internal-real-time)))
        (setf (prompter:input prompter) "foo")
        (setf (prompter:input prompter) "bar")
        (setf (prompter:input prompter) "baz")
        (prove:is (/ (- (get-internal-real-time) before-input)
                     internal-time-units-per-second)
                  0.01
                  :test #'<
                  "Consecutive inputs happened fast enough")
        (prompter:all-ready-p prompter)))))

(prove:subtest "Yes-No prompt"
  (with-report-dangling-threads
    (let* ((source (make-instance 'prompter:yes-no-source
                                  :constructor '("no" "yes")))
           (prompter (prompter:make
                      :sources (list source))))
      (prove:is
       (mapcar #'prompter:value (prompter:suggestions
                                 (first (prompter:sources prompter))))
       '("no" "yes"))
      (setf (prompter:input prompter) "y")
      (prove:ok (prompter:all-ready-p prompter))
      (let ((filtered-suggestions (prompter:suggestions
                                   (first (prompter:sources prompter)))))
        (prove:is (mapcar #'prompter:value filtered-suggestions)
                  '("yes" "no"))))))

(prove:subtest "Return result"
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:source
                                                   :name "Test source"
                                                   :constructor '("foo" "bar"))))))
      (setf (prompter:input prompter) "bar")
      (when (prompter:all-ready-p prompter)
        (prompter:return-selection prompter)
        (prove:is (calispel:? (prompter:result-channel prompter))
                  '("bar"))))))

(prove:subtest "Multi sources"
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
        (prove:is (all-source-suggestions prompter)
                  '("foo" "100 foo")))
      (setf (prompter:input prompter) "200")
      (let ((ready-source1 (prompter:next-ready-p prompter))
            (ready-source2 (prompter:next-ready-p prompter)))
        (prove:ok (find ready-source1 (prompter:sources prompter))
                  "Found first ready source")
        (prove:ok (find ready-source2 (prompter:sources prompter))
                  "Found second ready source")
        (prove:isnt ready-source1
                    ready-source2
                    "Ready sources are not the same")
        (prove:is (all-source-suggestions prompter)
                  '("foo" "bar" "200"))))))

(prove:subtest "Raw source"
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:raw-source)))))
      (setf (prompter:input prompter) "foo")
      (when (prompter:all-ready-p prompter)
        (prove:is (all-source-suggestions prompter)
                  '("foo"))))
    (let ((prompter (prompter:make
                     :input "foo"
                     :sources (list (make-instance 'prompter:raw-source)))))
      (when (prompter:all-ready-p prompter)
        (prove:is (all-source-suggestions prompter)
                  '("foo"))))))

(prove:subtest "Alist-plist-hash source"
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
      (prove:is (length (prompter:sources prompter))
                4)
      (prove:is (mapcar (lambda (s) (length (prompter:suggestions s))) (prompter:sources prompter))
                '(2 2 2 2))
      (prove:is (mapcar #'prompter:attributes
                        (prompter:suggestions (first (prompter:sources prompter))))
                '((("A" "17") ("B" "18"))
                  (("A" "foo") ("B" "bar"))))
      (prove:is (mapcar #'prompter:attributes
                        (prompter:suggestions (second (prompter:sources prompter))))
                '((("KEY1" "101") ("key2" "102"))
                  (("KEY1" "val1") ("key2" "val2"))))
      (prove:is (mapcar #'prompter:attributes
                        (prompter:suggestions (third (prompter:sources prompter))))
                '((("KEY1" "101") ("key2" "102"))
                  (("KEY1" "val1") ("key2" "val2"))))
      (prove:is (mapcar #'prompter:attributes
                        (prompter:suggestions (fourth (prompter:sources prompter))))
                '((("17" "400") ("B" "200") ("a" "300"))
                  (("17" "4000") ("B" "2000") ("a" "3000"))))
      (prompter:all-ready-p prompter))))

(prove:subtest "History"
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list (make-instance 'prompter:source
                                                   :name "Test source"
                                                   :constructor '("foo" "bar"))))))
      (flet ((history ()
               (containers:container->list (prompter:history prompter)))
             (sync ()
               (prompter::add-input-to-history prompter)))
        (setf (prompter:input prompter) "banana")
        (sync)
        (prove:is (history)
                  '("banana"))
        (setf (prompter:input prompter) "jackfruit")
        (sync)
        (prove:is (history)
                  '("jackfruit" "banana"))
        (prove:is (containers:first-item (prompter:history prompter))
                  "jackfruit")
        (setf (prompter:input prompter) "banana")
        (sync)
        (prove:is (history)
                  '("banana" "jackfruit"))
        (prompter:all-ready-p prompter)))))

(prove:subtest "Select"
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
        (prove:is (selection-value)
                  "bar")
        (prompter:select-next prompter)
        (prove:is (selection-value)
                  "100 foo")
        (prompter:select-next prompter)
        (prove:is (selection-value)
                  "200")
        (prompter:select-next prompter)
        (prove:is (selection-value)
                  "200")
        (prompter:select-previous prompter)
        (prove:is (selection-value)
                  "100 foo")
        (prompter:select-first prompter)
        (prove:is (selection-value)
                  "foo")
        (prompter:select-previous prompter)
        (prove:is (selection-value)
                  "foo")
        (prompter:select-last prompter)
        (prove:is (selection-value)
                  "200")
        (prompter:select-previous-source prompter)
        (prove:is (selection-value)
                  "bar")
        (prompter:select-previous-source prompter)
        (prove:is (selection-value)
                  "bar")
        (prompter:select-next-source prompter)
        (prove:is (selection-value)
                  "100 foo")
        (prompter:select-next-source prompter)
        (prove:is (selection-value)
                  "100 foo")

        (setf (prompter:input prompter) "bar")
        (prompter:all-ready-p prompter)
        (prove:is (selection-value)
                  "bar")
        (prove:is (all-source-suggestions prompter)
                  '("bar"))
        (prompter:select-next prompter)
        (prove:is (selection-value)
                  "bar")
        (prompter:select-next-source prompter)
        (prove:is (selection-value)
                  "bar"))
      (prompter:all-ready-p prompter))))

(prove:subtest "Select with steps"
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
        (prove:is (selection-value)
                  "100 foo")
        (prompter:select-next prompter -2)
        (prove:is (selection-value)
                  "foo")
        (prompter:select-next prompter 99)
        (prove:is (selection-value)
                  "200"))
      (prompter:all-ready-p prompter))))

(prove:subtest "Select with wrap-over"
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
        (prove:is (selection-value)
                  "200")
        (prompter:select-next prompter)
        (prove:is (selection-value)
                  "200")
        (prompter::select prompter 1 :wrap-over-p t)
        (prove:is (selection-value)
                  "foo")
        (prompter::select prompter -1 :wrap-over-p t)
        (prove:is (selection-value)
                  "200")
        (prompter::select prompter 2 :wrap-over-p t)
        (prove:is (selection-value)
                  "bar")
        (prompter::select prompter -3 :wrap-over-p t)
        (prove:is (selection-value)
                  "100 foo"))
      (prompter:all-ready-p prompter))))

(class-star:define-class buffer ()
  ((title "")
   (keywords '("foo" "bar")))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((buffer buffer) (source t))
  (declare (ignore source))
  `(("Title" ,(title buffer))
    ("Keywords" ,(lambda (buffer) (sleep 1) (write-to-string (keywords buffer))))))

(prove:subtest "Async-attribute computation"
  (with-report-dangling-threads
      (let* ((buffer1 (make-instance 'buffer :title "buffer1" :keywords '("foo1" "bar1")))
             (buffer2 (make-instance 'buffer :title "buffer2" :keywords '("foo2" "bar2")))
             (prompter (prompter:make
                        :sources (list (make-instance 'prompter:source
                                                      :name "Test source"
                                                      :constructor (list buffer1 buffer2)
                                                      :active-attributes-keys '("Title"))))))
        (prove:is (prompter:active-attributes
                   (prompter:selected-suggestion prompter)
                   :source (prompter:selected-source prompter))
                  `(("Title" ,(title buffer1))))
        (setf (prompter:active-attributes-keys (prompter:selected-source prompter))
                  '("Title" "Keywords"))

        (prove:is (first (alex:assoc-value (prompter:active-attributes
                                            (prompter:selected-suggestion prompter)
                                            :source (prompter:selected-source prompter))
                                           "Keywords" :test 'equal))
                  "")
        (sleep 2)

        (prove:is (prompter:active-attributes
                   (prompter:selected-suggestion prompter)
                   :source (prompter:selected-source prompter))
                  `(("Title" ,(title buffer1))
                    ("Keywords" ,(write-to-string (keywords buffer1))))))))

(prove:subtest "Error handling"
  (with-report-dangling-threads
    (let ((prompter (prompter:make
                     :sources (list
                               (make-instance 'prompter:source
                                              :name "Test source"
                                              :constructor '("foo" "bar")
                                              :filter-postprocessor
                                              (lambda (suggestions source input)
                                                (declare (ignore suggestions source))
                                                (/ 1 input)))))))
      (flet ((selection-value ()
               (prompter:value (prompter:selected-suggestion prompter))))
        (prompter:all-ready-p prompter)
        (prove:is (selection-value)
                  "foo")
        (setf (prompter:input prompter) "bar")
        (prompter:all-ready-p prompter)
        (prove:is (selection-value)
                  "bar")
        (prompter:all-ready-p prompter)))))

(prove:finalize)
