;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(prove:plan nil)

(defun source1-suggestions (prompter)
  (mapcar #'prompter:value (prompter:suggestions
                            (first (prompter:sources prompter)))))

(prove:subtest "Prompter init"
  (let ((prompter (prompter:make
                   :sources (list (prompter:make-source
                                   :initial-suggestions '("foo" "bar"))))))
    (prove:ok (find "foo" (prompter:suggestions
                           (first (prompter:sources prompter)))
                    :test #'string=
                    :key #'prompter:value)
              "Found suggestion in dummy prompter")))

(prove:subtest "Prompter matching"
  (let ((prompter (prompter:make
                   :sources (list (prompter:make-source
                                   :initial-suggestions '("foo" "bar"))))))
    (setf (prompter:input prompter) "foo")
    (when (prompter:ready-p prompter)
      (prove:is (source1-suggestions prompter)
                '("foo")))
    (setf (prompter:input prompter) "bar")
    (when (prompter:ready-p prompter)
      (prove:is (source1-suggestions prompter)
                '("bar")))
    (setf (prompter:input prompter) "")
    (when (prompter:ready-p prompter)
      (prove:is (source1-suggestions prompter)
                '("foo" "bar")))))

(class*:define-class url ()
  ((uri "")
   (title ""))
  (:accessor-name-transformer #'class*:name-identity))

(defmethod prompter:object-properties ((url url))
  `(:uri ,(uri url)
    :title ,(title url)))

(prove:subtest "Multi-property matching"
  (let* ((url1 (make-instance 'url :uri "http://example.org" :title "Example"))
         (url2 (make-instance 'url :uri "http://nyxt.atlas.engineer" :title "Nyxt homepage"))
         (prompter (prompter:make
                    :sources (list (prompter:make-source
                                    :initial-suggestions (list url1 url2))))))
    (setf (prompter:input prompter) "nyxt")
    (when (prompter:ready-p prompter)
      (let ((filtered-suggestions (prompter:suggestions
                                   (first (prompter:sources prompter)))))
        (prove:is (mapcar #'prompter:value filtered-suggestions)
                  (list url2))))))

(defvar *prompter-suggestion-update-interval* 1.5)

(defun slow-identity-match (input suggestion)
  (declare (ignore input))
  (sleep *prompter-suggestion-update-interval*)
  suggestion)

(prove:subtest "Asynchronous suggestion computation"
  (let ((prompter (prompter:make
                   :sources (list (prompter:make-source
                                   :initial-suggestions '("foo" "bar")
                                   :filter #'slow-identity-match)))))
    (setf (prompter:input prompter) "foo")
    (when (prompter:ready-p prompter)
      (let ((filtered-suggestions (prompter:suggestions
                                   (first (prompter:sources prompter)))))
        (prove:is (mapcar #'prompter:value filtered-suggestions)
                  '("foo"))))))

(prove:subtest "Asynchronous suggestion notifications"
  (let* ((suggestion-values '("foobar" "foobaz"))
         (source (prompter:make-source
                  :initial-suggestions suggestion-values
                  :filter #'slow-identity-match))
         (prompter (prompter:make
                    :sources (list source))))
    (setf (prompter:input prompter) "foo")
    (sera:nlet query-suggestions ((computed-count 1))
      (calispel:fair-alt
        ((calispel:? (prompter:ready-notifier source))
         (prove:is (length (prompter:suggestions source))
                   (length suggestion-values)))
        ((calispel:? (prompter:update-notifier source))
         (prove:is (length (prompter:suggestions source))
                   computed-count)
         (query-suggestions (1+ computed-count)))))))

(prove:subtest "Asynchronous suggestion interrupt"
  (let* ((suggestion-values '("foobar" "foobaz"))
         (source (prompter:make-source
                  :initial-suggestions suggestion-values
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
                "Consecutive inputs happened fast enough"))))

(prove:subtest "Yes-No prompt"
  (let* ((source (make-instance 'prompter:yes-no-source
                                :initial-suggestions '("no" "yes")))
         (prompter (prompter:make
                    :sources (list source))))
    (prove:is
     (mapcar #'prompter:value (prompter:suggestions
                               (first (prompter:sources prompter))))
     '("no" "yes"))
    (setf (prompter:input prompter) "y")
    (when (prompter:ready-p prompter)
      (let ((filtered-suggestions (prompter:suggestions
                                   (first (prompter:sources prompter)))))
        (prove:is (mapcar #'prompter:value filtered-suggestions)
                  '("yes" "no"))))))

(prove:subtest "Return result"
  (let ((prompter (prompter:make
                   :sources (list (prompter:make-source
                                   :initial-suggestions '("foo" "bar"))))))
    (setf (prompter:input prompter) "bar")
    (when (prompter:ready-p prompter)
      (prompter:return-selection prompter)
      (prove:is (calispel:? (prompter:result-channel prompter))
                "bar"))))

(prove:finalize)
