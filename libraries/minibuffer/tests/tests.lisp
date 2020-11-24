;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(prove:plan nil)

(prove:subtest "Minibuffer init"
  (let ((minibuffer (minibuffer:make
                     :sources (list (minibuffer:make-source
                                     :initial-suggestions '("foo" "bar"))))))
    (prove:ok (find "foo" (minibuffer:suggestions
                           (first (minibuffer:sources minibuffer)))
                    :test #'string=
                    :key #'minibuffer:value)
              "Found suggestion in dummy minibuffer")))

(prove:subtest "Minibuffer matching"
  (let ((minibuffer (minibuffer:make
                     :sources (list (minibuffer:make-source
                                     :initial-suggestions '("foo" "bar"))))))
    (setf (minibuffer:input minibuffer) "foo")
    (when (minibuffer:ready? minibuffer)
      (let ((filtered-suggestions (minibuffer:suggestions
                                   (first (minibuffer:sources minibuffer)))))
        (prove:is (mapcar #'minibuffer:value filtered-suggestions)
                  '("foo"))))))

(class*:define-class url ()
  ((uri "")
   (title ""))
  (:accessor-name-transformer #'class*:name-identity))

(defmethod minibuffer:object-properties ((url url))
  `(:uri ,(uri url)
    :title ,(title url)))

(prove:subtest "Multi-property matching"
  (let* ((url1 (make-instance 'url :uri "http://example.org" :title "Example"))
         (url2 (make-instance 'url :uri "http://nyxt.atlas.engineer" :title "Nyxt homepage"))
         (minibuffer (minibuffer:make
                      :sources (list (minibuffer:make-source
                                      :initial-suggestions (list url1 url2))))))
    (setf (minibuffer:input minibuffer) "nyxt")
    (when (minibuffer:ready? minibuffer)
      (let ((filtered-suggestions (minibuffer:suggestions
                                   (first (minibuffer:sources minibuffer)))))
        (prove:is (mapcar #'minibuffer:value filtered-suggestions)
                  (list url2))))))

(defvar *minibuffer-suggestion-update-interval* 1.5)

(defun slow-identity-match (input suggestion)
  (declare (ignore input))
  (sleep *minibuffer-suggestion-update-interval*)
  suggestion)

(prove:subtest "Asynchronous suggestion computation"
  (let ((minibuffer (minibuffer:make
                     :sources (list (minibuffer:make-source
                                     :initial-suggestions '("foo" "bar")
                                     :filter #'slow-identity-match)))))
    (setf (minibuffer:input minibuffer) "foo")
    (when (minibuffer:ready? minibuffer)
      (let ((filtered-suggestions (minibuffer:suggestions
                                   (first (minibuffer:sources minibuffer)))))
        (prove:is (mapcar #'minibuffer:value filtered-suggestions)
                  '("foo"))))))

(prove:subtest "Asynchronous suggestion notifications"
  (let* ((suggestion-values '("foobar" "foobaz"))
         (source (minibuffer:make-source
                  :initial-suggestions suggestion-values
                  :filter #'slow-identity-match))
         (minibuffer (minibuffer:make
                      :sources (list source))))
    (setf (minibuffer:input minibuffer) "foo")
    (sera:nlet query-suggestions ((computed-count 1))
      (calispel:fair-alt
        ((calispel:? (minibuffer:ready-notifier source))
         (prove:is (length (minibuffer:suggestions source))
                   (length suggestion-values)))
        ((calispel:? (minibuffer:update-notifier source))
         (prove:is (length (minibuffer:suggestions source))
                   computed-count)
         (query-suggestions (1+ computed-count)))))))

(prove:subtest "Asynchronous suggestion interrupt"
  (let* ((suggestion-values '("foobar" "foobaz"))
         (source (minibuffer:make-source
                  :initial-suggestions suggestion-values
                  :filter #'slow-identity-match))
         (minibuffer (minibuffer:make
                      :sources (list source))))
    (let ((before-input (get-internal-real-time)))
      (setf (minibuffer:input minibuffer) "foo")
      (setf (minibuffer:input minibuffer) "bar")
      (setf (minibuffer:input minibuffer) "baz")
      (prove:is (/ (- (get-internal-real-time) before-input)
                   internal-time-units-per-second)
                0.01
                :test #'<
                "Consecutive inputs happened fast enough"))))

(prove:finalize)
