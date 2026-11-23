;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(unless lparallel:*kernel* (setf lparallel:*kernel*
                                 (lparallel:make-kernel (or (serapeum:count-cpus) 1))))

(define-class test-source (prompter:source)
  ;; The number of suggestions is only relevant to assess the algorithm's
  ;; performance, so it suffices to use a single suggestion for general testing.
  ((prompter:name "Test")
   (prompter:constructor '("suggestion"))))

(defun set-test-source-object-attributes (widths)
  (handler-bind ((warning #'muffle-warning))
    (defmethod prompter:object-attributes ((object string) (source test-source))
      (loop for width in widths
            for i from 1
            collect `(,(str:concat "Attribute-" (princ-to-string i))
                      ,(when width (make-string width))
                      (:width ,width))))))

(define-test uniform-width-attributes ()
  (let ((size (sera:random-in-range 1 11))
        (width (sera:random-in-range 1 11)))
    (set-test-source-object-attributes (make-list size :initial-element width))
    (assert-equal (make-list size :initial-element (/ width (* width size)))
                  (nyxt::attribute-widths (make-instance 'test-source)))))

(define-test fallback-on-unallocated-attributes-widths ()
  "Fallback to uniform width distribution when at least one ratio isn't specified."
  (set-test-source-object-attributes (list 1 2 3 nil))
  (assert-equal (make-list 4 :initial-element 1/4)
                (nyxt::attribute-widths (make-instance 'test-source))))

(define-test preserve-ratios-on-inactive-attributes ()
  "Preserve original proportions between attributes' width on disable."
  (set-test-source-object-attributes (list 1 2 3))
  (let ((source (make-instance 'test-source)))
    (setf (slot-value source 'prompter:active-attributes-keys) '("Attribute-1" "Attribute-2"))
    (assert-equal (list 1/3 2/3)
                  (nyxt::attribute-widths source))
    (setf (slot-value source 'prompter:active-attributes-keys) '("Attribute-2" "Attribute-3"))
    (assert-equal (list 2/5 3/5)
                  (nyxt::attribute-widths source))
    (setf (slot-value source 'prompter:active-attributes-keys) '("Attribute-1" "Attribute-3"))
    (assert-equal (list 1/4 3/4)
                  (nyxt::attribute-widths source))))

(define-test composite-source-lists-top-suggestions-from-each-source ()
  (let* ((alpha-action (lambda (values) (list :alpha values)))
         (beta-action (lambda (values) (list :beta values)))
         (alpha (make-instance 'prompter:source
                               :name "Alpha"
                               :filter nil
                               :filter-preprocessor #'prompter:filter-exact-match
                               :actions-on-return alpha-action
                               :constructor '("a1" "a2" "a3")))
         (beta (make-instance 'prompter:source
                              :name "Beta"
                              :filter nil
                              :filter-preprocessor #'prompter:filter-exact-match
                              :actions-on-return beta-action
                              :constructor '("b1" "b2" "b3")))
         (sources (nyxt::add-composite-source (list alpha beta)))
         (composite (first sources)))
    (assert-equal 1 (prompter:suggestions-per-source composite))
    (setf (prompter:suggestions-per-source composite) 2)
    (let ((prompt (prompter:make :sources sources)))
      (unwind-protect
           (progn
             (assert-eq composite (first (prompter:sources prompt)))
             (assert-true (bordeaux-threads:with-timeout (5)
                            (prompter:all-ready-p prompt)))
             (assert-equal '("a1" "a2" "b1" "b2")
                           (mapcar #'prompter:value (prompter:suggestions composite)))
             (assert-equal '("Alpha" "Alpha" "Beta" "Beta")
                           (mapcar (lambda (suggestion)
                                     (prompter:name (prompter:match-data suggestion)))
                                   (prompter:suggestions composite)))
             (setf (prompter::current-suggestion prompt) (list composite 0))
             (assert-eq alpha-action (first (prompter:actions-on-return prompt)))
             (setf (prompter::current-suggestion prompt) (list composite 2))
             (assert-eq beta-action (first (prompter:actions-on-return prompt)))
             (setf (prompter:input prompt) "a1")
             (assert-true (bordeaux-threads:with-timeout (5)
                            (prompter:all-ready-p prompt)))
             (assert-equal '("a1")
                           (mapcar #'prompter:value (prompter:suggestions composite)))
             (setf (prompter:input prompt) "")
             (assert-true (bordeaux-threads:with-timeout (5)
                            (prompter:all-ready-p prompt)))
             (assert-equal '("a1" "a2" "b1" "b2")
                           (mapcar #'prompter:value (prompter:suggestions composite))))
        (prompter:destroy prompt)))))

(defun prompt-source-node (view source)
  (car (gethash source (user-interface::nodes view))))

(define-test prompt-suggestions-keep-an-unchanged-source ()
  (let* ((*browser* (make-instance 'browser))
         (alpha (make-instance 'prompter:source
                               :name "Alpha"
                               :filter nil
                               :enable-marks-p t
                               :constructor '("one" "two" "three")))
         (beta (make-instance 'prompter:source
                              :name "Beta"
                              :filter nil
                              :constructor '("beta-one")))
         (prompt (make-instance 'prompt-buffer
                                :prompt "Pick"
                                :sources (list alpha beta))))
    (unwind-protect
         (progn
           (assert-true (bordeaux-threads:with-timeout (5)
                          (prompter:all-ready-p prompt)))
           (setf (prompter::current-suggestion prompt) (list alpha 0)
                 (prompter:marks alpha) '("one"))
           (assert-equal (list alpha beta)
                         (nyxt::prompt-visible-sources prompt))
           (let ((view (make-instance
                        'user-interface:dynamic-element
                        :template (lambda (source)
                                    (nyxt::prompt-source-view prompt source))
                        :item-key (lambda (source)
                                    (nyxt::prompt-source-stamp prompt source)))))
             (setf (user-interface:items view) (list alpha beta))
             (let ((html (spinneret:with-html-string
                           (user-interface:to-html view)))
                   (alpha-node (prompt-source-node view alpha))
                   (beta-node (prompt-source-node view beta)))
               (assert-true (search "Alpha" html))
               (assert-true (search "Beta" html))
               (assert-true (search "one" html))
               (assert-true (search "selection" html))
               (assert-true (search "marked" html))
               (assert-true (search "checkbox" html))
               (assert-true (search "source-content" html))
               (setf (user-interface:items view) (list alpha beta))
               (assert-eq alpha-node (prompt-source-node view alpha))
               (assert-eq beta-node (prompt-source-node view beta))
               (setf (prompter::current-suggestion prompt) (list alpha 1))
               (setf (user-interface:items view) (list alpha beta))
               (assert-false (eq alpha-node (prompt-source-node view alpha)))
               (assert-eq beta-node (prompt-source-node view beta)))))
      (prompter:destroy prompt))))

(define-test buffer-prompt-shows-stored-keywords-only ()
  (let* ((*browser* (make-instance 'browser))
         (ready (make-instance 'web-buffer
                               :title "Ready"
                               :url (quri:uri "https://example.com/ready")))
         (waiting (make-instance 'web-buffer
                                 :title "Waiting"
                                 :url (quri:uri "https://example.com/waiting")))
         (source (make-instance 'prompter:source :name "Attributes")))
    (setf (nyxt::keywords ready) '(("nyxt" 1) ("lisp" 1)))
    (assert-string= "nyxt lisp"
                    (second (assoc "Keywords"
                                   (prompter:object-attributes ready source)
                                   :test #'string=)))
    (assert-string= ""
                    (second (assoc "Keywords"
                                   (prompter:object-attributes waiting source)
                                   :test #'string=)))))

(define-test prompt-source-shows-a-window-around-the-cursor ()
  (let* ((*browser* (make-instance 'browser))
         (source (make-instance 'prompter:source
                                :name "Rows"
                                :filter nil
                                :constructor (loop for index from 0 below 10
                                                   collect (format nil "row-~d" index))))
         (prompt (make-instance 'prompt-buffer :sources (list source))))
    (unwind-protect
         (progn
           (assert-true (bordeaux-threads:with-timeout (5)
                          (prompter:all-ready-p prompt)))
           (setf (prompter::current-suggestion prompt) (list source 0))
           (let ((html (spinneret:with-html-string
                         (user-interface:to-html
                          (nyxt::prompt-source-view prompt source)))))
             (assert-true (search "row-0" html))
             (assert-true (search "row-7" html))
             (assert-false (search "row-8" html)))
           (setf (prompter::current-suggestion prompt) (list source 9))
           (let ((html (spinneret:with-html-string
                         (user-interface:to-html
                          (nyxt::prompt-source-view prompt source)))))
             (assert-false (search "row-0" html))
             (assert-false (search "row-5" html))
             (assert-true (search "row-6" html))
             (assert-true (search "row-9" html))
             (assert-true (search "selection" html))))
      (prompter:destroy prompt))))

(define-test literal-search-source-is-ready-before-slower-sources ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'buffer))
         (default-sources (url-sources buffer
                                       (lambda (urls)
                                         (buffer-load* urls buffer))))
         (search (first default-sources))
         (completions (second default-sources))
         (history (make-instance 'prompter:source
                                 :name "History"
                                 :filter nil
                                 :constructor '("https://example.com/nyxt-quick")))
         (delayed (make-instance 'prompter:source
                                 :name "Delayed"
                                 :filter nil
                                 :constructor '("late")
                                 :filter-postprocessor
                                 (lambda (suggestions source input)
                                   (declare (ignore source input))
                                   (sleep 1)
                                   suggestions)))
         (sources (nyxt::add-composite-source (list search history delayed)))
         (composite (first sources))
         (prompt (prompter:make :input "nyxt-quick" :sources sources)))
    (unwind-protect
         (let ((preprocessor (prompter:filter-preprocessor search)))
           (assert-true (bordeaux-threads:with-timeout (1)
                          (loop until (and (prompter:ready-p search)
                                           (prompter:ready-p history)
                                           (prompter:suggestions composite))
                                do (sleep 0.01)
                                finally (return t))))
           (assert-false (prompter:ready-p delayed))
           (let ((first (prompter:value (first (prompter:suggestions composite)))))
             (assert-eq :search-query (kind first))
             (assert-string= "nyxt-quick" (search-query first))
             (assert-string= "https://duckduckgo.com/?q=nyxt-quick"
                             (render-url (url first))))
           (assert-string= "https://example.com/nyxt-quick"
                           (prompter:value (second (prompter:suggestions composite))))
           (assert-eq nil (funcall preprocessor nil search "  "))
           (assert-eq :url (kind (first (funcall preprocessor
                                                 nil search "https://nyxt.atlas.engineer"))))
           (assert-equal '() (funcall (prompter:filter-postprocessor completions)
                                      nil completions "https://nyxt.atlas.engineer")))
      (prompter:destroy prompt))))

(define-test prompt-row-selection-targets-exact-source-instance ()
  (let* ((*browser* (make-instance 'browser))
         (first-source (make-instance 'prompter:source
                                      :name "First"
                                      :filter nil
                                      :constructor '("alpha")))
         (second-source (make-instance 'prompter:source
                                       :name "Second"
                                       :filter nil
                                       :constructor '("beta")))
         (prompt (make-instance 'prompt-buffer
                                :sources (list first-source second-source))))
    (unwind-protect
         (progn
           (assert-true (bordeaux-threads:with-timeout (5)
                          (prompter:all-ready-p prompt)))
           (assert-true (nyxt::select-prompt-source-suggestion prompt second-source 0))
           (assert-string= "beta" (current-suggestion-value prompt))
           (assert-false (nyxt::select-prompt-source-suggestion prompt second-source 9)))
      (prompter:destroy prompt))))
