;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/integration)

(defun current-url ()
  (nyxt:render-url (nyxt:url (nyxt:current-buffer))))

(define-test open-url-from-the-prompt ()
  (with-page ((article-url) "The article body" :page-title "Article")
    (drive-prompts
     (lambda () (nyxt:set-url))
     (list (lambda (prompt)
             (accept-prompt prompt
                            :input (home-url)
                            :shows '("Open URL" "integration-home")))))
    (wait-for-text "The lighthouse keeper")
    (assert-string= (home-url) (current-url))
    (assert-true (search "Gazette" (nyxt:title (nyxt:current-buffer))))))

(define-test status-bar-shows-the-open-pages ()
  (with-page ((home-url) "The lighthouse keeper" :page-title "Gazette")
    (let ((status (nyxt:status-buffer (nyxt:current-window))))
      (wait-for-text "integration-home" :buffer status)
      (open-page (article-url) "The article body" :page-title "Article")
      (wait-for-text "integration-article" :buffer status)
      (let ((tabs (element-text status "#tabs")))
        (assert-true (search "integration-home" tabs))
        (assert-true (search "integration-article" tabs))))))

(define-test scroll-the-page ()
  (with-page ((home-url) "Far end" :page-title "Gazette")
    (let ((buffer (nyxt:current-buffer)))
      (wait-until (lambda ()
                    (nyxt:ps-eval :buffer buffer
                      (> (ps:@ document document-element scroll-height)
                         (ps:@ window inner-height))))
                  :description "a page taller than the window")
      (nyxt/mode/document:scroll-to-bottom)
      (wait-until (lambda () (> (scroll-y buffer) 500))
                  :description "the bottom of the page")
      (nyxt/mode/document:scroll-to-top)
      (wait-until (lambda () (< (scroll-y buffer) 50))
                  :description "the top of the page")
      (nyxt/mode/document:scroll-page-down)
      (wait-until (lambda () (> (scroll-y buffer) 50))
                  :description "the next screen")
      (nyxt/mode/document:scroll-up :y-pixels 10)
      (let ((before (scroll-y buffer)))
        (nyxt/mode/document:scroll-down :y-pixels 80)
        (wait-until (lambda () (> (scroll-y buffer) before))
                    :description "a short scroll down")
        (assert-true (> (scroll-y buffer) before))))))

(define-test zoom-the-page ()
  (with-page ((home-url) "Gazette" :page-title "Gazette")
    (let* ((buffer (nyxt:current-buffer))
           (start (nyxt:ffi-buffer-zoom-ratio buffer)))
      (nyxt/mode/document:zoom-page)
      (wait-until (lambda () (> (nyxt:ffi-buffer-zoom-ratio buffer) start))
                  :description "a larger zoom")
      (nyxt/mode/document:unzoom-page)
      (wait-until (lambda ()
                    (< (abs (- (nyxt:ffi-buffer-zoom-ratio buffer) start)) 0.05))
                  :description "the previous zoom")
      (nyxt/mode/document:zoom-page)
      (nyxt/mode/document:reset-page-zoom)
      (wait-until (lambda ()
                    (< (abs (- (nyxt:ffi-buffer-zoom-ratio buffer) 1.0)) 0.05))
                  :description "the default zoom")
      (assert-true (< (abs (- (nyxt:ffi-buffer-zoom-ratio buffer) 1.0)) 0.05)))))

(define-test search-the-page ()
  (with-page ((home-url) "lighthouse" :page-title "Gazette")
    (drive-prompts
     (lambda () (nyxt/mode/search-buffer:search-buffer))
     (list (lambda (prompt)
             (accept-prompt prompt
                            :input "lighthouse"
                            :shows '("Search text" "lighthouse"))
             (wait-until (lambda () (>= (search-mark-count) 3))
                         :description "search marks"))))
    (assert-true (>= (search-mark-count) 3))
    (nyxt/mode/search-buffer:remove-search-marks)
    (wait-until (lambda () (zerop (search-mark-count)))
                :description "search marks to clear")
    (assert-true (zerop (search-mark-count)))))

(define-test jump-to-a-heading ()
  (with-page ((home-url) "Far end" :page-title "Gazette")
    (nyxt/mode/document:scroll-to-top)
    (wait-until (lambda () (< (scroll-y) 50)) :description "the top of the page")
    (drive-prompts
     (lambda () (nyxt/mode/document:jump-to-heading))
     (list (lambda (prompt)
             (accept-prompt prompt
                            :input "Far end"
                            :shows '("Jump to heading" "Far end")
                            :suggestion-p
                            (lambda (heading)
                              (search "Far end" (nyxt:title heading)))))))
    (wait-until (lambda () (> (scroll-y) 500))
                :description "the far heading")
    (assert-true (> (scroll-y) 500))))

(define-test follow-a-link-by-hint ()
  (with-page ((home-url) "Read the article" :page-title "Gazette")
    (drive-prompts
     (lambda () (nyxt/mode/hint:follow-hint))
     (list (lambda (prompt)
             (accept-prompt prompt
                            :input "integration-article"
                            :shows '("Select elements" "integration-article")
                            :suggestion-p
                            (lambda (element)
                              (search "integration-article"
                                      (nyxt:render-url (nyxt:url element))))))))
    (wait-for-text "The article body")
    (assert-string= (article-url) (current-url))))

(define-test go-back-and-forward ()
  (with-page ((home-url) "The lighthouse keeper" :page-title "Gazette")
    (load-page (article-url) "The article body" :page-title "Article")
    (assert-string= (article-url) (current-url))
    (nyxt/mode/history:history-backwards)
    (wait-for-text "The lighthouse keeper")
    (assert-string= (home-url) (current-url))
    (nyxt/mode/history:history-forwards)
    (wait-for-text "The article body")
    (assert-string= (article-url) (current-url))))

(define-test open-a-url-in-a-new-buffer ()
  (with-page ((home-url) "Gazette" :page-title "Gazette")
    (let ((before (length (nyxt:buffer-list))))
      (drive-prompts
       (lambda () (nyxt:set-url-new-buffer))
       (list (lambda (prompt)
               (accept-prompt prompt
                              :input (article-url)
                              :shows '("Open URL" "integration-article")))))
      (wait-for-text "The article body")
      (assert-string= (article-url) (current-url))
      (assert-equal (1+ before) (length (nyxt:buffer-list)))
      (assert-true (find (home-url) (nyxt:buffer-list)
                         :key (lambda (buffer) (nyxt:render-url (nyxt:url buffer)))
                         :test #'string=)))))

(define-test switch-between-buffers ()
  (with-page ((home-url) "Gazette" :page-title "Gazette")
    (open-page (article-url) "The article body" :page-title "Article")
    (drive-prompts
     (lambda () (nyxt:switch-buffer))
     (list (lambda (prompt)
             (accept-prompt prompt
                            :input "Gazette"
                            :shows '("Switch to buffer" "Gazette")
                            :suggestion-p
                            (lambda (buffer)
                              (search "Gazette" (nyxt:title buffer)))))))
    (wait-until (lambda () (string= (home-url) (current-url)))
                :description "the gazette buffer")
    (nyxt:switch-buffer-next)
    (wait-until (lambda () (string= (article-url) (current-url)))
                :description "the next buffer")
    (nyxt:switch-buffer-previous)
    (wait-until (lambda () (string= (home-url) (current-url)))
                :description "the previous buffer")
    (assert-string= (home-url) (current-url))))

(define-test reload-the-page ()
  (with-page ((home-url) "The lighthouse keeper" :page-title "Gazette")
    (let ((done nil))
      (hooks:once-on (nyxt:buffer-loaded-hook (nyxt:current-buffer)) (buffer)
        (setf done t))
      (nyxt:reload-current-buffer)
      (wait-until (lambda () done) :description "the page to reload"))
    (wait-for-text "The lighthouse keeper")
    (assert-string= (home-url) (current-url))))

(define-test copy-and-paste ()
  (with-page ((home-url) "Gazette" :page-title "Gazette")
    (nyxt/mode/document:focus-first-input-field)
    (wait-until (lambda ()
                  (string-equal "INPUT" (nyxt/mode/document:active-element-tag)))
                :description "the note field")
    (assert-true (string-equal "INPUT" (nyxt/mode/document:active-element-tag)))
    ;; Copy uses the system clipboard. A Wayland session without wl-copy cannot
    ;; complete that step; focusing the field above is still checked.
    (handler-case
        (progn
          (nyxt/mode/message:clear-messages)
          (nyxt:copy-title)
          (wait-for-text "Gazette"
                         :buffer (nyxt:message-buffer (nyxt:current-window)))
          (let ((clip (trivial-clipboard:text)))
            (assert-true (and (stringp clip) (search "Gazette" clip))))
          (nyxt/mode/document:paste)
          (wait-until (lambda ()
                        (search "Gazette"
                                (input-value (nyxt:current-buffer) "#note")))
                      :description "the pasted title")
          (nyxt:copy-url)
          (nyxt/mode/message:list-messages)
          (wait-for-text (home-url))
          (assert-true (search "Messages" (page-text))))
      (error (condition)
        (unless (search "wl-copy" (princ-to-string condition))
          (error condition))))))

(define-test bookmark-and-revisit ()
  (with-page ((home-url) "Gazette" :page-title "Gazette")
    (clear-bookmarks)
    (drive-prompts
     (lambda () (nyxt/mode/bookmark:add-bookmark))
     (list (lambda (prompt)
             (accept-prompt prompt
                            :source 1
                            :shows '("Add Bookmark" "Gazette")
                            :suggestion-p (lambda (suggestion) (typep suggestion 'nyxt:buffer))))
           (lambda (prompt)
             (accept-prompt prompt :shows '("Title for")))
           (lambda (prompt)
             ;; Hiding the title prompt shows the bookmark prompt again
             ;; before the tag prompt opens.
             (if (search "Tag" (prompter:prompt prompt))
                 (accept-prompt prompt :shows '("Tag"))
                 :ignore))))
    (wait-until (lambda ()
                  (find (home-url)
                        (files:content (nyxt/mode/bookmark:bookmarks-file
                                        (nyxt:current-buffer)))
                        :key (lambda (entry)
                               (nyxt:render-url (nyxt:url entry)))
                        :test #'string=))
                :description "the saved bookmark")
    (nyxt/mode/bookmark:list-bookmarks)
    (wait-for-text "Bookmarks")
    (wait-until (lambda ()
                  (search "integration-home"
                          (or (ignore-errors
                               (nyxt:ps-eval
                                 (ps:@ document body |innerHTML|)))
                              "")))
                :description "the bookmark in the list")
    (load-page (article-url) "The article body" :page-title "Article")
    (drive-prompts
     (lambda () (nyxt/mode/bookmark:set-url-from-bookmark))
     (list (lambda (prompt)
             (accept-prompt prompt
                            :shows '("Open bookmark" "integration-home")))))
    (wait-for-text "The lighthouse keeper")
    (assert-string= (home-url) (current-url))
    (drive-prompts
     (lambda () (nyxt/mode/bookmark:delete-bookmark))
     (list (lambda (prompt)
             (accept-prompt prompt
                            :shows '("Delete bookmark" "integration-home")))))
    (wait-until (lambda ()
                  (null (files:content (nyxt/mode/bookmark:bookmarks-file
                                        (nyxt:current-buffer)))))
                :description "the bookmark to be deleted")
    (nyxt/mode/bookmark:list-bookmarks)
    (wait-for-text "No bookmarks")))

(define-test execute-a-command-from-the-prompt ()
  (with-page ((home-url) "The lighthouse keeper" :page-title "Gazette")
    (let ((done nil))
      (hooks:once-on (nyxt:buffer-loaded-hook (nyxt:current-buffer)) (buffer)
        (setf done t))
      (drive-prompts
       (lambda () (nyxt:execute-command))
       (list (lambda (prompt)
               (accept-prompt prompt
                              :input "reload-current-buffer"
                              :shows '("Execute command" "reload-current-buffer")
                              :suggestion-p
                              (lambda (command)
                                (eq (nyxt:name command)
                                    'nyxt:reload-current-buffer))))))
      (wait-until (lambda () done) :description "reload started from the prompt"))
    (wait-for-text "The lighthouse keeper")
    (assert-string= (home-url) (current-url))))

(define-test close-and-reopen-a-buffer ()
  (with-page ((home-url) "Gazette" :page-title "Gazette")
    (open-page (article-url) "The article body" :page-title "Article")
    (nyxt:delete-current-buffer)
    (wait-until (lambda () (string= (home-url) (current-url)))
                :description "the remaining buffer")
    (assert-false (find (article-url) (nyxt:buffer-list)
                        :key (lambda (buffer) (nyxt:render-url (nyxt:url buffer)))
                        :test #'string=))
    (nyxt:reopen-last-buffer)
    (wait-for-text "The article body")
    (assert-string= (article-url) (current-url))))

(define-test review-history-and-open-buffers ()
  (with-page ((home-url) "Gazette" :page-title "Gazette")
    (load-page (article-url) "The article body" :page-title "Article")
    (nyxt/mode/history:list-history)
    (wait-for-text "integration-home")
    (assert-true (search "integration-article" (page-text)))
    (assert-true (search "History" (page-text)))
    (nyxt/mode/buffer-listing:list-buffers)
    (wait-for-text "integration-article")
    (assert-true (search "Buffers" (page-text)))))
