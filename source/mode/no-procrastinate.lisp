;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/no-procrastinate-mode
    (:use :common-lisp :nyxt :nyxt/blocker-mode)
  (:documentation "Block resource queries for listed hosts."))
(in-package :nyxt/no-procrastinate-mode)
(use-nyxt-package-nicknames)

(sera:export-always '*default-hostlist-no-procrastinate*)
(defparameter *default-hostlist-no-procrastinate*
  (make-instance 'hostlist
                 :url (quri:uri "https://raw.githubusercontent.com/atlas-engineer/default-hosts-no-procrastinate/main/hosts")
                 :base-path #p"hostlist-no-procrastinate.txt")
  "Default hostlist for `no-procrastinate-mode'.")

(define-mode no-procrastinate-mode (nyxt/blocker-mode:blocker-mode)
  "Mode to block access to hosts associated to procrastination."
  ((rememberable-p nil)
   (load-hostlists-p t)
   (style (theme:themed-css (theme *browser*)
            ("summary"
             :background-color theme:secondary
             :color            theme:background
             :font-size        "14px"
             :padding          "16px"
             :margin           "6px"
             :width            "100%"
             :border           "none"
             :outline          "none"
             :text-align       "left")))
   (nyxt/blocker-mode:hostlists
    (list (nyxt/blocker-mode:make-hostlist
           :hosts (mapcar #'(lambda (y) (hostname y))
                          (nfiles:content (no-procrastinate-hosts-file (current-buffer)))))
          nyxt/blocker-mode:*default-hostlist*))))

(defun group-no-procrastinate-hosts (buffer)
  (let ((no-procrastinate-hosts-table (make-hash-table :test #'equalp))
        (no-procrastinate-hosts (nfiles:content (no-procrastinate-hosts-file buffer))))
    (dolist (no-procrastinate-host no-procrastinate-hosts)
      (let ((tags (tags no-procrastinate-host)))
        (if tags
            (dolist (tag tags)
              (push no-procrastinate-host (gethash tag no-procrastinate-hosts-table nil)))
            (push no-procrastinate-host (gethash tags no-procrastinate-hosts-table nil)))))
    no-procrastinate-hosts-table))

(define-internal-page-command-global list-no-procrastinate-hosts ()
    (no-procrastinate-hosts-buffer "*No Procrastinate Hosts*" 'no-procrastinate-mode)
  "List all hosts to avoid procrastination in a new buffer."
  (let ((no-procrastinate-hosts (group-no-procrastinate-hosts no-procrastinate-hosts-buffer)))
    (spinneret:with-html-string
      (:style (style (find-mode no-procrastinate-hosts-buffer 'no-procrastinate-mode)))
      (:h1 "Hosts to avoid procrastination")
      (:body
       (if (zerop (hash-table-count no-procrastinate-hosts))
           (format nil "No hosts to avoid procrastination in ~s."
                   (nfiles:expand (no-procrastinate-hosts-file no-procrastinate-hosts-buffer)))
           (maphash
            (lambda (tag no-procrastinate-hosts)
              (:details
               (:summary (or tag "Unsorted"))
               (dolist (host no-procrastinate-hosts)
                 (let ((url-display (render-url (url host)))
                       (url-href (render-url (url host))))
                   (:div :class "host-entry"
                         (:p (:b "Host: ") (hostname host))
                         (:p (:b "Homepage's Title: ") (title host))
                         (:p (:b "Homepage's URL: ") (:a :href url-href
                                              url-display))
                         (:p (:b "Tags: ")
                             (when (tags host)
                               (format nil " (~{~a~^, ~})" (tags host))))
                         (:p (:button :class "button"
                                      :onclick
                                      (ps:ps
                                        (let ((section (ps:chain document active-element
                                                                 (closest ".host-entry"))))
                                          (ps:chain section parent-node (remove-child section))
                                          (nyxt/ps:lisp-eval
                                           `(delete-no-procrastinate-host ,url-href))))
                                      "Delete"))
                         (:hr ""))))))
            no-procrastinate-hosts))))))
