;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/buffer-listing
  (:documentation "Package for `buffer-listing-mode', mode for buffer listing."))
(in-package :nyxt/mode/buffer-listing)

(define-mode buffer-listing-mode ()
  "Mode for buffer-listing.
Hosts `list-buffers' page and `buffers-panel' panel."
  ((visible-in-status-p nil))
  (:toggler-command-p nil))

(define-internal-page-command-global list-buffers (&key (cluster nil)
                                                  linear-view-p) ; TODO: Document `cluster'.
    (listing-buffer "*Buffers*" 'nyxt/mode/buffer-listing:buffer-listing-mode)
  "Show all buffers and their interrelations.

When a buffer is spawned from another one (e.g. by middle-clicking on a link),
we say that a child buffer originates from a parent one. These kind of
relationships define a buffer tree.  When LINEAR-VIEW-P is non-nil, buffers are
shown linearly instead."
  (labels ((cluster-buffers ()
             "Return buffers as hash table, where each value is a cluster (list of documents)."
             (let ((collection (make-instance 'analysis::document-collection)))
               (loop for buffer in (buffer-list)
                     do (with-current-buffer buffer
                          (analysis::add-document
                           collection
                           (make-instance 'analysis::document-cluster
                                          :source buffer
                                          :string-contents (document-get-paragraph-contents)))))
               (analysis::tf-vectorize-documents collection)
               (analysis::generate-document-distance-vectors collection)
               (analysis::dbscan collection :minimum-points 1 :epsilon 0.075)
               (analysis::clusters collection)))
           (buffer-markup (buffer)
             "Present a buffer in HTML."
             ;; To avoid spurious spaces.
             ;; See https://github.com/ruricolist/spinneret/issues/37.
             (let ((*print-pretty* nil))
               (spinneret:with-html
                 (:p (:nbutton
                       :text "✕"
                       :title "Delete buffer"
                       `(nyxt::delete-buffer :buffers ,buffer)
                       `(reload-buffer ,listing-buffer))
                     (:nbutton
                       :text "→"
                       :title "Switch to buffer"
                       `(nyxt::switch-buffer :buffer ,buffer))
                     (:a :href (render-url (url buffer))
                         (if (uiop:emptyp (title buffer))
                             (render-url (url buffer))
                             (title buffer)))))))
           (buffer-tree->html (root-buffer)
             "Present a single buffer tree in HTML."
             (spinneret:with-html
               (:div (buffer-markup root-buffer))
               (:ul
                (dolist (child-buffer (nyxt::buffer-children root-buffer))
                  (:li (buffer-tree->html child-buffer))))))
           (cluster-markup (cluster-id cluster)
             "Present a cluster in HTML."
             (spinneret:with-html
               (:div (:h2 (format nil "Cluster ~a" cluster-id))
                     (loop for document in cluster
                           collect (buffer-markup (analysis::source document)))))))
    (spinneret:with-html-string
      (:h1 "Buffers")
      (:nbutton
        :text "Tree display"
        '(nyxt/mode/buffer-listing::list-buffers))
      (:nbutton
        :text "Linear display"
        '(nyxt/mode/buffer-listing::list-buffers :linear-view-p t))
      (:br)
      (:div
       (if cluster
           (loop for cluster-key being the hash-key
                   using (hash-value cluster) of (cluster-buffers)
                 collect (cluster-markup cluster-key cluster))
           (dolist (buffer (buffer-list))
             (if linear-view-p
                 (buffer-markup buffer)
                 (unless (nyxt::buffer-parent buffer)
                   (buffer-tree->html buffer)))))))))

;; FIXME: It's a terribly confusing panel:
;; - Buffer as a button is quite confusing and does not match the idea of a
;;   switchable-to entity better separate action (switch, delete) from
;;   presentation.
(define-panel-command-global buffers-panel ()
    (panel-buffer "*Buffers panel*")
  "Display a list of buffers with easy switching."
  (flet ((buffer-markup (buffer)
           "Create the presentation for a buffer."
           (spinneret:with-html-string
             (:p (:nbutton :text (title buffer)
                   :buffer panel-buffer
                   `(nyxt::switch-buffer :buffer ,buffer))))))
    (spinneret:with-html-string
      (:nstyle '(.button
                 :white-space nowrap
                 :overflow-x hidden
                 :display block
                 :text-overflow ellipsis))
      (:body
       (:nbutton :text "Update ↺"
         :style "background-color:green;"
         :buffer panel-buffer
         `(reload-buffer
           (find
            (render-url (url ,panel-buffer))
            (nyxt::panel-buffers (current-window))
            :test #'string=
            :key (compose
                  #'render-url #'url))))
       (:h1 "Buffers")
       (loop for buffer in (buffer-list)
             collect (:raw (buffer-markup buffer)))))))
