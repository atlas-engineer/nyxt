;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/buffer-listing-mode
  (:use :common-lisp :nyxt)
  (:documentation "Mode for buffer-listings"))
(in-package :nyxt/buffer-listing-mode)

(define-mode buffer-listing-mode ()
  "Mode for buffer-listing."
  ((rememberable-p nil)))

(define-command-global list-buffers (&key (cluster nil)
                                     linear-view-p) ; TODO: Document `cluster'.
  "Show a buffer listing all buffer trees.
Buffers have relationships.  When a buffer is spawned from another one (e.g. by
middle-clicking on a link), the new buffer is a child buffer.
This kind of relationships creates 'trees' of buffers.

With LINEAR-VIEW-P, list buffers linearly instead."
  (labels ((cluster-buffers ()
             "Return buffers as hash table, where each value is a cluster (list of documents)."
             (let ((collection (make-instance 'analysis::document-collection)))
               (loop for buffer in (buffer-list)
                     unless (internal-buffer-p buffer)
                     do (with-current-buffer buffer
                          (analysis::add-document
                           collection
                           (make-instance 'analysis::document-cluster
                                          :source buffer
                                          :string-contents (document-get-paragraph-contents)))))
               (analysis::tf-vectorize-documents collection)
               (analysis::generate-document-distance-vectors collection)
               (analysis::dbscan collection :minimum-points 3 :epsilon 0.065)
               (analysis::clusters collection)))
           (buffer-markup (buffer)
             "Present a buffer in HTML."
             ;; To avoid spurious spaces.
             ;; See https://github.com/ruricolist/spinneret/issues/37.
             (let ((*print-pretty* nil))
               (spinneret:with-html
                 (:p (:a :class "button"
                         :href (lisp-url `(nyxt::delete-buffer :id ,(id buffer))) "✕")
                     (:a :class "button"
                         :href (lisp-url `(nyxt::switch-buffer :id ,(id buffer))) "→")
                     (:span (title buffer) "  "
                            (:u (render-url (url buffer))))))))
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
                           collect (buffer-markup (analysis::source document))))))
           (internal-buffers-markup ()
             "Present the internal buffers in HTML."
             (spinneret:with-html
               (:div (:h2 "Internal Buffers")
                     (loop for buffer in (buffer-list)
                           when (internal-buffer-p buffer)
                           collect (buffer-markup buffer))))))
    (with-current-html-buffer (buffer "*Buffers*" 'nyxt/buffer-listing-mode:buffer-listing-mode)
      (spinneret:with-html-string
        (:style (style buffer))
        (:h1 "Buffers")
        (:a :class "button"
            :href (lisp-url '(nyxt/buffer-listing-mode::list-buffers))
            "Tree display")
        (:a :class "button"
            :href (lisp-url '(nyxt/buffer-listing-mode::list-buffers :linear-view-p t))
            "Linear display")
        (:br "")
        (:div
         (if cluster
             (append (list (internal-buffers-markup))
                     (loop for cluster-key being the hash-key
                             using (hash-value cluster) of (cluster-buffers)
                           collect (cluster-markup cluster-key cluster)))
             (dolist (buffer (buffer-list))
               (if linear-view-p
                   (buffer-markup buffer)
                   (unless (nyxt::buffer-parent buffer)
                     (buffer-tree->html buffer))))))))))

(define-command-global show-buffers-panel (&key (side :left))
  "Show the bookmarks in a panel."
  (flet ((buffer-markup (buffer)
           "Create the presentation for a buffer."
           (spinneret:with-html
             (:p (:a :class "button"
                     :href (lisp-url `(nyxt::switch-buffer :id ,(id buffer)))
                     (:span :title (title buffer) :class "title" (title buffer)))))))
    (nyxt::with-current-panel (panel-buffer "*Buffers Panel*" :side side)
      (spinneret:with-html-string (:style (style panel-buffer))
                                  (:style (cl-css:css
                                           '((".button"
                                              :white-space "nowrap"
                                              :overflow-x "hidden"
                                              :display "block"
                                              :text-overflow "ellipsis"))))
                                  (:body
                                   (:h1 "Buffers")
                                   (:a :class "button" :href (lisp-url '(nyxt/buffer-listing-mode::show-buffers-panel)) "Update ↺")
                                   (loop for buffer in (buffer-list)
                                         collect (buffer-markup buffer)))))))
