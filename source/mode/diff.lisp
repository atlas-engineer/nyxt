;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/diff-mode
  (:use #:cl #:nyxt)
  (:import-from #:serapeum #:export-always)
  (:documentation "Mode for viewing diffs between two buffers."))

(in-package :nyxt/diff-mode)

(export-always 'diff-mode)
(define-mode diff-mode ()
  "Diff mode is used to view the diffs between two buffers."
  ((rememberable-p nil)
   (style (theme:themed-css (theme *browser*)
            (".nyxt-diff-insert"
             :text-decoration "none"
             :background-color "#bbeabb")
            ("ins.nyxt-diff-replace"
             :text-decoration "none"
             :background-color "#bbeabb")
            (".nyxt-diff-delete"
             :text-decoration "none"
             :background-color "#efcbcf")
            ("del.nyxt-diff-replace"
             :text-decoration "none"
             :background-color "#efcbcf"))
          :documentation "Diff colours for its visual representation.
They're based on the modus-operandi theme by Protesilaos Stavrou, which follows
the highest standard on accessibility."))
  (:toggler-command-p nil))

(export-always 'diff)
(define-internal-page-command-global diff
    (&key (old-buffer-id (id (prompt1 :prompt "Old buffer"
                                      :sources (make-instance 'buffer-source
                                                              :return-actions  nil))))
          (new-buffer-id (id (prompt1 :prompt "New buffer"
                                      :sources (make-instance 'buffer-source
                                                              :return-actions  nil)))))
    (diff-buffer "*diff*" 'diff-mode)
  "Show difference between two buffers"
  (let ((old-html (ffi-buffer-get-document (nyxt::buffers-get old-buffer-id)))
        (new-html (ffi-buffer-get-document (nyxt::buffers-get new-buffer-id))))
    (spinneret:with-html-string
      (:style (style (find-submode 'nyxt/diff-mode:diff-mode diff-buffer)))
      (:raw
       (html-diff:html-diff
        old-html new-html
        :insert-class "nyxt-diff-insert"
        :delete-class "nyxt-diff-delete"
        :replace-class "nyxt-diff-replace")))))
