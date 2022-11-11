;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/diff-mode
    (:documentation "Mode for viewing diffs between two buffers."))
(in-package :nyxt/diff-mode)

(export-always 'diff-mode)
(define-mode diff-mode ()
  "Diff mode is used to view the diffs between two buffers."
  ((style (theme:themed-css (theme *browser*)
            (".nyxt-diff-insert"
             :background-color "#bbeabb"
             :text-decoration "none")
            ("ins.nyxt-diff-replace"
             :background-color "#bbeabb"
             :text-decoration "none")
            (".nyxt-diff-delete"
             :background-color "#efcbcf"
             :text-decoration "none")
            ("del.nyxt-diff-replace"
             :background-color "#efcbcf"
             :text-decoration "none"))
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
