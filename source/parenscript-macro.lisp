;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; `uiop:define-package' instead of `nyxt:define-package' since it does not
;; depend on Nyxt.
(uiop:define-package :nyxt/parenscript
  (:nicknames :nyxt/ps)
  (:use :cl :parenscript)
  (:import-from :serapeum #:export-always))

(in-package :nyxt/parenscript)
(nyxt:use-nyxt-package-nicknames)

(export-always 'qs)
(defpsmacro qs (context selector)
  "Alias of context.querySelector()"
  `(chain ,context (query-selector ,selector)))

(export-always 'qsa)
(defpsmacro qsa (context selector)
  "Alias of context.querySelectorAll()"
  `(chain ,context (query-selector-all ,selector)))

(export-always 'qs-id)
(defpsmacro qs-id (context id)
  "Alias of context.getElementById()"
  `(chain ,context (get-element-by-id ,id)))

(export-always 'qs-nyxt-id)
(defpsmacro qs-nyxt-id (context id)
  "context.querySelector() tailored for Nyxt IDs."
  `(chain ,context (query-selector (lisp (format nil "[nyxt-identifier=\"~a\"]" ,id)))))

(export-always 'iframe-document)
(defpsmacro iframe-document (iframe)
  `(let ((iframe ,iframe))
     (or (ps:@ iframe content-document)
         (ps:@ iframe content-window document))))

(export-always 'active-element)
(defpsmacro active-element (context)
  "A smarter active element search in CONTEXT, aware of arbitrarily nested iframes."
  `(labels ((find-actual-active-element (element)
              (let ((active (@ element active-element)))
                (when active
                  (if (not (equal (@ active tag-name) "IFRAME"))
                      active
                      (find-actual-active-element (iframe-document active)))))))
     (find-actual-active-element ,context)))

(defpsmacro get-caret ()
  `(let* ((element (active-element document))
          (tag-name (chain element tag-name)))
     (cond
       ((or (string= tag-name "INPUT") (string= tag-name "TEXTAREA"))
        (list (chain element selection-start) (chain element selection-end)))
       ((chain element is-content-editable)
        (let ((range (chain window (get-selection) (get-range-at 0))))
          (list (@ range start-offset) (@ range end-offset)))))))

(defpsmacro set-caret (element &optional start end)
  `(let* ((element ,element)
          (tag-name (chain element tag-name))
          (start ,start)
          (end ,end))
     (unless (active-element document)
       (chain element (focus)))
     (cond
       ((or (string= tag-name "INPUT")
            (string= tag-name "TEXTAREA"))
        (setf (chain element selection-start) (or start nil)
              (chain element selection-end) (or end start nil)))
       ((chain element is-content-editable)
        (let* ((selection (chain window (get-selection)))
               (range (chain document (create-range))))
          (when (and selection (chain selection (get-range-at 0)))
            (chain selection (remove-all-ranges)))
          (when start
            (chain range (set-start element start))
            (if end
                (chain range (set-end element end))
                (chain range (set-end element start)))
            (chain window (get-selection) (add-range range))))))))

(export-always 'insert-at)
(defpsmacro insert-at (tag input-text)
  "Insert text at a tag."
  `(let* ((element ,tag)
          (caret (get-caret))
          (origin (@ caret 0))
          (end (or (@ caret 1) origin))
          (tag-name (chain element tag-name)))
     (cond
       ((or (string= tag-name "INPUT")
            (string= tag-name "TEXTAREA"))
        (setf (chain element value)
              (+ (chain element value (substring 0 origin))
                 ,input-text
                 (chain element value
                        (substring end (chain element value length))))))
       ((chain element is-content-editable)
        ;; TODO: Implement caret movement, as in
        ;; https://stackoverflow.com/questions/6249095/how-to-set-the-caret-cursor-position-in-a-contenteditable-element-div
        (setf (chain element inner-text)
              (+ (chain element inner-text (substring 0 origin))
                 ,input-text
                 (chain element inner-text
                        (substring end
                                   (chain element inner-text length)))))))
     (set-caret
      (if (= origin end)
          (+ origin (chain ,input-text length))
          origin)
      (+ origin (chain ,input-text length)))))

(export-always 'element-editable-p)
(defpsmacro element-editable-p (element)
  "Whether ELEMENT is editable."
  `(let ((tag (chain ,element tag-name)))
     (if (or (and (string= tag "INPUT")
                  ;; The list of all input types:
                  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
                  (not (chain ([] "hidden" "checkbox" "button") (includes (chain ,element type))))
                  (not (chain ,element disabled)))
             (string= tag "TEXTAREA")
             (chain ,element is-content-editable))
         t f)))

(export-always 'element-drawable-p)
(defpsmacro element-drawable-p (element)
  "Whether ELEMENT is drawable."
  `(if (or (chain ,element offset-width)
           (chain ,element offset-height)
           (chain ,element (get-client-rects) length))
       t f))

(export-always 'element-in-view-port-p)
(defpsmacro element-in-view-port-p (element)
  "Whether ELEMENT is in viewport."
  `(let* ((rect (chain ,element (get-bounding-client-rect)))
          (computed-style (chain window (get-computed-style ,element))))
     (if (and (>= (chain rect top) 0)
              ;; a partially visible element is still visible
              (<= (chain rect top) (- (chain window inner-height) 1))
              (>= (chain rect left) 0)
              ;; a partially visible element is still visible
              (<= (chain rect left) (- (chain window inner-width) 1))
              ;; some elements have top=bottom=left=right
              (> (chain rect width) 3)
              (> (chain rect height) 3)
              (not (= (chain computed-style "visibility") "hidden"))
              (not (= (chain computed-style "display") "none")))
         t nil)))

;; The following function is inspired by the algorithm from `saka-key` project
;;
;; https://github.com/lusakasa/saka-key/blob/8b2743c33e58056fe945df1797a3b6be83353e7e/src/modes/hints/client/findHints.js#L114
(export-always 'element-overlapped-p)
(defpsmacro element-overlapped-p (element)
  "Whether ELEMENT is overlapped by another element."
  `(let* ((rect (chain ,element (get-bounding-client-rect)))
          (computed-style (chain window (get-computed-style ,element)))
          (coord-truncation-offset 2)
          (radius (parse-float (chain computed-style border-top-left-radius)))
          (rounded-border-offset (ceiling (* radius (- 1 (sin (/ pi 4))))))
          (offset (max coord-truncation-offset rounded-border-offset))
          (el (chain document (element-from-point (+ (chain rect left) offset)
                                                  (+ (chain rect top) offset)))))
     (if (or (>= offset (chain rect width))
             (>= offset (chain rect height)))
         t
         (progn (loop while (and el (not (eq el element)))
                      do (setf el (chain el parent-node)))
                (null el)))))
