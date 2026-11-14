;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/reading-line
  (:documentation "Package for `reading-line-mode', for drawing a line to keep
track of the reading position."))
(in-package :nyxt/mode/reading-line)

(define-mode reading-line-mode ()
  "Mode for drawing a line on screen that you can use to keep track of
your reading position.

The line sits on a line of text. `reading-line-cursor-up' and
`reading-line-cursor-down' move it to the previous or next line, keeping the
horizontal position in the text. A click on text moves the line to that spot.

Commands:

- `reading-line-cursor-up' and `reading-line-cursor-down' to move the reading
  line cursor.

- `jump-to-reading-line-cursor': If you navigate away from the reading line, you
  can always invoke this command to center the view port on it."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "reading-line-mode" ()
      keyscheme:cua
      (list
       "M-up" 'reading-line-cursor-up
       "M-down" 'reading-line-cursor-down)
      keyscheme:emacs
      (list
       "M-p" 'reading-line-cursor-up
       "M-n" 'reading-line-cursor-down)
      keyscheme:vi-normal
      (list
       "K" 'reading-line-cursor-up
       "J" 'reading-line-cursor-down)))
   (style (theme:themed-css (theme *browser*)
            `("#reading-line-cursor"
              :position "absolute"
              :top "0"
              :left "0"
              :width "0"
              :height "0"
              :margin "0"
              :background-color ,theme:primary-color
              :z-index ,(1- (expt 2 31)) ; 32 bit signed integer max
              :opacity "15%"
              :pointer-events "none"
              :box-sizing "border-box"))
          :documentation "The CSS applied to the reading line.
Position and size are set from the text line under the cursor.")))

(-> reading-line-markup (reading-line-mode) string)
(defun reading-line-markup (mode)
  "HTML for the reading line and its stylesheet."
  (spinneret:with-html-string
    (:nstyle :id "reading-line-style" (style mode))
    (:span :id "reading-line-cursor")))

(-> reading-line-move (buffer integer) t)
(defun reading-line-move (buffer direction)
  "Move the reading line one text line. DIRECTION is 1 for down and -1 for up."
  (ps-eval :buffer buffer
    (let ((api (ps:@ window nyxt-reading-line)))
      (when api
        (ps:chain api (move (ps:lisp direction)))))))

(define-command jump-to-reading-line-cursor (&key (buffer (current-buffer)))
  "Center the view port on the reading line."
  (ps-eval :buffer buffer
    (let ((api (ps:@ window nyxt-reading-line)))
      (when api
        (ps:chain api (jump))))))

(define-command reading-line-cursor-up (&key (buffer (current-buffer)))
  "Move the reading line up to the previous line of text."
  (reading-line-move buffer -1))

(define-command reading-line-cursor-down (&key (buffer (current-buffer)))
  "Move the reading line down to the next line of text."
  (reading-line-move buffer 1))

(defun install-reading-line (buffer markup)
  "Insert the reading line into BUFFER and follow lines of text."
  (ps-eval :buffer buffer
    (when (ps:@ document body)
      (let ((previous (ps:@ window nyxt-reading-line)))
        (when (and previous (ps:@ previous click-listener))
          (ps:chain document (remove-event-listener "click"
                                                    (ps:@ previous click-listener)
                                                    t))))
      (let ((cursor (ps:chain document (query-selector "#reading-line-cursor")))
            (style (ps:chain document (query-selector "#reading-line-style"))))
        (when cursor
          (ps:chain cursor (remove)))
        (when style
          (ps:chain style (remove))))
      (ps:chain document body
                (|insertAdjacentHTML| "afterbegin" (ps:lisp markup)))
      (setf (ps:@ window nyxt-reading-line)
            (ps:create
             column-x nil
             caret-range
             (lambda (client-x client-y)
               (when (and (>= client-x 0)
                          (>= client-y 0)
                          (<= client-x (ps:@ window inner-width))
                          (<= client-y (ps:@ window inner-height))
                          (ps:@ document caret-range-from-point))
                 (ps:chain document (caret-range-from-point client-x client-y))))
             text-node
             (lambda (range)
               (when range
                 (let* ((node (ps:@ range start-container))
                        (offset (ps:@ range start-offset))
                        (children (ps:@ node child-nodes)))
                   (cond ((= (ps:@ node node-type) 3)
                          node)
                         ((and (= (ps:@ node node-type) 1)
                               (aref children offset)
                               (= (ps:@ (aref children offset) node-type) 3))
                          (aref children offset))
                         ((and (= (ps:@ node node-type) 1)
                               (> offset 0)
                               (aref children (- offset 1))
                               (= (ps:@ (aref children (- offset 1)) node-type) 3))
                          (aref children (- offset 1)))
                         (t nil)))))
             fixed-p
             (lambda (node)
               (let ((element (if (= (ps:@ node node-type) 3)
                                  (ps:@ node parent-element)
                                  node))
                     (fixed nil))
                 (loop named find-fixed
                       while (and element
                                  (not (eq element (ps:@ document document-element))))
                       do (when (string= (ps:@ (ps:chain window (get-computed-style element))
                                               position)
                                         "fixed")
                            (setf fixed t)
                            (return-from find-fixed))
                          (setf element (ps:@ element parent-element)))
                 fixed))
             block-of
             (lambda (node)
               (let ((element (if (= (ps:@ node node-type) 3)
                                  (ps:@ node parent-element)
                                  node)))
                 (loop named find-block
                       while (and element
                                  (not (eq element (ps:@ document body)))
                                  (not (eq element (ps:@ document document-element))))
                       do (let ((display (ps:@ (ps:chain window (get-computed-style element))
                                               display)))
                            (when (or (string= display "block")
                                      (string= display "list-item")
                                      (string= display "table-cell")
                                      (string= display "flex")
                                      (string= display "inline-flex")
                                      (string= display "grid")
                                      (string= display "inline-grid")
                                      (string= display "inline-block")
                                      (string= display "flow-root")
                                      (string= display "table"))
                              (return-from find-block)))
                          (setf element (ps:@ element parent-element)))
                 (or element (ps:@ document body))))
             block-distance
             (lambda (text-block client-x)
               (let ((rect (ps:chain text-block (get-bounding-client-rect))))
                 (cond ((and (>= client-x (- (ps:@ rect left) 8))
                             (<= client-x (+ (ps:@ rect right) 8)))
                        0)
                       ((< client-x (ps:@ rect left))
                        (- (ps:@ rect left) client-x))
                       (t
                        (- client-x (ps:@ rect right))))))
             accept-node
             (lambda (node client-x max-distance)
               (let ((api (ps:@ window nyxt-reading-line)))
                 (when (and node (not (ps:chain api (fixed-p node))))
                   (let ((text-block (ps:chain api (block-of node))))
                     (when (and text-block
                                (not (eq text-block (ps:@ document body)))
                                (not (eq text-block (ps:@ document document-element)))
                                (<= (ps:chain api (block-distance text-block client-x))
                                    max-distance))
                       text-block)))))
             block-at
             (lambda (client-x client-y max-distance)
               (let* ((api (ps:@ window nyxt-reading-line))
                      (chosen (ps:chain api (accept-node
                                             (ps:chain api (text-node
                                                            (ps:chain api (caret-range
                                                                           client-x client-y))))
                                             client-x max-distance))))
                 (unless chosen
                   (let ((element (ps:chain document (element-from-point client-x client-y))))
                     (when (and element
                                (not (string= (ps:@ element id) "reading-line-cursor")))
                       (let ((text-block (ps:chain api (accept-node element client-x
                                                                    max-distance))))
                         (when (and text-block
                                    (<= (ps:@ (ps:chain text-block (get-bounding-client-rect))
                                              height)
                                        700))
                           (setf chosen text-block))))))
                 (unless chosen
                   (dolist (dx '(-48 -120 -220 -360 48 120 220))
                     (let ((text-block (ps:chain api (accept-node
                                                      (ps:chain api
                                                                (text-node
                                                                 (ps:chain api
                                                                           (caret-range
                                                                            (+ client-x dx)
                                                                            client-y))))
                                                      client-x max-distance))))
                       (when text-block
                         (setf chosen text-block)
                         (return)))))
                 chosen))
             same-line-p
             (lambda (line frag)
               (let* ((line-top (ps:@ line anchor-top))
                      (line-bottom (ps:@ line anchor-bottom))
                      (line-height (- line-bottom line-top))
                      (overlap (- (min line-bottom (ps:@ frag bottom))
                                  (max line-top (ps:@ frag top)))))
                 (and (> line-height 0)
                      (> overlap (* (min line-height (ps:@ frag height)) 0.5)))))
             line-boxes
             (lambda (text-block)
               (let* ((api (ps:@ window nyxt-reading-line))
                      (fragments (array))
                      (walker (ps:chain document (create-tree-walker text-block 4 null)))
                      (text-node (ps:chain walker (next-node))))
                 (loop named read-text
                       while text-node
                       do (let ((parent (ps:@ text-node parent-element))
                                (content (and (ps:@ text-node text-content)
                                              (ps:chain (ps:@ text-node text-content) (trim)))))
                            (when (and content
                                       parent
                                       (not (or (string= (ps:@ parent tag-name) "SCRIPT")
                                                (string= (ps:@ parent tag-name) "STYLE")
                                                (string= (ps:@ parent tag-name) "NOSCRIPT")
                                                (string= (ps:@ parent tag-name) "TEXTAREA")
                                                (string= (ps:@ parent tag-name) "INPUT"))))
                              (let ((range (ps:chain document (create-range))))
                                (ps:chain range (select-node-contents text-node))
                                (let ((rects (ps:chain range (get-client-rects))))
                                  (loop named read-rects
                                        for index from 0 below (ps:@ rects length)
                                        do (let ((rect (aref rects index)))
                                             (when (and (>= (ps:@ rect width) 2)
                                                        (>= (ps:@ rect height) 8))
                                               (ps:chain fragments
                                                         (push (ps:create
                                                                top (ps:@ rect top)
                                                                bottom (ps:@ rect bottom)
                                                                left (ps:@ rect left)
                                                                right (ps:@ rect right)
                                                                height (ps:@ rect height)
                                                                width (ps:@ rect width)
                                                                node text-node))))))))))
                          (setf text-node (ps:chain walker (next-node))))
                 (let ((lines (array)))
                   (setf fragments
                         (ps:chain fragments
                                   (sort (lambda (left right)
                                           (- (ps:@ left top) (ps:@ right top))))))
                   (loop named merge-frags
                         for index from 0 below (ps:@ fragments length)
                         do (let* ((frag (aref fragments index))
                                   (last (when (> (ps:@ lines length) 0)
                                           (aref lines (- (ps:@ lines length) 1)))))
                              (if (and last (ps:chain api (same-line-p last frag)))
                                  (progn
                                    (setf (ps:@ last top) (min (ps:@ last top) (ps:@ frag top))
                                          (ps:@ last bottom) (max (ps:@ last bottom)
                                                                  (ps:@ frag bottom))
                                          (ps:@ last left) (min (ps:@ last left) (ps:@ frag left))
                                          (ps:@ last right) (max (ps:@ last right)
                                                                 (ps:@ frag right)))
                                    (setf (ps:@ last height) (- (ps:@ last bottom) (ps:@ last top))
                                          (ps:@ last width) (- (ps:@ last right) (ps:@ last left))))
                                  (ps:chain lines
                                            (push (ps:create top (ps:@ frag top)
                                                             bottom (ps:@ frag bottom)
                                                             left (ps:@ frag left)
                                                             right (ps:@ frag right)
                                                             height (ps:@ frag height)
                                                             width (ps:@ frag width)
                                                             anchor-top (ps:@ frag top)
                                                             anchor-bottom (ps:@ frag bottom)
                                                             node (ps:@ frag node)))))))
                   lines)))
             pick-line
             (lambda (lines doc-mid direction min-shift)
               (let ((best nil)
                     (best-mid nil))
                 (loop named pick
                       for index from 0 below (ps:@ lines length)
                       do (let ((mid (+ (/ (+ (ps:@ (aref lines index) top)
                                              (ps:@ (aref lines index) bottom))
                                           2)
                                        (ps:@ window scroll-y))))
                            (when (if (> direction 0)
                                      (> mid (+ doc-mid min-shift))
                                      (< mid (- doc-mid min-shift)))
                              (when (or (null best)
                                        (if (> direction 0)
                                            (< mid best-mid)
                                            (> mid best-mid)))
                                (setf best (aref lines index)
                                      best-mid mid)))))
                 best))
             line-height-px
             (lambda (style fallback)
               (let* ((raw (ps:@ style line-height))
                      (parsed (parse-float raw))
                      (font (or (parse-float (ps:@ style font-size)) fallback)))
                 (cond ((or (not parsed) (not raw)) (* font 1.2))
                       ((>= (ps:chain raw (index-of "px")) 0) parsed)
                       ((>= (ps:chain raw (index-of "%")) 0) (* font (/ parsed 100)))
                       (t (* font parsed)))))
             geometry
             (lambda (text-block line)
               (let* ((style (ps:chain window (get-computed-style text-block)))
                      (rect (ps:chain text-block (get-bounding-client-rect)))
                      (pad-left (or (parse-float (ps:@ style padding-left)) 0))
                      (pad-right (or (parse-float (ps:@ style padding-right)) 0))
                      (border-left (or (parse-float (ps:@ style border-left-width)) 0))
                      (border-right (or (parse-float (ps:@ style border-right-width)) 0))
                      (left (+ (ps:@ rect left) border-left pad-left))
                      (right (- (ps:@ rect right) border-right pad-right))
                      (glyph-height (max 1 (- (ps:@ line bottom) (ps:@ line top))))
                      (glyph-mid (/ (+ (ps:@ line top) (ps:@ line bottom)) 2))
                      (line-height (ps:chain (ps:@ window nyxt-reading-line)
                                            (line-height-px style glyph-height)))
                      (height (max glyph-height line-height)))
                 (ps:create top (- glyph-mid (/ height 2))
                            left left
                            width (max 8 (- right left))
                            height height)))
             place
             (lambda (line)
               (let ((cursor (ps:chain document (query-selector "#reading-line-cursor"))))
                 (when (and cursor line)
                   (setf (ps:@ cursor style position) "absolute"
                         (ps:@ cursor style margin) "0"
                         (ps:@ cursor style pointer-events) "none"
                         (ps:@ cursor style top) "0px"
                         (ps:@ cursor style left) "0px")
                   (let ((origin (ps:chain cursor (get-bounding-client-rect))))
                     (setf (ps:@ cursor style top)
                           (+ (- (ps:@ line top) (ps:@ origin top)) "px")
                           (ps:@ cursor style left)
                           (+ (- (ps:@ line left) (ps:@ origin left)) "px")
                           (ps:@ cursor style width)
                           (+ (ps:@ line width) "px")
                           (ps:@ cursor style height)
                           (+ (ps:@ line height) "px"))))))
             reveal
             (lambda (doc-y)
               (let ((client-y (- doc-y (ps:@ window scroll-y)))
                     (margin 40))
                 (when (or (< client-y margin)
                           (> client-y (- (ps:@ window inner-height) margin)))
                   (ps:chain window (scroll-to
                                     (ps:@ window scroll-x)
                                     (max 0 (- doc-y (/ (ps:@ window inner-height) 2))))))))
             reveal-column
             (lambda ()
               (let* ((api (ps:@ window nyxt-reading-line))
                      (client-x (- (ps:@ api column-x) (ps:@ window scroll-x))))
                 (when (or (< client-x 16)
                           (> client-x (- (ps:@ window inner-width) 16)))
                   (ps:chain window (scroll-to
                                     (max 0 (- (ps:@ api column-x)
                                               (/ (ps:@ window inner-width) 2)))
                                     (ps:@ window scroll-y))))))
             place-initial
             (lambda ()
               (let* ((api (ps:@ window nyxt-reading-line))
                      (root (or (ps:chain document (query-selector
                                                    "article, main, [role=main]"))
                                (ps:@ document body))))
                 (when root
                   (let* ((root-rect (ps:chain root (get-bounding-client-rect)))
                          (root-style (ps:chain window (get-computed-style root)))
                          (pad-left (or (parse-float (ps:@ root-style padding-left)) 0))
                          (client-x (+ (ps:@ root-rect left)
                                       (max (+ pad-left 16)
                                            (min (* (ps:@ root-rect width) 0.35) 280))))
                          (y0 (max (ps:@ root-rect top) 8))
                          (y1 (min (max (ps:@ root-rect bottom) (+ y0 1))
                                   (- (ps:@ window inner-height) 8)))
                          (found nil))
                     (when (or (< client-x 8)
                               (> client-x (- (ps:@ window inner-width) 8)))
                       (setf client-x (/ (ps:@ window inner-width) 2)))
                     (when (<= y1 y0)
                       (setf y0 8
                             y1 (- (ps:@ window inner-height) 8)))
                     (let ((client-y y0))
                       (loop named scan-y
                             while (and (not found) (< client-y y1))
                             do (let ((text-block (ps:chain api (block-at client-x client-y 280))))
                                  (when text-block
                                    (let ((lines (ps:chain api (line-boxes text-block)))
                                          (line nil))
                                      (loop named hit-line
                                            for index from 0 below (ps:@ lines length)
                                            do (let ((candidate (aref lines index)))
                                                 (when (and (>= client-y (- (ps:@ candidate top) 2))
                                                            (<= client-y
                                                                (+ (ps:@ candidate bottom) 2)))
                                                   (setf line candidate)
                                                   (return-from hit-line))))
                                      (unless line
                                        (loop named below-line
                                              for index from 0 below (ps:@ lines length)
                                              do (let ((candidate (aref lines index)))
                                                   (when (>= (ps:@ candidate bottom) client-y)
                                                     (setf line candidate)
                                                     (return-from below-line)))))
                                      (unless line
                                        (when (> (ps:@ lines length) 0)
                                          (setf line (aref lines 0))))
                                      (when line
                                        (setf (ps:@ api column-x)
                                              (+ client-x (ps:@ window scroll-x))
                                              found t)
                                        (ps:chain api (place
                                                       (ps:chain api (geometry text-block
                                                                               line)))))))
                                (incf client-y 8))))
                     found))))
             move-to-next-block
             (lambda (direction doc-mid min-shift skip-block)
               (let* ((api (ps:@ window nyxt-reading-line))
                      (cursor (ps:chain document (query-selector "#reading-line-cursor")))
                      (rect (ps:chain cursor (get-bounding-client-rect)))
                      (probe-y (if (> direction 0)
                                  (+ (ps:@ rect bottom) (ps:@ window scroll-y) 1)
                                  (- (+ (ps:@ rect top) (ps:@ window scroll-y)) 1)))
                      (max-doc (max (ps:@ document document-element scroll-height)
                                    (if (ps:@ document body)
                                        (ps:@ document body scroll-height)
                                        0)))
                      (limit (max (* (ps:@ window inner-height) 3) 900))
                      (traveled 0)
                      (placed nil))
                 (loop named next-block
                       while (< traveled limit)
                       do (let ((doc-y (+ probe-y (* direction traveled))))
                            (when (or (< doc-y 0) (> doc-y max-doc))
                              (return-from next-block))
                            (ps:chain api (reveal doc-y))
                            (let ((text-block (ps:chain api (block-at
                                                             (- (ps:@ api column-x)
                                                                (ps:@ window scroll-x))
                                                             (- doc-y (ps:@ window scroll-y))
                                                             48))))
                              (when (and text-block (not (eq text-block skip-block)))
                                (let ((line (ps:chain api (pick-line
                                                           (ps:chain api (line-boxes text-block))
                                                           doc-mid direction min-shift))))
                                  (when line
                                    (ps:chain api (place
                                                   (ps:chain api (geometry text-block line))))
                                    (setf placed t)
                                    (return-from next-block))))))
                          (incf traveled 10))
                 placed))
             move
             (lambda (direction)
               (let ((api (ps:@ window nyxt-reading-line))
                     (cursor (ps:chain document (query-selector "#reading-line-cursor"))))
                 (cond ((or (not cursor) (not api))
                        nil)
                       ((or (null (ps:@ api column-x))
                            (< (ps:@ (ps:chain cursor (get-bounding-client-rect)) height) 1))
                        (ps:chain api (place-initial)))
                       (t
                        (ps:chain api (reveal-column))
                        (let* ((rect (ps:chain cursor (get-bounding-client-rect)))
                               (doc-mid (+ (/ (+ (ps:@ rect top) (ps:@ rect bottom)) 2)
                                           (ps:@ window scroll-y)))
                               (min-shift (max 4 (* (ps:@ rect height) 0.35)))
                               (text-block (ps:chain api (block-at
                                                          (- (ps:@ api column-x)
                                                             (ps:@ window scroll-x))
                                                          (/ (+ (ps:@ rect top)
                                                                (ps:@ rect bottom))
                                                             2)
                                                          48)))
                               (placed nil))
                          (when text-block
                            (let ((line (ps:chain api (pick-line
                                                       (ps:chain api (line-boxes text-block))
                                                       doc-mid direction min-shift))))
                              (when line
                                (ps:chain api (place (ps:chain api (geometry text-block line))))
                                (setf placed t))))
                          (unless placed
                            (setf placed (ps:chain api (move-to-next-block
                                                        direction doc-mid min-shift text-block))))
                          (when placed
                            (ps:chain cursor (scroll-into-view
                                              (ps:create block "nearest" inline "nearest"))))
                          placed)))))
             place-at
             (lambda (client-x client-y)
               (let* ((api (ps:@ window nyxt-reading-line))
                      (text-block (ps:chain api (block-at client-x client-y 48)))
                      (lines (when text-block
                               (ps:chain api (line-boxes text-block))))
                      (chosen nil))
                 (when lines
                   (loop named click-line
                         for index from 0 below (ps:@ lines length)
                         do (let ((candidate (aref lines index)))
                              (when (and (>= client-y (- (ps:@ candidate top) 4))
                                         (<= client-y (+ (ps:@ candidate bottom) 4)))
                                (setf chosen candidate)
                                (return-from click-line)))))
                 (when chosen
                   (setf (ps:@ api column-x) (+ client-x (ps:@ window scroll-x)))
                   (ps:chain api (place (ps:chain api (geometry text-block chosen))))
                   t)))
             click-listener
             (lambda (event)
               (when (= (ps:@ event button) 0)
                 (let ((api (ps:@ window nyxt-reading-line)))
                   (when api
                     (ps:chain api (place-at (ps:@ event client-x)
                                             (ps:@ event client-y)))))))
             jump
             (lambda ()
               (let ((api (ps:@ window nyxt-reading-line))
                     (cursor (ps:chain document (query-selector "#reading-line-cursor"))))
                 (when (and cursor api)
                   (when (or (null (ps:@ api column-x))
                             (< (ps:@ (ps:chain cursor (get-bounding-client-rect)) height) 1))
                     (ps:chain api (place-initial)))
                   (ps:chain cursor (scroll-into-view
                                     (ps:create block "center" inline "nearest"))))))))
      (let ((api (ps:@ window nyxt-reading-line)))
        (when api
          (ps:chain api (place-initial))
          (ps:chain document (add-event-listener "click"
                                                 (ps:@ api click-listener)
                                                 t)))))))

(defmethod on-signal-load-finished ((mode reading-line-mode) url title)
  (declare (ignore url title))
  (enable mode))

(defmethod enable ((mode reading-line-mode) &key)
  (install-reading-line (buffer mode) (reading-line-markup mode)))

(defmethod disable ((mode reading-line-mode) &key)
  (ps-eval :buffer (buffer mode)
    (let ((api (ps:@ window nyxt-reading-line))
          (cursor (ps:chain document (query-selector "#reading-line-cursor")))
          (style (ps:chain document (query-selector "#reading-line-style"))))
      (when (and api (ps:@ api click-listener))
        (ps:chain document (remove-event-listener "click"
                                                  (ps:@ api click-listener)
                                                  t)))
      (when cursor
        (ps:chain cursor (remove)))
      (when style
        (ps:chain style (remove)))
      (setf (ps:@ window nyxt-reading-line) nil))))
