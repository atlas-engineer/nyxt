;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/terminal
  (:documentation "Package for `terminal-mode'.
The mode runs a shell on a pseudoterminal and emulates it with libghostty-vt.
A WebGL page draws the viewport. The buffer title is the title the program
sets, or the foreground program's name. Keys go to the terminal. C-M-z runs
`toggle-terminal-keys'."))
(in-package :nyxt/mode/terminal)

(defvar *browser-keys*
  (sera:dict "Enter" :enter
             "Backspace" :backspace
             "Tab" :tab
             "Escape" :escape
             " " :space
             "Delete" :delete
             "Insert" :insert
             "Home" :home
             "End" :end
             "PageUp" :page-up
             "PageDown" :page-down
             "ArrowUp" :arrow-up
             "ArrowDown" :arrow-down
             "ArrowLeft" :arrow-left
             "ArrowRight" :arrow-right
             "F1" :f1
             "F2" :f2
             "F3" :f3
             "F4" :f4
             "F5" :f5
             "F6" :f6
             "F7" :f7
             "F8" :f8
             "F9" :f9
             "F10" :f10
             "F11" :f11
             "F12" :f12
             "`" :backquote
             "\\" :backslash
             "[" :bracket-left
             "]" :bracket-right
             "," :comma
             "=" :equal
             "-" :minus
             "." :period
             "'" :quote
             ";" :semicolon
             "/" :slash)
  "Browser KeyboardEvent.key strings mapped to libghostty key names.")

(defparameter *vertex-shader*
  "#version 300 es
layout(location = 0) in vec2 corner;
layout(location = 1) in float glyph;
layout(location = 2) in vec3 fg;
layout(location = 3) in vec3 bg;
layout(location = 4) in float style;
uniform vec2 grid;
uniform vec2 atlasGrid;
uniform vec2 atlasInset;
out vec2 vUv;
out vec2 vCorner;
out vec3 vFg;
out vec3 vBg;
out float vStyle;
void main() {
  float id = float(gl_InstanceID);
  float col = mod(id, grid.x);
  float row = floor(id / grid.x);
  vec2 pos = (vec2(col, row) + corner) / grid;
  pos.y = 1.0 - pos.y;
  gl_Position = vec4(pos * 2.0 - 1.0, 0.0, 1.0);
  float atlasCols = atlasGrid.x;
  vec2 origin = vec2(mod(glyph, atlasCols), floor(glyph / atlasCols));
  vec2 local = mix(atlasInset, vec2(1.0) - atlasInset, corner);
  vUv = (origin + local) / atlasGrid;
  vCorner = corner;
  vFg = fg;
  vBg = bg;
  vStyle = style;
}
")

(defparameter *fragment-shader*
  "#version 300 es
precision highp float;
uniform sampler2D atlas;
uniform vec2 atlasGrid;
uniform vec2 atlasInset;
in vec2 vUv;
in vec2 vCorner;
in vec3 vFg;
in vec3 vBg;
in float vStyle;
out vec4 outColor;
void main() {
  vec3 coverage = clamp(vec3(1.0) - texture(atlas, vUv).rgb, 0.0, 1.0);
  if (mod(floor(vStyle / 2.0), 2.0) >= 1.0) {
    vec2 step = vec2(atlasInset.x / atlasGrid.x, 0.0);
    coverage = max(coverage, clamp(vec3(1.0) - texture(atlas, vUv + step).rgb, 0.0, 1.0));
  }
  if (mod(vStyle, 2.0) >= 1.0 && vCorner.y > 0.86) coverage = vec3(1.0);
  outColor = vec4(mix(vBg, vFg, coverage), 1.0);
}
")

(defun ensure-library ()
  "Load libghostty-vt.
`LIBGHOSTTY_VT' names the shared library when the loader cannot find it."
  (cl-libghostty:load-library))

(defun scroll-library-path ()
  "Return the cached helper that scrolls a libghostty viewport."
  (uiop:merge-pathnames* "nyxt/libnyxt-terminal-scroll.so"
                         (uiop:ensure-directory-pathname (uiop:xdg-cache-home))))

(defun ensure-scroll-library ()
  "Compile and load the viewport-scroll helper. Return T when it is loaded."
  (let* ((source (uiop:merge-pathnames* "libraries/terminal/scroll.c"
                                       (asdf:system-source-directory :nyxt)))
         (ghostty (or (uiop:getenv "LIBGHOSTTY_VT") ""))
         (directory (if (and (plusp (length ghostty)) (find #\/ ghostty))
                        (namestring (uiop:pathname-directory-pathname ghostty))
                        ""))
         (output (scroll-library-path))
         (fresh (and (probe-file output)
                     (>= (file-write-date output) (file-write-date source)))))
    (unless fresh
      (ensure-directories-exist output)
      (uiop:run-program
       (append (list "cc" "-shared" "-fPIC" "-O2"
                     "-o" (uiop:native-namestring output)
                     (uiop:native-namestring source))
               (when (plusp (length directory))
                 (list (format nil "-L~a" directory)
                       (format nil "-Wl,-rpath,~a" directory)))
               (list "-lghostty-vt"))
       :error-output :string))
    (cffi:load-foreign-library (uiop:native-namestring output))
    t))

(defun scroll-viewport (screen delta)
  "Scroll SCREEN by DELTA rows. Negative moves into the scrollback."
  (cffi:foreign-funcall "nyxt_terminal_scroll"
                        :pointer screen :long delta :int))

(defun scroll-to-bottom (screen)
  "Show the active area of SCREEN."
  (cffi:foreign-funcall "nyxt_terminal_scroll_bottom" :pointer screen :int))

(defun css-color-rgb (color)
  "Return the red, green, and blue of CSS COLOR, or NIL when it is not understood."
  (let ((text (string-downcase (string-trim '(#\space #\tab) (or color "")))))
    (cond
      ((string= text "black") '(0 0 0))
      ((string= text "white") '(255 255 255))
      ((and (= (length text) 4) (char= (char text 0) #\#))
       (let ((value (parse-integer text :start 1 :radix 16 :junk-allowed t)))
         (when value
           (list (* (ldb (byte 4 8) value) 17)
                 (* (ldb (byte 4 4) value) 17)
                 (* (ldb (byte 4 0) value) 17)))))
      ((and (= (length text) 7) (char= (char text 0) #\#))
       (let ((value (parse-integer text :start 1 :radix 16 :junk-allowed t)))
         (when value
           (list (ldb (byte 8 16) value)
                 (ldb (byte 8 8) value)
                 (ldb (byte 8 0) value))))))))

(defun set-screen-color (screen option rgb)
  "Set SCREEN's libghostty color OPTION to RGB."
  (cffi:with-foreign-object (color :uint8 3)
    (setf (cffi:mem-aref color :uint8 0) (first rgb)
          (cffi:mem-aref color :uint8 1) (second rgb)
          (cffi:mem-aref color :uint8 2) (third rgb))
    (cffi:foreign-funcall "ghostty_terminal_set"
                          :pointer screen :int option :pointer color :int)))

(defun apply-theme-colors (screen)
  "Use the browser theme for SCREEN's default foreground and background.
The previous terminal took these from the theme, so a light theme stays light."
  (let ((theme (theme *browser*)))
    (when theme
      (when-let ((foreground (css-color-rgb (theme:on-background-color theme))))
        (set-screen-color screen 12 foreground))
      (when-let ((background (css-color-rgb (theme:background-color theme))))
        (set-screen-color screen 13 background)))))

(defun configure-scrollback (screen)
  "Keep about 5000 lines, matching the previous terminal's scrollback."
  (cffi:foreign-funcall "nyxt_terminal_scrollback"
                        :pointer screen
                        :unsigned-long 5000
                        :unsigned-long (* 32 1024 1024)
                        :int))

(defun terminal-title (text)
  "Return TEXT as a buffer title.
A blank TEXT names the terminal itself."
  (let ((title (string-trim '(#\space #\tab #\newline #\return)
                            (or text ""))))
    (if (plusp (length title))
        title
        "*Terminal*")))

(defun shell-command-name (mode)
  "Return the file name of the shell MODE starts."
  (file-namestring (first (program-argv mode))))

(defun note-terminal-title (mode text)
  "Set MODE's buffer title from the title TEXT the program chose."
  (when (buffer mode)
    (setf (title-from-program-p mode) t
          (title (buffer mode)) (terminal-title text))))

(defun note-foreground-program (mode)
  "Name MODE's buffer after a foreground program other than its shell."
  (handler-case
      (when-let ((pty (pty mode)))
        (let ((name (terminal:foreground-name pty)))
          (when (and name (not (equal name (foreground-name mode))))
            (setf (foreground-name mode) name
                  (title-from-program-p mode) nil))
          (when (and name
                     (not (title-from-program-p mode))
                     (not (string= name (shell-command-name mode)))
                     (buffer mode)
                     (not (string= name (title (buffer mode)))))
            (setf (title (buffer mode)) name))))
    (error (condition)
      (log:debug "Terminal title was not updated: ~a" condition))))

(defun note-screen-title (mode)
  "Apply a new program title, or the foreground program name."
  (note-foreground-program mode)
  (let ((text (and (screen mode)
                   (cl-libghostty:terminal-title (screen mode)))))
    (when (and (stringp text)
               (plusp (length text))
               (not (equal text (last-program-title mode))))
      (setf (last-program-title mode) text)
      (note-terminal-title mode text))))

(defun browser-key (name)
  "Return the libghostty key for the browser key NAME."
  (or (gethash name *browser-keys*)
      (when (and (= (length name) 1) (alpha-char-p (char name 0)))
        (intern (string (char-upcase (char name 0))) :keyword))
      (when (and (= (length name) 1) (digit-char-p (char name 0)))
        (intern (format nil "DIGIT-~C" (char name 0)) :keyword))
      :unidentified))

(defun put-u16 (octets index value)
  (setf (aref octets index) (logand value #xff)
        (aref octets (1+ index)) (logand (ash value -8) #xff))
  (+ index 2))

(defun put-u32 (octets index value)
  (setf (aref octets index) (logand value #xff)
        (aref octets (+ index 1)) (logand (ash value -8) #xff)
        (aref octets (+ index 2)) (logand (ash value -16) #xff)
        (aref octets (+ index 3)) (logand (ash value -24) #xff))
  (+ index 4))

(defun cell-codepoint (text)
  "Return the first codepoint of TEXT, or a space when TEXT is empty."
  (if (and (stringp text) (plusp (length text)))
      (char-code (char text 0))
      32))

(defun write-rgb (octets index color)
  (setf (aref octets index) (or (first color) 0)
        (aref octets (+ index 1)) (or (second color) 0)
        (aref octets (+ index 2)) (or (third color) 0))
  (+ index 4))

(defun cell-selected-p (mode x y)
  "Return whether cell X, Y is inside MODE's mouse selection."
  (let ((start (selection-start mode))
        (end (selection-end mode)))
    (when (and start end)
      (let* ((start-index (+ (* (cdr start) 100000) (car start)))
             (end-index (+ (* (cdr end) 100000) (car end)))
             (index (+ (* y 100000) x)))
        (<= (min start-index end-index) index (max start-index end-index))))))

(defun theme-color-rgb (reader fallback)
  "Return the red, green, and blue of the theme color READER, or FALLBACK."
  (let ((theme (and *browser* (theme *browser*))))
    (or (and theme (css-color-rgb (funcall reader theme)))
        fallback)))

(defun cursor-color ()
  "Return the block cursor color."
  (theme-color-rgb #'theme:action-color '(55 168 228)))

(defun cell-flags (cells)
  "Return the style flags of CELLS, or 0 when the cell is unstyled."
  (if (cl-libghostty:cell-styled-p cells)
      (cl-libghostty:cell-style-flags cells)
      0))

(defun cell-wide (cells)
  "Return the width class of the current cell.
0 is narrow, 1 is wide, 2 is the spacer after a wide character, and 3 is the
spacer at the start of a wrapped line. A spacer is not drawn."
  (cffi:with-foreign-objects ((raw :uint64) (wide :int))
    (if (and (zerop (cffi:foreign-funcall "ghostty_render_state_row_cells_get"
                                          :pointer cells :int 1 :pointer raw :int))
             (zerop (cffi:foreign-funcall "ghostty_cell_get"
                                          :uint64 (cffi:mem-ref raw :uint64)
                                          :int 3 :pointer wide :int)))
        (cffi:mem-ref wide :int)
        0)))

(defun write-cell (octets offset cells foreground background cursor-color
                   cursor-p selected-p wide previous)
  "Pack the current cell of CELLS at OFFSET.
Inverse swaps the resolved colors, which is how `top' draws its header bar.
A cursor cell uses CURSOR-COLOR behind the glyph and BACKGROUND for the glyph,
which is a solid block on an empty cell. Invisible text is packed as a space.
WIDE is the result of `cell-wide'. A spacer after a wide character repeats
PREVIOUS so the page can draw the right half of that glyph."
  (let ((fore (or (cl-libghostty:cell-foreground cells) foreground))
        (back (or (cl-libghostty:cell-background cells) background))
        (flags (cell-flags cells))
        (text (cl-libghostty:cell-text cells)))
    (cond ((= wide 2)
           (setf text (or previous "")
                 flags (logior flags 8)))
          ((= wide 3)
           (setf text "")))
    (when (plusp (logand flags cl-libghostty:+style-inverse+))
      (rotatef fore back))
    (when selected-p
      (setf back '(51 153 255)))
    (when cursor-p
      (setf fore background
            back cursor-color))
    (when (plusp (logand flags cl-libghostty:+style-invisible+))
      (setf text ""))
    (put-u32 octets offset (cell-codepoint text))
    (write-rgb octets (+ offset 4) fore)
    (setf (aref octets (+ offset 7))
          (logior (if (plusp (logand flags cl-libghostty:+style-underline-mask+))
                      1
                      0)
                  (if (plusp (logand flags cl-libghostty:+style-bold+))
                      2
                      0)))
    (write-rgb octets (+ offset 8) back)))

(defun pack-viewport (mode)
  "Pack MODE's viewport into its frame vector.
Return the number of bytes used. The vector starts with the grid size and the
cursor, then one 12-byte cell: codepoint, foreground, background."
  (let* ((state (render-state mode))
         (columns (or (cl-libghostty:render-columns state) (columns mode)))
         (rows (or (cl-libghostty:render-rows state) (rows mode)))
         (needed (+ 16 (* columns rows 12)))
         (octets (frame-octets mode)))
    (unless (and octets (>= (length octets) needed))
      (setf octets (make-array needed :element-type '(unsigned-byte 8)
                               :initial-element 0)
            (frame-octets mode) octets))
    (fill octets 0 :end needed)
    (multiple-value-bind (cursor-x cursor-y cursor-visible)
        (cl-libghostty:render-cursor state)
      (put-u16 octets 0 columns)
      (put-u16 octets 2 rows)
      (put-u16 octets 4 (or cursor-x #xffff))
      (put-u16 octets 6 (or cursor-y #xffff))
      (setf (aref octets 8) (if cursor-visible 1 0))
      (let ((foreground (or (cl-libghostty:render-foreground state)
                            '(255 255 255)))
            (background (or (cl-libghostty:render-background state)
                            '(0 0 0)))
            (cursor (cursor-color)))
        (cl-libghostty:row-iterator-load state (row-iterator mode))
        (loop while (cl-libghostty:row-next (row-iterator mode))
              for y = (cl-libghostty:row-viewport-y (row-iterator mode))
              when (and y (<= 0 y (1- rows)))
                do (cl-libghostty:row-cells-load (row-iterator mode) (row-cells mode))
                   (loop with previous = ""
                         for x below columns
                         for offset = (+ 16 (* (+ (* y columns) x) 12))
                         for wide = (progn
                                      (cl-libghostty:cell-select (row-cells mode) x)
                                      (cell-wide (row-cells mode)))
                         do (write-cell octets offset (row-cells mode)
                                        foreground background cursor
                                        (and cursor-visible
                                             cursor-x cursor-y
                                             (= x cursor-x)
                                             (= y cursor-y))
                                        (cell-selected-p mode x y)
                                        wide previous)
                            (setf previous
                                  (if (= wide 1)
                                      (or (cl-libghostty:cell-text (row-cells mode)) "")
                                      ""))))))
    (setf (frame-length mode) needed)
    needed))

(defun frame-script (b64)
  "Return the script that delivers the viewport B64 to the page."
  (ps:ps
    (when (ps:@ window nyxt-vt)
      (ps:chain window nyxt-vt (frame (ps:lisp b64))))))

(defun send-viewport (mode)
  "Send MODE's packed viewport to its page."
  (let ((buffer (buffer mode))
        (octets (frame-octets mode))
        (length (frame-length mode)))
    (when (and buffer (find buffer (buffer-list) :test #'eq) (plusp length))
      (ffi-buffer-evaluate-javascript-async
       buffer
       (frame-script
        (cl-base64:usb8-array-to-base64-string
         (make-array length :element-type '(unsigned-byte 8) :displaced-to octets)
         :columns 0)))
      (incf (frames mode)))))

(defun frame-loop (mode)
  "Publish a viewport about sixty times a second."
  (loop
    (sleep 0.016)
    (handler-case
        (unless (running-p mode)
          (return))
      (error ()
        (return)))
    (handler-case (publish-frame mode)
      (error (condition)
        (log:debug "Terminal frame failed: ~a" condition)))))

(defun feed-octets (mode octets)
  "Write OCTETS into MODE's emulator and count them."
  (when (plusp (length octets))
    (let ((start (get-internal-real-time)))
      (bt:with-lock-held ((lock mode))
        (when (screen mode)
          (cl-libghostty:terminal-write (screen mode) octets)
          (incf (bytes-in mode) (length octets))
          (incf (write-ticks mode) (- (get-internal-real-time) start))
          (when (selection-start mode)
            (setf (selection-start mode) nil
                  (selection-end mode) nil
                  (selection-dirty-p mode) t)))))))

(defun accept-output (mode octets)
  "Emulate OCTETS from the pseudoterminal."
  (feed-octets mode octets))

(defun vt-markup ()
  "Return the terminal page body."
  (spinneret:with-html-string
    (:nstyle
      `("html, body"
        :height "100%"
        :margin 0
        :padding 0
        :overflow "hidden"
        :background ,theme:background-color
        :color ,theme:on-background-color)
      '("#terminal"
        :height "100%"
        :width "100%"
        :display "flex"
        :align-items "center"
        :justify-content "center")
'("canvas"
  :display "block"
  :flex "none"
  :outline "none"))
    (:div :id "terminal"
          (:canvas :id "screen" :tabindex "0" :width "1" :height "1"))))

(defparameter *default-font-size* 13
  "Terminal face size in CSS pixels.")

(defun vt-script (buffer)
  "Return the script that draws BUFFER's viewport with WebGL."
  (let* ((theme (theme *browser*))
         (mode (find-mode buffer 'terminal-mode))
         (font-size (if mode (font-size mode) *default-font-size*)))
    (ps:ps
      ((lambda ()
         (let* ((buffer-id (ps:lisp (id buffer)))
                (font-family (ps:lisp (or (theme:monospace-font-family theme)
                                          "DejaVu Sans Mono")))
                (face "")
                (node (ps:chain document (get-element-by-id "terminal")))
                (canvas (ps:chain document (get-element-by-id "screen")))
                (gl nil)
                (atlas nil)
                (instance-buffer nil)
                (instance-floats nil)
                (grid-location nil)
                (cols 0)
                (rows 0)
                (sent-cols 0)
                (sent-rows 0)
                (frames 0)
                (screen-text "")
                (css-cell-w 8)
                (css-cell-h 18)
                (device-cell-w 8)
                (device-cell-h 18)
                (atlas-font "")
                (atlas-pad 1)
                (glyph-baseline 12)
                (quad-buffer nil)
                (atlas-sampler-location nil)
                (bg-r (ps:lisp (let ((rgb (or (and theme
                                                   (css-color-rgb
                                                    (theme:background-color theme)))
                                              '(248 248 248))))
                                 (/ (first rgb) 255.0))))
                (bg-g (ps:lisp (let ((rgb (or (and theme
                                                   (css-color-rgb
                                                    (theme:background-color theme)))
                                              '(248 248 248))))
                                 (/ (second rgb) 255.0))))
                (bg-b (ps:lisp (let ((rgb (or (and theme
                                                   (css-color-rgb
                                                    (theme:background-color theme)))
                                              '(248 248 248))))
                                 (/ (third rgb) 255.0))))
                (atlas-grid-location nil)
                (atlas-inset-location nil)
                (gl-program nil)
                (vertex (ps:lisp *vertex-shader*))
                (fragment (ps:lisp *fragment-shader*)))
           (defun send (action query)
             (ps:chain window
                       (fetch (ps:+ "nyxt-terminal://session/" action "?" query)
                              (ps:create mode "no-cors"))))
           (defun font-of (px)
             (let ((family (or font-family "DejaVu Sans Mono")))
               (ps:+ px "px "
                     (if (>= (ps:chain family (index-of ",")) 0)
                         family
                         (ps:+ "\"" (ps:chain family (split "\"") (join "")) "\", monospace")))))
           (defun font-px ()
             (let ((size (or (and (ps:@ window nyxt-vt)
                                  (ps:@ window nyxt-vt font-size))
                             (ps:lisp font-size))))
               (ps:chain -math
                         (max 6 (ps:chain -math
                                          (min 72 (ps:chain -math (round size))))))))
           (defun use-atlas-font (ctx)
             (setf (ps:@ ctx font) atlas-font
                   (ps:@ ctx text-align) "left"
                   (ps:@ ctx text-baseline) "alphabetic"
                   (ps:@ ctx image-smoothing-enabled) ps:false))
           (defun wide-p (cp)
             (or (and (>= cp #x1100) (<= cp #x115f))
                 (and (>= cp #x2e80) (<= cp #xa4cf))
                 (and (>= cp #xac00) (<= cp #xd7a3))
                 (and (>= cp #xf900) (<= cp #xfaff))
                 (and (>= cp #xfe10) (<= cp #xfe6f))
                 (and (>= cp #xff00) (<= cp #xff60))
                 (and (>= cp #x1f300) (<= cp #x1f64f))
                 (and (>= cp #x1f900) (<= cp #x1f9ff))))
           (defun box-mask (cp)
             (cond ((= cp #x2500) 3) ((= cp #x2501) 3) ((= cp #x2550) 3)
                   ((= cp #x2502) 12) ((= cp #x2503) 12) ((= cp #x2551) 12)
                   ((= cp #x250c) 10) ((= cp #x2554) 10) ((= cp #x256d) 10)
                   ((= cp #x2510) 9) ((= cp #x2557) 9) ((= cp #x256e) 9)
                   ((= cp #x2514) 6) ((= cp #x255a) 6) ((= cp #x2570) 6)
                   ((= cp #x2518) 5) ((= cp #x255d) 5) ((= cp #x256f) 5)
                   ((= cp #x251c) 14) ((= cp #x2560) 14)
                   ((= cp #x2524) 13) ((= cp #x2563) 13)
                   ((= cp #x252c) 11) ((= cp #x2566) 11)
                   ((= cp #x2534) 7) ((= cp #x2569) 7)
                   ((= cp #x253c) 15) ((= cp #x256c) 15)
                   (t 0)))
           (defun draw-box (ctx cp x y w h)
             (let ((mask (box-mask cp))
                   (thickness (ps:chain -math (max 1 (ps:chain -math (round (/ (ps:chain -math (min w h)) 8))))))
                   (mid-x (ps:chain -math (floor (+ x (/ w 2)))))
                   (mid-y (ps:chain -math (floor (+ y (/ h 2))))))
               (cond ((> mask 0)
                      (let ((left (ps:chain -math (floor (- (+ y (/ h 2)) (/ thickness 2)))))
                            (top (ps:chain -math (floor (- (+ x (/ w 2)) (/ thickness 2))))))
                        (setf (ps:@ ctx fill-style) "#000000")
                        (when (> (logand mask 1) 0)
                          (ps:chain ctx (fill-rect x left (- mid-x x) thickness)))
                        (when (> (logand mask 2) 0)
                          (ps:chain ctx (fill-rect mid-x left (- (+ x w) mid-x) thickness)))
                        (when (> (logand mask 4) 0)
                          (ps:chain ctx (fill-rect top y thickness (- mid-y y))))
                        (when (> (logand mask 8) 0)
                          (ps:chain ctx (fill-rect top mid-y thickness (- (+ y h) mid-y)))))
                      t)
                     ((= cp #x2588)
                      (setf (ps:@ ctx fill-style) "#000000")
                      (ps:chain ctx (fill-rect x y w h))
                      t)
                     ((= cp #x2580)
                      (setf (ps:@ ctx fill-style) "#000000")
                      (ps:chain ctx (fill-rect x y w (- mid-y y)))
                      t)
                     ((= cp #x2584)
                      (setf (ps:@ ctx fill-style) "#000000")
                      (ps:chain ctx (fill-rect x mid-y w (- (+ y h) mid-y)))
                      t)
                     ((= cp #x258c)
                      (setf (ps:@ ctx fill-style) "#000000")
                      (ps:chain ctx (fill-rect x y (- mid-x x) h))
                      t)
                     ((= cp #x2590)
                      (setf (ps:@ ctx fill-style) "#000000")
                      (ps:chain ctx (fill-rect mid-x y (- (+ x w) mid-x) h))
                      t)
                     ((and (>= cp #x2591) (<= cp #x2593))
                      (setf (ps:@ ctx fill-style)
                            (cond ((= cp #x2591) "#c0c0c0")
                                  ((= cp #x2592) "#808080")
                                  (t "#404040")))
                      (ps:chain ctx (fill-rect x y w h))
                      t)
                     (t ps:false))))
           (defun paint-glyph (ctx ch x y w h)
             (let* ((cp (ps:chain ch (code-point-at 0)))
                    (x0 (ps:chain -math (round (+ x atlas-pad))))
                    (y0 (ps:chain -math (round (+ y atlas-pad))))
                    (cw (ps:chain -math (max 1 (- w (* atlas-pad 2)))))
                    (content-h (ps:chain -math (max 1 (- h (* atlas-pad 2))))))
               (ps:chain ctx (save))
               (ps:chain ctx (begin-path))
               (ps:chain ctx (rect x y w h))
               (ps:chain ctx (clip))
               (setf (ps:@ ctx fill-style) "#ffffff")
               (ps:chain ctx (fill-rect x y w h))
               (unless (draw-box ctx cp x0 y0 cw content-h)
                 (setf (ps:@ ctx fill-style) "#000000")
                 (ps:chain ctx (fill-text ch x0 (ps:chain -math (round (+ y0 glyph-baseline))))))
               (ps:chain ctx (restore))))
           (defun measure ()
             (let* ((dpr (or (ps:@ window device-pixel-ratio) 1))
                    (px (ps:chain -math (max 1 (ps:chain -math (round (* (font-px) dpr))))))
                    (probe (ps:chain (ps:chain document (create-element "canvas"))
                                     (get-context "2d")))
                    (metrics nil)
                    (advance 8)
                    (ascent px)
                    (descent 2))
               (setf atlas-font (font-of px)
                     (ps:@ probe font) atlas-font
                     (ps:@ probe text-baseline) "alphabetic"
                     metrics (ps:chain probe (measure-text "M"))
                     advance (ps:chain -math (ceil (or (ps:@ metrics width) 8)))
                     ascent (ps:chain -math
                                      (round (or (ps:@ metrics font-bounding-box-ascent)
                                                 (* px 0.8))))
                     descent (ps:chain -math
                                       (ceil (or (ps:@ metrics font-bounding-box-descent)
                                                 (* px 0.2))))
                     device-cell-w (ps:chain -math (max 1 advance))
                     glyph-baseline (ps:chain -math (max 1 ascent))
                     device-cell-h (ps:chain -math (max px (+ glyph-baseline descent)))
                     css-cell-w (/ device-cell-w dpr)
                     css-cell-h (/ device-cell-h dpr))))
           (defun compile-shader (type source)
             (let ((shader (ps:chain gl (create-shader type))))
               (ps:chain gl (shader-source shader source))
               (ps:chain gl (compile-shader shader))
               shader))
           (defun bind-atlas-metrics ()
             (when (and atlas atlas-grid-location)
               (ps:chain gl (uniform2f atlas-grid-location
                                       (ps:@ atlas cols)
                                       (ps:@ atlas rows)))
               (when atlas-inset-location
                 (ps:chain gl (uniform2f atlas-inset-location
                                         (/ (ps:@ atlas pad) (ps:@ atlas cell-w))
                                         (/ (ps:@ atlas pad) (ps:@ atlas cell-h)))))))
           (defun build-atlas (cell-w cell-h)
             (let* ((atlas-cols 16)
                    (atlas-rows 32)
                    (drawn 95)
                    (full-w (+ cell-w (* atlas-pad 2)))
                    (full-h (+ cell-h (* atlas-pad 2)))
                    (sheet (ps:chain document (create-element "canvas")))
                    (ctx nil)
                    (texture (ps:chain gl (create-texture))))
               (setf (ps:@ sheet width) (* atlas-cols full-w)
                     (ps:@ sheet height) (* atlas-rows full-h)
                     (ps:@ sheet style position) "fixed"
                     (ps:@ sheet style left) "-10000px"
                     (ps:@ sheet style top) "0"
                     ctx (ps:chain sheet (get-context "2d" (ps:create alpha ps:false))))
               (unless (ps:@ sheet parent-node)
                 (ps:chain document body (append-child sheet)))
               (use-atlas-font ctx)
               (setf (ps:@ ctx fill-style) "#ffffff")
               (ps:chain ctx (fill-rect 0 0 (ps:@ sheet width) (ps:@ sheet height)))
               (loop for i from 0 below drawn
                     for column = (ps:rem i atlas-cols)
                     for row = (ps:chain -math (floor (/ i atlas-cols)))
                     do (paint-glyph ctx
                                     (ps:chain -string (from-char-code (+ 32 i)))
                                     (* column full-w)
                                     (* row full-h)
                                     full-w
                                     full-h))
               (ps:chain gl (bind-texture (ps:@ gl "TEXTURE_2D") texture))
               (ps:chain gl (pixel-storei (ps:@ gl "UNPACK_FLIP_Y_WEBGL") ps:false))
               (ps:chain gl (pixel-storei (ps:@ gl "UNPACK_PREMULTIPLY_ALPHA_WEBGL") ps:false))
               (ps:chain gl (tex-image2-d (ps:@ gl "TEXTURE_2D") 0 (ps:@ gl "RGBA")
                                          (ps:@ gl "RGBA") (ps:@ gl "UNSIGNED_BYTE")
                                          sheet))
               (setf (ps:@ window nyxt-vt-log)
                     (ps:+ "atlas " (ps:@ sheet width) "x" (ps:@ sheet height)
                           " err " (ps:chain gl (get-error))))
               (ps:chain gl (tex-parameteri (ps:@ gl "TEXTURE_2D")
                                            (ps:@ gl "TEXTURE_MIN_FILTER")
                                            (ps:@ gl "NEAREST")))
               (ps:chain gl (tex-parameteri (ps:@ gl "TEXTURE_2D")
                                            (ps:@ gl "TEXTURE_MAG_FILTER")
                                            (ps:@ gl "NEAREST")))
               (ps:chain gl (tex-parameteri (ps:@ gl "TEXTURE_2D")
                                            (ps:@ gl "TEXTURE_WRAP_S")
                                            (ps:@ gl "CLAMP_TO_EDGE")))
               (ps:chain gl (tex-parameteri (ps:@ gl "TEXTURE_2D")
                                            (ps:@ gl "TEXTURE_WRAP_T")
                                            (ps:@ gl "CLAMP_TO_EDGE")))
               (ps:create texture texture
                          cols atlas-cols
                          rows atlas-rows
                          ctx ctx
                          sheet sheet
                          cell-w full-w
                          cell-h full-h
                          pad atlas-pad
                          count drawn
                          glyphs (ps:create)
                          wide (ps:create))))
           (defun glyph-index (cp half)
             (cond
               ((and (>= cp 32) (<= cp 126)) (- cp 32))
               ((not atlas) 31)
               (t (let ((known (aref (ps:@ atlas glyphs) cp)))
                    (if (not (eql known undefined))
                        (+ known (if (and half (aref (ps:@ atlas wide) cp)) half 0))
                        (if (>= (ps:@ atlas count) (* (ps:@ atlas cols) (ps:@ atlas rows)))
                            31
                            (let* ((index (ps:@ atlas count))
                                   (column (ps:rem index (ps:@ atlas cols)))
                                   (row (ps:chain -math (floor (/ index (ps:@ atlas cols)))))
                                   (span (and (wide-p cp)
                                              (< column (1- (ps:@ atlas cols)))
                                              (< (+ index 1)
                                                 (* (ps:@ atlas cols) (ps:@ atlas rows))))))
                              (setf (aref (ps:@ atlas glyphs) cp) index
                                    (ps:@ atlas count) (+ index (if span 2 1)))
                              (when span
                                (setf (aref (ps:@ atlas wide) cp) t))
                              (let* ((x (* column (ps:@ atlas cell-w)))
                                     (y (* row (ps:@ atlas cell-h)))
                                     (ch (ps:chain -string (from-code-point cp))))
                                (use-atlas-font (ps:@ atlas ctx))
                                (if span
                                    (paint-glyph (ps:@ atlas ctx) ch x y
                                                 (* (ps:@ atlas cell-w) 2)
                                                 (ps:@ atlas cell-h))
                                    (paint-glyph (ps:@ atlas ctx) ch x y
                                                 (ps:@ atlas cell-w)
                                                 (ps:@ atlas cell-h))))
                              (ps:chain gl (bind-texture (ps:@ gl "TEXTURE_2D")
                                                        (ps:@ atlas texture)))
                              (ps:chain gl (pixel-storei (ps:@ gl "UNPACK_FLIP_Y_WEBGL") ps:false))
                              (ps:chain gl (tex-image2-d (ps:@ gl "TEXTURE_2D") 0
                                                        (ps:@ gl "RGBA")
                                                        (ps:@ gl "RGBA")
                                                        (ps:@ gl "UNSIGNED_BYTE")
                                                        (ps:@ atlas sheet)))
                              (+ index (if (and span half) half 0)))))))))
           (defun init-gl ()
             (setf gl (ps:chain canvas
                                (get-context "webgl2"
                                             (ps:create antialias ps:false
                                                        alpha ps:false
                                                        preserve-drawing-buffer t))))
             (when gl
               (let* ((program (ps:chain gl (create-program)))
                      (quad (ps:new (-float32-array (array 0 0 1 0 0 1
                                                          0 1 1 0 1 1))))
                      (dpr (or (ps:@ window device-pixel-ratio) 1)))
                 (ps:chain gl (attach-shader
                               program
                               (compile-shader (ps:@ gl "VERTEX_SHADER") vertex)))
                 (ps:chain gl (attach-shader
                               program
                               (compile-shader (ps:@ gl "FRAGMENT_SHADER") fragment)))
                 (ps:chain gl (link-program program))
                 (setf (ps:@ window nyxt-vt-log)
                       (ps:chain gl (get-program-info-log program)))
                 (ps:chain gl (use-program program))
                 (setf instance-buffer (ps:chain gl (create-buffer))
                       quad-buffer (ps:chain gl (create-buffer))
                       atlas (build-atlas device-cell-w device-cell-h)
                       grid-location (ps:chain gl (get-uniform-location program "grid"))
                       gl-program program
                       atlas-grid-location (ps:chain gl (get-uniform-location program "atlasGrid"))
                       atlas-inset-location (ps:chain gl (get-uniform-location program "atlasInset")))
                 (bind-atlas-metrics)
                 (ps:chain gl (uniform1i (ps:chain gl (get-uniform-location program "atlas")) 0))
                 (ps:chain gl (bind-buffer (ps:@ gl "ARRAY_BUFFER") quad-buffer))
                 (ps:chain gl (buffer-data (ps:@ gl "ARRAY_BUFFER") quad
                                           (ps:@ gl "STATIC_DRAW")))
                 (ps:chain gl (enable-vertex-attrib-array 0))
                 (ps:chain gl (vertex-attrib-pointer 0 2 (ps:@ gl "FLOAT") ps:false 0 0))
                 (ps:chain gl (bind-buffer (ps:@ gl "ARRAY_BUFFER") instance-buffer))
                 (ps:chain gl (enable-vertex-attrib-array 1))
                 (ps:chain gl (enable-vertex-attrib-array 2))
                 (ps:chain gl (enable-vertex-attrib-array 3))
                 (ps:chain gl (enable-vertex-attrib-array 4))
                 (ps:chain gl (vertex-attrib-pointer 1 1 (ps:@ gl "FLOAT") ps:false 32 0))
                 (ps:chain gl (vertex-attrib-pointer 2 3 (ps:@ gl "FLOAT") ps:false 32 4))
                 (ps:chain gl (vertex-attrib-pointer 3 3 (ps:@ gl "FLOAT") ps:false 32 16))
                 (ps:chain gl (vertex-attrib-pointer 4 1 (ps:@ gl "FLOAT") ps:false 32 28))
                 (ps:chain gl (vertex-attrib-divisor 1 1))
                 (ps:chain gl (vertex-attrib-divisor 2 1))
                 (ps:chain gl (vertex-attrib-divisor 3 1))
                 (ps:chain gl (vertex-attrib-divisor 4 1))
                 (ps:chain gl (active-texture (ps:@ gl "TEXTURE0")))
                 (ps:chain gl (bind-texture (ps:@ gl "TEXTURE_2D") (ps:@ atlas texture))))))
           (defun draw ()
             (when (and gl gl-program atlas quad-buffer (ps:> cols 0) (ps:> rows 0))
               (ps:chain gl (use-program gl-program))
               (ps:chain gl (active-texture (ps:@ gl "TEXTURE0")))
               (ps:chain gl (bind-texture (ps:@ gl "TEXTURE_2D") (ps:@ atlas texture)))
               (ps:chain gl (bind-buffer (ps:@ gl "ARRAY_BUFFER") quad-buffer))
               (ps:chain gl (enable-vertex-attrib-array 0))
               (ps:chain gl (vertex-attrib-pointer 0 2 (ps:@ gl "FLOAT") ps:false 0 0))
               (ps:chain gl (bind-buffer (ps:@ gl "ARRAY_BUFFER") instance-buffer))
               (ps:chain gl (enable-vertex-attrib-array 1))
               (ps:chain gl (enable-vertex-attrib-array 2))
               (ps:chain gl (enable-vertex-attrib-array 3))
               (ps:chain gl (enable-vertex-attrib-array 4))
               (ps:chain gl (vertex-attrib-pointer 1 1 (ps:@ gl "FLOAT") ps:false 32 0))
               (ps:chain gl (vertex-attrib-pointer 2 3 (ps:@ gl "FLOAT") ps:false 32 4))
               (ps:chain gl (vertex-attrib-pointer 3 3 (ps:@ gl "FLOAT") ps:false 32 16))
               (ps:chain gl (vertex-attrib-pointer 4 1 (ps:@ gl "FLOAT") ps:false 32 28))
               (ps:chain gl (vertex-attrib-divisor 1 1))
               (ps:chain gl (vertex-attrib-divisor 2 1))
               (ps:chain gl (vertex-attrib-divisor 3 1))
               (ps:chain gl (vertex-attrib-divisor 4 1))
               (bind-atlas-metrics)
               (ps:chain gl (viewport 0 0 (ps:@ canvas width) (ps:@ canvas height)))
               (ps:chain gl (clear-color bg-r bg-g bg-b 1))
               (ps:chain gl (clear (ps:@ gl "COLOR_BUFFER_BIT")))
               (ps:chain gl (uniform2f grid-location cols rows))
               (ps:chain gl (draw-arrays-instanced (ps:@ gl "TRIANGLES") 0 6 (* cols rows)))))
           (defun decode (b64)
             (if (ps:@ -uint8-array from-base64)
                 (ps:chain -uint8-array (from-base64 b64))
                 (let* ((binary ((ps:@ window atob) b64))
                        (bytes (ps:new (-uint8-array (ps:@ binary length)))))
                   (loop for i from 0 below (ps:@ binary length)
                         do (setf (aref bytes i) (ps:chain binary (char-code-at i))))
                   bytes)))
           (defun frame (b64)
             (when gl
               (let* ((bytes (decode b64))
                      (view (ps:new (-data-view (ps:@ bytes buffer))))
                      (n-cols (ps:chain view (get-uint16 0 t)))
                      (n-rows (ps:chain view (get-uint16 2 t)))
                      (count (* n-cols n-rows))
                      (chars (array)))
                 (when (or (null instance-floats)
                           (< (ps:@ instance-floats length) (* count 8)))
                   (setf instance-floats (ps:new (-float32-array (* count 8))))
                   (ps:chain gl (bind-buffer (ps:@ gl "ARRAY_BUFFER") instance-buffer))
                   (ps:chain gl (buffer-data (ps:@ gl "ARRAY_BUFFER")
                                             (* count 8 4)
                                             (ps:@ gl "DYNAMIC_DRAW"))))
                 (loop for i from 0 below count
                       for base = (+ 16 (* i 12))
                       for cp = (ps:chain view (get-uint32 base t))
                       for flags = (ps:chain view (get-uint8 (+ base 7)))
                       for glyph = (glyph-index cp (if (> (logand flags 8) 0) 1 0))
                       for origin = (* i 8)
                       do (setf (aref instance-floats origin) glyph
                                (aref instance-floats (+ origin 1))
                                (/ (ps:chain view (get-uint8 (+ base 4))) 255)
                                (aref instance-floats (+ origin 2))
                                (/ (ps:chain view (get-uint8 (+ base 5))) 255)
                                (aref instance-floats (+ origin 3))
                                (/ (ps:chain view (get-uint8 (+ base 6))) 255)
                                (aref instance-floats (+ origin 4))
                                (/ (ps:chain view (get-uint8 (+ base 8))) 255)
                                (aref instance-floats (+ origin 5))
                                (/ (ps:chain view (get-uint8 (+ base 9))) 255)
                                (aref instance-floats (+ origin 6))
                                (/ (ps:chain view (get-uint8 (+ base 10))) 255)
                                (aref instance-floats (+ origin 7))
                                (logand flags 3))
                          (ps:chain chars (push (ps:chain -string (from-code-point cp))))
                          (when (= (ps:rem (+ i 1) n-cols) 0)
                            (ps:chain chars (push "\n"))))
                 (setf cols n-cols
                       rows n-rows
                       screen-text (ps:chain chars (join "")))
                 (ps:chain gl (bind-buffer (ps:@ gl "ARRAY_BUFFER") instance-buffer))
                 (ps:chain gl (buffer-sub-data (ps:@ gl "ARRAY_BUFFER") 0
                                               (ps:chain instance-floats
                                                         (subarray 0 (* count 8)))))
                 (draw)
                 (incf frames))))
           (defun fit ()
             (let* ((rect (ps:chain node (get-bounding-client-rect)))
                    (next-cols (ps:chain -math
                                         (max 2 (ps:chain -math
                                                          (floor (/ (ps:@ rect width)
                                                                    css-cell-w))))))
                    (next-rows (ps:chain -math
                                         (max 1 (ps:chain -math
                                                          (floor (/ (ps:@ rect height)
                                                                    css-cell-h))))))
                    (dpr (or (ps:@ window device-pixel-ratio) 1))
                    (next-width (* next-cols device-cell-w))
                    (next-height (* next-rows device-cell-h)))
               (setf (ps:@ canvas style width)
                     (ps:+ (/ next-width dpr) "px")
                     (ps:@ canvas style height)
                     (ps:+ (/ next-height dpr) "px"))
               (when (or (/= (ps:@ canvas width) next-width)
                         (/= (ps:@ canvas height) next-height))
                 (setf (ps:@ canvas width) next-width
                       (ps:@ canvas height) next-height)
                 (draw))
               (when (or (/= next-cols sent-cols) (/= next-rows sent-rows))
                 (setf sent-cols next-cols
                       sent-rows next-rows)
                 (send "resize"
                       (ps:+ "buffer=" buffer-id
                             "&columns=" next-cols
                             "&rows=" next-rows)))))
           (defun on-key (event)
             (ps:chain event (prevent-default))
             (let* ((mods (+ (if (ps:@ event shift-key) 1 0)
                             (if (ps:@ event ctrl-key) 2 0)
                             (if (ps:@ event alt-key) 4 0)
                             (if (ps:@ event meta-key) 8 0)))
                    (text (if (= (ps:@ (ps:@ event key) length) 1)
                              (ps:@ event key)
                              ""))
                    (cp (if (ps:> (ps:@ text length) 0)
                            (ps:chain text (code-point-at 0))
                            0)))
               (send "key"
                     (ps:+ "buffer=" buffer-id
                           "&key=" (encode-u-r-i-component (ps:@ event key))
                           "&mods=" mods
                           "&cp=" cp
                           "&text=" (encode-u-r-i-component text)))))
           (defun cell-of (event)
             (let ((rect (ps:chain canvas (get-bounding-client-rect))))
               (ps:create x (ps:chain -math
                                      (max 0 (ps:chain -math
                                                       (min (- cols 1)
                                                            (ps:chain -math
                                                                      (floor (* (/ (- (ps:@ event client-x)
                                                                                      (ps:@ rect left))
                                                                                   (ps:@ rect width))
                                                                                cols)))))))
                          y (ps:chain -math
                                      (max 0 (ps:chain -math
                                                       (min (- rows 1)
                                                            (ps:chain -math
                                                                      (floor (* (/ (- (ps:@ event client-y)
                                                                                      (ps:@ rect top))
                                                                                   (ps:@ rect height))
                                                                                rows))))))))))
           (defun selection-text (start end)
             (let* ((x1 (ps:@ start x))
                    (y1 (ps:@ start y))
                    (x2 (ps:@ end x))
                    (y2 (ps:@ end y))
                    (lines (ps:chain screen-text (split "\n")))
                    (parts (array)))
               (when (or (> y1 y2) (and (= y1 y2) (> x1 x2)))
                 (let ((swap-x x1) (swap-y y1))
                   (setf x1 x2 y1 y2 x2 swap-x y2 swap-y)))
               (loop for y from y1 to y2
                     for line = (or (aref lines y) "")
                     for from = (if (= y y1) x1 0)
                     for to = (if (= y y2) (+ x2 1) (ps:@ line length))
                     do (ps:chain parts (push (ps:chain (ps:chain line (slice from to))
                                                       (replace (ps:regex "/\\s+$/") "")))))
               (ps:chain parts (join "\n"))))
           (let ((anchor nil)
                 (dragging ps:false))
             (defun on-down (event)
               (when (= (ps:@ event button) 0)
                 (ps:chain event (prevent-default))
                 (ps:chain canvas (focus))
                 (setf anchor (cell-of event)
                       dragging t)
                 (send "clear-selection" (ps:+ "buffer=" buffer-id))))
             (defun on-move (event)
               (when (and dragging anchor)
                 (let ((cell (cell-of event)))
                   (send "select"
                         (ps:+ "buffer=" buffer-id
                               "&x=" (ps:@ anchor x) "&y=" (ps:@ anchor y)
                               "&x2=" (ps:@ cell x) "&y2=" (ps:@ cell y))))))
             (defun on-up (event)
               (when dragging
                 (let ((cell (cell-of event)))
                   (setf dragging ps:false)
                   (if (and (= (ps:@ cell x) (ps:@ anchor x))
                            (= (ps:@ cell y) (ps:@ anchor y)))
                       (send "clear-selection" (ps:+ "buffer=" buffer-id))
                       (send "copy"
                             (ps:+ "buffer=" buffer-id
                                   "&text="
                                   (encode-u-r-i-component
                                    (selection-text anchor cell))))))))
             (defun on-wheel (event)
               (ps:chain event (prevent-default))
               (send "scroll"
                     (ps:+ "buffer=" buffer-id
                           "&delta=" (if (> (ps:@ event delta-y) 0) 3 -3)))))
           (defun on-paste (event)
             (ps:chain event (prevent-default))
             (send "paste"
                   (ps:+ "buffer=" buffer-id
                         "&text="
                         (encode-u-r-i-component
                          (ps:chain (ps:@ event clipboard-data)
                                    (get-data "text/plain"))))))
           (defun rebuild-atlas ()
             (when (and gl gl-program)
               (when (and atlas (ps:@ atlas sheet) (ps:@ (ps:@ atlas sheet) parent-node))
                 (ps:chain (ps:@ (ps:@ atlas sheet) parent-node)
                           (remove-child (ps:@ atlas sheet))))
               (ps:chain gl (use-program gl-program))
               (setf atlas (build-atlas device-cell-w device-cell-h))
               (bind-atlas-metrics)
               (ps:chain gl (bind-texture (ps:@ gl "TEXTURE_2D")
                                          (ps:@ atlas texture)))))
           (defun refit ()
             (measure)
             (rebuild-atlas)
             (fit)
             (draw))
           (measure)
           (init-gl)
           (when gl
             (when (ps:@ window nyxt-vt-cleanup)
               (ps:chain window (nyxt-vt-cleanup)))
             (setf (ps:@ window nyxt-vt-cleanup)
                   (lambda ()
                     (ps:chain canvas (remove-event-listener "keydown" on-key))
                     (ps:chain canvas (remove-event-listener "mousedown" on-down))
                     (ps:chain window (remove-event-listener "mousemove" on-move))
                     (ps:chain window (remove-event-listener "mouseup" on-up))
                     (ps:chain canvas (remove-event-listener "wheel" on-wheel))
                     (ps:chain canvas (remove-event-listener "paste" on-paste))
                     (ps:chain window (remove-event-listener "resize" fit))))
             (setf (ps:@ window nyxt-vt)
                   (ps:create frame frame
                              font-size (ps:lisp font-size)
                              refit refit
                              text (lambda () screen-text)
                              frames (lambda () frames)
                              error (lambda () (ps:chain gl (get-error)))))
             (ps:chain canvas (add-event-listener "keydown" on-key))
             (ps:chain canvas (add-event-listener "mousedown" on-down))
             (ps:chain window (add-event-listener "mousemove" on-move))
             (ps:chain window (add-event-listener "mouseup" on-up))
             (ps:chain canvas (add-event-listener "wheel" on-wheel
                                                  (ps:create passive ps:false)))
             (ps:chain canvas (add-event-listener "paste" on-paste))
             (ps:chain window (add-event-listener "resize" fit))
             (fit)
             (ps:chain canvas (focus))
             (send "ready" (ps:+ "buffer=" buffer-id))
             (when (and (ps:@ document fonts) (ps:@ document fonts load))
               (ps:chain (ps:chain document fonts (load atlas-font))
                         (then (lambda ()
                                 (refit)
                                 (setf (ps:@ window nyxt-vt-log)
                                       (ps:+ (or (ps:@ window nyxt-vt-log) "")
                                             " font "
                                             (if (ps:chain document fonts (check atlas-font))
                                                 "ok"
                                                 "missing")
                                             " "
                                             (font-px)
                                             "px")))))))))))))

(defun install-vt-ui (mode)
  "Load the WebGL renderer into MODE's buffer."
  (ffi-buffer-evaluate-javascript-async (buffer mode) (vt-script (buffer mode))))

(defun query-integer (params name)
  "Return the integer query parameter NAME, or NIL."
  (let ((value (assoc-value params name :test #'equal)))
    (when (and value (plusp (length value)))
      (handler-case (parse-integer value) (error () nil)))))

(defun request-action (url)
  "Return the path or host action of URL."
  (let* ((uri (quri:uri url))
         (path (string-trim '(#\/) (or (quri:uri-path uri) ""))))
    (if (plusp (length path))
        path
        (or (quri:uri-host uri) ""))))

(defun buffer-by-id (id-string)
  "Return the buffer whose id is ID-STRING."
  (let ((id (and id-string (handler-case (parse-integer id-string) (error () nil)))))
    (when id
      (find id (buffer-list) :key #'id))))

(defun resize-screen (mode columns rows)
  "Resize MODE's emulator and its pseudoterminal to COLUMNS by ROWS."
  (bt:with-lock-held ((lock mode))
    (when (and (screen mode) (plusp columns) (plusp rows))
      (setf (columns mode) columns
            (rows mode) rows)
      (cl-libghostty:terminal-resize (screen mode) columns rows)
      (when (pty mode)
        (terminal:resize (pty mode) rows columns)))))

(defun key-text (text mods)
  "Return TEXT when it should be encoded as characters."
  (if (and (stringp text)
           (plusp (length text))
           (zerop (logand mods (logior cl-libghostty:+mod-ctrl+
                                       cl-libghostty:+mod-alt+
                                       cl-libghostty:+mod-super+))))
      text
      ""))

(defun encode-browser-key (mode name mods codepoint text)
  "Return the octets NAME produces under MODE's keyboard modes."
  (let ((event (key-event mode))
        (encoder (key-encoder mode))
        (key (browser-key (or name ""))))
    (cl-libghostty:key-event-set-action event :press)
    (cl-libghostty:key-event-set-key event key)
    (cl-libghostty:key-event-set-mods event (or mods 0))
    (cl-libghostty:key-event-set-codepoint event (or codepoint 0))
    (cl-libghostty:key-event-set-text event (key-text text mods))
    (bt:with-lock-held ((lock mode))
      (when (screen mode)
        (cl-libghostty:key-encoder-set-from-terminal encoder (screen mode))))
    (cl-libghostty:encode-key encoder event)))

(defun handle-terminal-request (url)
  "Apply a key, paste, resize, or ready request from the terminal page."
  (let* ((params (quri:uri-query-params (quri:uri url)))
         (buffer (buffer-by-id (assoc-value params "buffer" :test #'equal)))
         (mode (and buffer (find-mode buffer 'terminal-mode)))
         (pty (and mode (pty mode))))
    (when pty
      (let ((action (request-action url)))
        (cond
          ((string= action "key")
           (let ((octets (encode-browser-key
                          mode
                          (assoc-value params "key" :test #'equal)
                          (query-integer params "mods")
                          (query-integer params "cp")
                          (assoc-value params "text" :test #'equal))))
             (when (plusp (length octets))
               (bt:with-lock-held ((lock mode))
                 (when (screen mode)
                   (scroll-to-bottom (screen mode))))
               (terminal:enqueue-input pty octets))))
          ((string= action "paste")
           (let* ((text (or (assoc-value params "text" :test #'equal) ""))
                  (octets (cl-libghostty:paste-encode
                           (babel:string-to-octets text :encoding :utf-8))))
             (when (plusp (length octets))
               (bt:with-lock-held ((lock mode))
                 (when (screen mode)
                   (scroll-to-bottom (screen mode))))
               (terminal:enqueue-input pty octets))))
          ((string= action "scroll")
           (let ((delta (query-integer params "delta")))
             (when delta
               (bt:with-lock-held ((lock mode))
                 (when (screen mode)
                   (scroll-viewport (screen mode) delta))))))
          ((string= action "select")
           (setf (selection-start mode)
                 (cons (or (query-integer params "x") 0)
                       (or (query-integer params "y") 0))
                 (selection-end mode)
                 (cons (or (query-integer params "x2") 0)
                       (or (query-integer params "y2") 0))
                 (selection-dirty-p mode) t))
          ((string= action "copy")
           (let ((text (or (assoc-value params "text" :test #'equal) "")))
             (when (plusp (length text))
               (setf (clipboard-text) text))))
          ((string= action "clear-selection")
           (setf (selection-start mode) nil
                 (selection-end mode) nil
                 (selection-dirty-p mode) t))
          ((string= action "resize")
           (let ((columns (query-integer params "columns"))
                 (rows (query-integer params "rows")))
             (when (and columns rows)
               (resize-screen mode columns rows))))
          ((string= action "ready")
           (setf (page-ready-p mode) t)))))))

(define-internal-scheme "nyxt-terminal"
    (lambda (url)
      (handler-case (handle-terminal-request url)
        (error (condition)
          (log:debug "Terminal request failed: ~a" condition)))
      (values "ok" "text/plain" 200
              '(("Access-Control-Allow-Origin" . "*")
                ("Access-Control-Allow-Methods" . "GET, OPTIONS"))
              "OK"))
  (lambda (condition)
    (log:debug "Terminal scheme failed: ~a" condition)))

(define-mode terminal-mode ()
  "A terminal in the buffer.
The shell runs on a pseudoterminal in this process. Output is emulated with
libghostty-vt and drawn on a WebGL grid.
The buffer title is the title the program sets. While another program is in the
foreground, the title is that program's name.
Keys go to the terminal. C-+ and C-= increase the font size, C-- decreases it,
and C-0 restores it. C-M-z runs `toggle-terminal-keys', which gives the keys
to Nyxt instead. The same binding sends them back."
  ((keyscheme-map
    (define-keyscheme-map "terminal-mode" ()
      keyscheme:default
      (list
       "C-M-z" 'toggle-terminal-keys
       "C-+" 'increase-font-size
       "C-=" 'increase-font-size
       "C-hyphen" 'decrease-font-size
       "C-0" 'reset-font-size)
      keyscheme:vi-insert
      (list
       "C-M-z" 'toggle-terminal-keys
       "C-+" 'increase-font-size
       "C-=" 'increase-font-size
       "C-hyphen" 'decrease-font-size
       "C-0" 'reset-font-size)))
   (font-size
    *default-font-size*
    :documentation "Face size in CSS pixels.
C-+ and C-= increase it, C-- decreases it, and C-0 restores the size from when
the mode was enabled.")
   (font-size-default
    nil
    :type (or null integer)
    :export nil
    :documentation "Size `reset-font-size' restores.")
   (program
    nil
    :type (or null string)
    :documentation "Program run in the terminal.
When NIL, the SHELL environment variable is used, or /bin/sh. The program is
started with -i.")
   (keys-to-terminal-p
    t
    :type boolean
    :documentation "When non-nil, keys are delivered to the terminal.
When NIL, Nyxt keybindings handle them. `toggle-terminal-keys' switches this.")
   (pty nil :export nil :documentation "The pseudoterminal for this buffer.")
   (screen nil :export nil :documentation "The libghostty terminal.")
   (render-state nil :export nil :documentation "The viewport snapshot.")
   (row-iterator nil :export nil :documentation "Row cursor for the snapshot.")
   (row-cells nil :export nil :documentation "Cell cursor for one row.")
   (key-encoder nil :export nil :documentation "Keyboard encoder for this terminal.")
   (key-event nil :export nil :documentation "Reused key event.")
   (lock (bt:make-lock "terminal-vt") :export nil :documentation "Serializes emulator access.")
   (frame-thread nil :export nil :documentation "Thread that publishes viewports.")
   (running-p nil :type boolean :export nil :documentation "Whether the frame thread should run.")
   (page-ready-p
    nil
    :type boolean
    :export nil
    :documentation "Whether the page has created its WebGL context.")
   (columns 80 :type integer :export nil :documentation "Viewport width in cells.")
   (rows 24 :type integer :export nil :documentation "Viewport height in cells.")
   (frame-octets nil :export nil :documentation "Packed viewport bytes.")
   (frame-length 0 :type integer :export nil :documentation "Used length of `frame-octets'.")
   (bytes-in 0 :type integer :export nil :documentation "Octets fed to the emulator.")
   (write-ticks
    0
    :type integer
    :export nil
    :documentation "Internal time spent in `terminal-write'.")
   (frames 0 :type integer :export nil :documentation "Viewports sent to the page.")
   (last-cursor
    nil
    :export nil
    :documentation "Column, row, and visibility sent in the last frame.")
   (foreground-name
    nil
    :export nil
    :documentation "Name of the program last seen in the foreground.")
   (title-from-program-p
    nil
    :type boolean
    :export nil
    :documentation "Whether the buffer title was set by the program itself.")
   (last-program-title
    ""
    :type string
    :export nil
    :documentation "The last title read from the emulator.")
   (selection-start
    nil
    :export nil
    :documentation "The cell where the mouse selection starts, or NIL.")
   (selection-end
    nil
    :export nil
    :documentation "The cell where the mouse selection ends, or NIL.")
   (selection-dirty-p
    nil
    :type boolean
    :export nil
    :documentation "Whether the selection changed since the last frame.")
   (delete-handler
    nil
    :export nil
    :documentation "Hook that closes the shell when the buffer is deleted.")
   (exit-handler nil :export nil :documentation "Hook that closes the shell when Nyxt quits."))
  (:toggler-command-p nil))

(defun viewport-cursor (state)
  "Return the column, row, and visibility of STATE's cursor."
  (multiple-value-list (cl-libghostty:render-cursor state)))

(defun publish-frame (mode)
  "Copy the screen into the render state and send it when cells or the cursor changed.
A cursor move does not dirty any row, so the cursor is compared with the last
frame."
  (let ((dirty nil)
        (selection-p nil)
        (cursor-moved nil))
    (bt:with-lock-held ((lock mode))
      (when (and (running-p mode) (page-ready-p mode) (screen mode))
        (setf selection-p (selection-dirty-p mode)
              (selection-dirty-p mode) nil
              dirty (cl-libghostty:render-state-update (render-state mode)
                                                       (screen mode)))
        (let ((cursor (viewport-cursor (render-state mode))))
          (setf cursor-moved (not (equal cursor (last-cursor mode))))
          (when (or selection-p cursor-moved (member dirty '(:partial :full)))
            (pack-viewport mode)
            (note-screen-title mode)
            (setf (last-cursor mode) cursor)))))
    (when (or selection-p cursor-moved (member dirty '(:partial :full)))
      (send-viewport mode)
      (bt:with-lock-held ((lock mode))
        (when (and (member dirty '(:partial :full)) (render-state mode))
          (cl-libghostty:render-state-clean (render-state mode)))))))

(defun program-argv (mode)
  "Return the program and arguments for MODE."
  (let ((program (or (and (stringp (program mode))
                          (plusp (length (program mode)))
                          (program mode))
                     (uiop:getenv "SHELL")
                     "/bin/sh")))
    (list program "-i")))

(defun free-screen (mode)
  "Release MODE's libghostty objects."
  (setf (last-cursor mode) nil)
  (when (key-event mode)
    (cl-libghostty:key-event-free (key-event mode))
    (setf (key-event mode) nil))
  (when (key-encoder mode)
    (cl-libghostty:key-encoder-free (key-encoder mode))
    (setf (key-encoder mode) nil))
  (when (row-cells mode)
    (cl-libghostty:row-cells-free (row-cells mode))
    (setf (row-cells mode) nil))
  (when (row-iterator mode)
    (cl-libghostty:row-iterator-free (row-iterator mode))
    (setf (row-iterator mode) nil))
  (when (render-state mode)
    (cl-libghostty:render-state-free (render-state mode))
    (setf (render-state mode) nil))
  (when (screen mode)
    (cl-libghostty:terminal-free (screen mode))
    (setf (screen mode) nil)))

(defun stop-vt (mode)
  "Stop MODE's frame thread, shell, and emulator."
  (setf (running-p mode) nil)
  (when (and (frame-thread mode)
             (bt:thread-alive-p (frame-thread mode))
             (not (eq (frame-thread mode) (bt:current-thread))))
    (bt:join-thread (frame-thread mode))
    (setf (frame-thread mode) nil))
  (when (pty mode)
    (terminal:stop (pty mode))
    (setf (pty mode) nil))
  (bt:with-lock-held ((lock mode))
    (free-screen mode))
  (when (buffer mode)
    (setf (forward-input-events-p (buffer mode)) nil)))

(defun detach-handlers (mode)
  "Remove MODE's buffer-delete and browser-exit hook handlers."
  (when (and (delete-handler mode) (buffer mode))
    (hooks:remove-hook (buffer-delete-hook (buffer mode)) (delete-handler mode))
    (setf (delete-handler mode) nil))
  (when (exit-handler mode)
    (hooks:remove-hook (before-exit-hook *browser*) (exit-handler mode))
    (setf (exit-handler mode) nil)))

(defun ensure-delete-handler (mode)
  "Close MODE's shell when its buffer is deleted."
  (unless (delete-handler mode)
    (setf (delete-handler mode)
          (make-instance 'hooks:handler
                         :name (gensym "TERMINAL-VT-DELETE")
                         :fn (lambda (buffer)
                               (declare (ignore buffer))
                               (detach-handlers mode)
                               (stop-vt mode))))
    (hooks:add-hook (buffer-delete-hook (buffer mode))
                    (delete-handler mode))))

(defun ensure-exit-handler (mode)
  "Close MODE's shell when Nyxt quits."
  (when (and *browser* (not (exit-handler mode)))
    (setf (exit-handler mode)
          (make-instance 'hooks:handler
                         :name (gensym "TERMINAL-VT-EXIT")
                         :fn (lambda ()
                               (stop-vt mode))))
    (hooks:add-hook (before-exit-hook *browser*)
                    (exit-handler mode))))

(defun open-screen (mode)
  "Create MODE's emulator, encoder, and frame thread."
  (setf (screen mode) (cl-libghostty:terminal-new (columns mode) (rows mode))
        (render-state mode) (cl-libghostty:render-state-new)
        (row-iterator mode) (cl-libghostty:row-iterator-new)
        (row-cells mode) (cl-libghostty:row-cells-new)
        (key-encoder mode) (cl-libghostty:key-encoder-new)
        (key-event mode) (cl-libghostty:key-event-new))
  (configure-scrollback (screen mode))
  (apply-theme-colors (screen mode))
  (setf (running-p mode) t
        (frame-thread mode)
        (bt:make-thread (lambda () (frame-loop mode)) :name "terminal")))

(defmethod enable ((mode terminal-mode) &key)
  (unless (font-size-default mode)
    (setf (font-size-default mode) (font-size mode)))
  (setf (forward-input-events-p (buffer mode)) (keys-to-terminal-p mode))
  (handler-case
      (progn
        (ensure-library)
        (ensure-scroll-library)
        (unless (screen mode)
          (open-screen mode))
        (unless (pty mode)
          (setf (pty mode)
                (terminal:start (program-argv mode)
                                :on-output (lambda (octets)
                                             (accept-output mode octets))
                                :on-exit (lambda ()
                                           (echo "Terminal exited."))))
          (feed-octets mode (terminal:take-transcript (pty mode)))
          (ensure-delete-handler mode)
          (ensure-exit-handler mode)))
    (error (condition)
      (echo-warning "Terminal failed to start: ~a" condition)
      (stop-vt mode))))

(defmethod disable ((mode terminal-mode) &key)
  (detach-handlers mode)
  (stop-vt mode))

(defmethod on-signal-load-finished ((mode terminal-mode) url title)
  (declare (ignore title))
  (when (screen mode)
    (install-vt-ui mode))
  url)

(define-internal-page terminal-page ()
    (:title "*Terminal*"
     :page-mode 'terminal-mode)
  "A libghostty terminal."
  (vt-markup))

(define-command-global terminal ()
  "Open a terminal."
  (let ((buffer (make-instance 'web-buffer)))
    (set-current-buffer buffer)
    (ffi-buffer-load buffer (quri:uri (nyxt-url 'terminal-page)))
    buffer))

(defun apply-font-size (mode)
  "Draw MODE at its `font-size'."
  (when (buffer mode)
    (ffi-buffer-evaluate-javascript-async
     (buffer mode)
     (ps:ps
       (when (ps:@ window nyxt-vt)
         (setf (ps:@ window nyxt-vt font-size) (ps:lisp (font-size mode)))
         (when (ps:@ window nyxt-vt refit)
           (ps:chain window nyxt-vt (refit))))))))

(defun set-font-size (buffer size)
  "Set BUFFER's terminal face to SIZE pixels."
  (let ((mode (find-mode buffer 'terminal-mode)))
    (if mode
        (let ((next (max 6 (min 72 size))))
          (setf (font-size mode) next)
          (apply-font-size mode)
          (echo "Terminal font size: ~a." next))
        (echo "This buffer has no terminal."))))

(define-command increase-font-size (buffer)
  "Increase the terminal font size."
  (let ((mode (find-mode buffer 'terminal-mode)))
    (set-font-size buffer (if mode (1+ (font-size mode)) *default-font-size*))))

(define-command decrease-font-size (buffer)
  "Decrease the terminal font size."
  (let ((mode (find-mode buffer 'terminal-mode)))
    (set-font-size buffer (if mode (1- (font-size mode)) *default-font-size*))))

(define-command reset-font-size (buffer)
  "Restore the terminal font size."
  (let ((mode (find-mode buffer 'terminal-mode)))
    (set-font-size buffer (if mode
                              (or (font-size-default mode) *default-font-size*)
                              *default-font-size*))))

(define-command-global toggle-terminal-keys (buffer)
  "Toggle whether keys go to the terminal or to Nyxt."
  (let ((mode (find-mode buffer 'terminal-mode)))
    (if mode
        (let ((to-terminal (not (keys-to-terminal-p mode))))
          (setf (keys-to-terminal-p mode) to-terminal
                (forward-input-events-p buffer) to-terminal)
          (when to-terminal
            (ffi-buffer-evaluate-javascript-async
             buffer
             (ps:ps (when (ps:@ window nyxt-vt)
                      (ps:chain (ps:chain document (get-element-by-id "screen"))
                                (focus))))))
          (echo (if to-terminal
                    "Keys go to the terminal."
                    "Keys go to Nyxt.")))
        (echo "This buffer has no terminal."))))

(-> write-ticks-seconds (terminal-mode) (values double-float &optional))
