;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :user-interface)

;; Taken from serapeum
(defmacro export-always (symbols &optional (package nil package-supplied?))
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))

(defvar *id* 0 "Counter used to generate a unique ID.")

(defun unique-id ()
  (format nil "ui-element-~d" (incf *id*)))

(export-always 'to-html)
(defgeneric to-html (object)
  (:documentation "The HTML representation of OBJECT.
A form suitable to be compiled by Spinneret."))

(export-always 'id)
(export-always 'buffer)
(defclass ui-element ()
  ((id :accessor id :initarg :id
       :documentation "DOM id. Generated when omitted.")
   (buffer :accessor buffer :initarg :buffer
           :documentation "Buffer where element is drawn.")))

(defmethod initialize-instance :after ((element ui-element) &key id)
  (unless id
    (setf (id element) (unique-id))))

(export-always 'connect)
(defmethod connect ((element ui-element) buffer)
  (setf (buffer element) buffer))

(export-always 'disconnect)
(defgeneric disconnect (element)
  (:documentation "Detach ELEMENT and its descendants from their buffer."))

(defmethod disconnect ((element ui-element))
  (slot-makunbound element 'buffer)
  element)

(export-always 'update)
(defgeneric update (ui-element)
  (:documentation "Propagate changes to the buffer."))

(export-always '*defer-update*)
(defvar *defer-update* nil
  "When true, slot writers record an element instead of calling `update'.
`batch' binds this around a group of writes, then updates once.")

(defvar *pending-updates* '()
  "Elements recorded while `*defer-update*' is true.")

(defun queue-update (element)
  "Update ELEMENT, or record it while `*defer-update*' is true.
An element with no buffer is left alone."
  (cond
    (*defer-update*
     (pushnew element *pending-updates*))
    ((slot-boundp element 'buffer)
     (update element))))

(export-always 'batch)
(defmacro batch ((element) &body body)
  "Apply BODY's slot writes, then update each changed element once.
ELEMENT is updated as well when it is connected and BODY did not
already record it. Writes inside BODY do not update on their own."
  (let ((element-var (gensym "ELEMENT"))
        (pending-var (gensym "PENDING"))
        (item-var (gensym "ITEM")))
    `(let ((,element-var ,element))
       (if *defer-update*
           (multiple-value-prog1
               (progn ,@body)
             (when ,element-var
               (queue-update ,element-var)))
           (let ((*defer-update* t)
                 (*pending-updates* '()))
             (unwind-protect
                  (progn ,@body)
               (let ((,pending-var (nreverse *pending-updates*))
                     (*defer-update* nil)
                     (*pending-updates* nil))
                 (dolist (,item-var ,pending-var)
                   (when (slot-boundp ,item-var 'buffer)
                     (update ,item-var)))
                 (when (and ,element-var
                            (slot-boundp ,element-var 'buffer)
                            (not (member ,element-var ,pending-var)))
                   (update ,element-var)))))))))

(export-always 'display-text)
(defun display-text (value)
  "Return VALUE as text. Nil is the empty string."
  (cond
    ((stringp value) value)
    ((null value) "")
    (t (princ-to-string value))))

(defun write-escaped (value)
  "Write VALUE as escaped text."
  (spinneret:html (display-text value)))

(defun raw-markup (value)
  "Return VALUE when it is HTML markup, otherwise nil."
  (if (and (stringp value) (not (string= value "")))
      value
      nil))

(defun js-handler (value)
  "Return VALUE when it is a JavaScript statement, otherwise nil."
  (if (and (stringp value) (not (string= value "")))
      value
      nil))

(defun each-child (children function)
  "Call FUNCTION on each item of CHILDREN.
CHILDREN is a list or a vector. Nil does nothing."
  (etypecase children
    (null)
    (cons
     (dolist (child children)
       (funcall function child)))
    (vector
     (loop for child across children
           do (funcall function child)))))

(defun render-body (children raw text)
  "Write CHILDREN, otherwise RAW, otherwise TEXT.
CHILDREN is a list or a vector. A string child is escaped. A
`ui-element' child is drawn with `to-html'. RAW is unescaped HTML.
An empty collection uses RAW or TEXT."
  (let ((markup (raw-markup raw)))
    (cond
      ((and children (plusp (length children)))
       (each-child children
                   (lambda (child)
                     (etypecase child
                       (string (write-escaped child))
                       (ui-element (to-html child))))))
      (markup
       (spinneret:with-html
         (:raw markup)))
      (text
       (write-escaped text))))
  nil)

(export-always 'button)
(export-always 'text)
(export-always 'action)
(export-always 'html-class)
(export-always 'raw)
(export-always 'children)
(export-always 'attributes)
(defclass button (ui-element)
  ((text :initform "" :initarg :text :accessor text)
   (alt-text :initform "" :initarg :alt-text :accessor alt-text)
   (action :initform "" :initarg :action :accessor action)
   (html-class :initform "button" :initarg :html-class :accessor html-class
               :documentation "Class attribute. \"button\" is the default.
Nil omits it.")
   (raw :initform nil :initarg :raw :accessor raw
        :documentation "Unescaped HTML body, such as an icon.
Used when `children' is empty.")
   (children :initform '() :initarg :children :accessor children
             :documentation "Body, left to right.
A string is escaped text. A `ui-element' is nested markup.
An empty list uses `raw' or `text'.")
   (attributes :initform nil :initarg :attributes :accessor attributes
               :documentation "Extra attributes as a plist.
For example (:type \"button\")."))
  (:documentation "A clickable button.
`action' is the JavaScript run on click. `alt-text' is the tooltip.
The body is `children', then `raw', then `text'."))

(defmethod (setf text) :after (text (button button))
  (declare (ignorable text))
  (queue-update button))

(defmethod (setf action) :after (action (button button))
  (declare (ignorable action))
  (queue-update button))

(defmethod (setf alt-text) :after (text (button button))
  (declare (ignorable text))
  (queue-update button))

(defmethod (setf html-class) :after (html-class (button button))
  (declare (ignorable html-class))
  (queue-update button))

(defmethod (setf raw) :after (raw (button button))
  (declare (ignorable raw))
  (queue-update button))

(defmethod (setf children) :after (children (button button))
  (declare (ignorable children))
  (connect-children button)
  (queue-update button))

(defmethod (setf attributes) :after (attributes (button button))
  (declare (ignorable attributes))
  (queue-update button))

(defmethod to-html ((button button))
  (spinneret:with-html
    (:button :id (id button)
             :class (html-class button)
             :title (alt-text button)
             :onclick (js-handler (action button))
             :attrs (attributes button)
             (render-body (children button) (raw button) (text button)))))

(defun connect-children (element)
  "Connect ELEMENT's child elements to its buffer."
  (when (slot-boundp element 'buffer)
    (let ((buffer (buffer element)))
      (each-child (children element)
                  (lambda (child)
                    (when (typep child 'ui-element)
                      (connect child buffer)))))))

(defun disconnect-children (element)
  "Disconnect ELEMENT's child elements from its buffer."
  (each-child (children element)
              (lambda (child)
                (when (typep child 'ui-element)
                  (disconnect child)))))

(defmethod connect :after ((button button) buffer)
  (declare (ignorable buffer))
  (connect-children button))

(defmethod disconnect :after ((button button))
  (disconnect-children button))

(export-always 'element)
(export-always 'tag)
(export-always 'title)
(defclass element (ui-element)
  ((tag :initform :div :initarg :tag :accessor tag
        :documentation "HTML tag. A keyword such as :div or :span, or a tag name.")
   (html-class :initform nil :initarg :html-class :accessor html-class
               :documentation "Class attribute. Nil omits it.")
   (title :initform nil :initarg :title :accessor title
          :documentation "Tooltip. Nil omits it.")
   (action :initform "" :initarg :action :accessor action
           :documentation "JavaScript run on click. Empty omits onclick.")
   (text :initform nil :initarg :text :accessor text
         :documentation "Escaped text. Nil draws none of its own.")
   (raw :initform nil :initarg :raw :accessor raw
        :documentation "Unescaped HTML body.
Used when `children' is empty.")
   (children :initform '() :initarg :children :accessor children
             :documentation "Body, left to right.
A string is escaped text. A `ui-element' is nested markup.
An empty list uses `raw' or `text'.")
   (attributes :initform nil :initarg :attributes :accessor attributes
               :documentation "Extra attributes as a plist.
For example (:aria-hidden \"true\")."))
  (:documentation "One HTML element, such as a box or a label.
The body is `children', then `raw', then `text'."))

(defmethod (setf tag) :after (tag (element element))
  (declare (ignorable tag))
  (queue-update element))

(defmethod (setf html-class) :after (html-class (element element))
  (declare (ignorable html-class))
  (queue-update element))

(defmethod (setf title) :after (title (element element))
  (declare (ignorable title))
  (queue-update element))

(defmethod (setf action) :after (action (element element))
  (declare (ignorable action))
  (queue-update element))

(defmethod (setf text) :after (text (element element))
  (declare (ignorable text))
  (queue-update element))

(defmethod (setf raw) :after (raw (element element))
  (declare (ignorable raw))
  (queue-update element))

(defmethod (setf children) :after (children (element element))
  (declare (ignorable children))
  (connect-children element)
  (queue-update element))

(defmethod (setf attributes) :after (attributes (element element))
  (declare (ignorable attributes))
  (queue-update element))

(defmethod connect :after ((element element) buffer)
  (declare (ignorable buffer))
  (connect-children element))

(defmethod disconnect :after ((element element))
  (disconnect-children element))

(defun tag-name (tag)
  "Return TAG as a name Spinneret can draw."
  (etypecase tag
    (keyword tag)
    (string tag)))

(defmethod to-html ((element element))
  (spinneret:with-html
    (:tag
     :name (tag-name (tag element))
     :id (id element)
     :class (html-class element)
     :title (title element)
     :onclick (js-handler (action element))
     :attrs (attributes element)
     (render-body (children element) (raw element) (text element)))))

(defun coerce-items (items)
  "Return ITEMS as a vector.
A list is copied. A vector is returned as itself."
  (etypecase items
    ((and vector (not string)) items)
    (list (coerce items 'vector))
    (string (error "Items are a vector or a list, not the string ~s." items))))

(defun make-node (element item)
  "Return the body for ITEM, or nil when ITEM is not drawn.
`template' builds it. Without one, a string or a `ui-element' is used as itself."
  (let ((template (template element)))
    (cond
      (template (funcall template item))
      ((or (stringp item) (typep item 'ui-element)) item)
      (t nil))))

(defun item-stamp (element item)
  "Return the value that decides whether ITEM is drawn again.
`item-key' computes it. Without one, the item itself is used."
  (let ((key (item-key element)))
    (if key
        (funcall key item)
        item)))

(defun rendered-items (element)
  "Return ELEMENT's body in `items' order.
An item already drawn is reused when its stamp is `equal' to the stamp
it was built with. An item no longer in `items' is dropped. A new item,
or one whose stamp changed, is built."
  (let ((nodes (nodes element))
        (seen (make-hash-table :test 'eq))
        (body '()))
    (each-child (items element)
                (lambda (item)
                  (setf (gethash item seen) t)
                  (let* ((stamp (item-stamp element item))
                         (record (gethash item nodes))
                         (node (car record)))
                    (unless (and record (equal stamp (cdr record)))
                      (setf node (make-node element item)
                            (gethash item nodes) (cons node stamp)))
                    (when node
                      (push node body)))))
    (maphash (lambda (item record)
               (declare (ignore record))
               (unless (gethash item seen)
                 (remhash item nodes)))
             nodes)
    (nreverse body)))

(defun connect-items (element)
  "Build ELEMENT's items and connect them when it has a buffer."
  (let ((buffer (when (slot-boundp element 'buffer)
                  (buffer element))))
    (dolist (node (rendered-items element))
      (when (and buffer (typep node 'ui-element))
        (connect node buffer)))))

(export-always 'dynamic-element)
(export-always 'items)
(export-always 'item)
(export-always 'template)
(export-always 'item-key)
(defclass dynamic-element (element)
  ((items :initform (make-array 0 :adjustable t :fill-pointer t)
          :initarg :items
          :accessor items
          :documentation "The items, a vector, from index 0.
A list stored here is copied into a vector. Items are compared with `eq'.
One already drawn is kept when its `item-key' is `equal' to the last draw,
one that is gone is dropped, and a new one is built. Setting `items' or
`item' redraws a connected element. `aref' and `vector-push-extend' do not.")
   (template :initform nil
             :initarg :template
             :accessor template
             :documentation "Function of one item. Returns what to draw, or nil to skip the item.
Nil draws the item itself: a string is text and a `ui-element' is nested.")
   (item-key :initform nil
             :initarg :item-key
             :accessor item-key
             :documentation "Function of one item, or nil.
When the value it returns is `equal' to the value from the last draw, the
node is kept. A different value builds the node again. Nil keeps a node
for as long as the item is `eq'. Return a fresh value: a value that shares
structure later code mutates keeps the old node.")
   (nodes :initform (make-hash-table :test 'eq)
          :accessor nodes
          :documentation "Items drawn so far, compared with `eq'.
The value is the node and the stamp it was built with."))
  (:documentation "An `element' whose body follows an array of items.
Adding an item builds one node. Removing an item drops its node.
A changed `item-key' builds that item again. The other nodes stay.
`children' is not the body."))

(defmethod initialize-instance :after ((element dynamic-element) &key)
  (setf (slot-value element 'items) (coerce-items (items element))))

(defmethod (setf items) :around (items (element dynamic-element))
  (call-next-method (coerce-items items) element))

(defmethod (setf items) :after (items (element dynamic-element))
  (declare (ignorable items))
  (connect-items element)
  (queue-update element))

(defmethod (setf template) :after (template (element dynamic-element))
  (declare (ignorable template))
  (clrhash (nodes element))
  (connect-items element)
  (queue-update element))

(defmethod (setf item-key) :after (item-key (element dynamic-element))
  (declare (ignorable item-key))
  (clrhash (nodes element))
  (connect-items element)
  (queue-update element))

(defun clear-dynamic-nodes (element)
  "Disconnect and drop all cached item nodes of ELEMENT."
  (maphash (lambda (item record)
             (declare (ignore item))
             (when (typep (car record) 'ui-element)
               (disconnect (car record))))
           (nodes element))
  (clrhash (nodes element)))

(defmethod connect :before ((element dynamic-element) buffer)
  (declare (ignore buffer))
  (clear-dynamic-nodes element))

(defmethod connect :after ((element dynamic-element) buffer)
  (declare (ignorable buffer))
  (connect-items element))

(defmethod disconnect :after ((element dynamic-element))
  (clear-dynamic-nodes element))

(defun item (element index)
  "Return the item of ELEMENT at INDEX."
  (aref (items element) index))

(defun (setf item) (value element index)
  "Replace the item of ELEMENT at INDEX.
An item already drawn is kept. A connected element redraws."
  (setf (aref (items element) index) value)
  (connect-items element)
  (queue-update element)
  value)

(defmethod to-html ((element dynamic-element))
  (spinneret:with-html
    (:tag
     :name (tag-name (tag element))
     :id (id element)
     :class (html-class element)
     :title (title element)
     :onclick (js-handler (action element))
     :attrs (attributes element)
     (render-body (rendered-items element) (raw element) (text element)))))

(export-always 'paragraph)
(defclass paragraph (ui-element)
  ((text :initform "" :initarg :text :accessor text)))

(defmethod (setf text) :after (text (paragraph paragraph))
  (declare (ignorable text))
  (queue-update paragraph))

(defmethod to-html ((paragraph paragraph))
  (spinneret:with-html
      (:p :id (id paragraph) (text paragraph))))

(export-always 'progress-bar)
(export-always 'percentage)
(defclass progress-bar (ui-element)
  ((percentage :initform 0
               :initarg :percentage
               :accessor percentage
               :documentation "The percentage the progress bar is
filled up, use a number between 0 and 100.")))

(export-always 'valid-percentage)
(defun valid-percentage (progress-bar)
  "Return PROGRESS-BAR's clamped percentage in [0, 100], or 0."
  (let ((percentage (percentage progress-bar)))
    (if (and (realp percentage) (<= 0 percentage 100))
        percentage
        0)))

(defmethod to-html ((progress-bar progress-bar))
  (spinneret:with-html
    (:div :class "progress-bar-base"
          (:div :class "progress-bar-fill"
                :id (id progress-bar)
                :style (format nil "width: ~,1f%;"
                               (valid-percentage progress-bar))
                ;; empty string to force markup to make closing :div tag
                ""))))

(defmethod (setf percentage) :after (percentage (progress-bar progress-bar))
  (declare (ignorable percentage))
  (queue-update progress-bar))

(export-always 'column)
(export-always 'width)
(defclass column ()
  ((text :initform "" :initarg :text :accessor text
         :documentation "Heading shown for this column.")
   (width :initform nil :initarg :width :accessor width
          :documentation "Column width.
A real is a fraction of the table, so 0.4 is 40%. A string is a CSS
width such as \"28px\" or \"40%\". Nil leaves the width unset."))
  (:documentation "One column of a `table'."))

(defun column-style (column)
  "Return the CSS width declaration for COLUMN, or nil."
  (let ((width (width column)))
    (etypecase width
      (null nil)
      (string (unless (string= width "")
                (format nil "width: ~a" width)))
      (real (format nil "width: ~,2f%" (* 100 width))))))

(export-always 'table-cell)
(export-always 'title)
(defclass table-cell ()
  ((text :initform "" :initarg :text :accessor text
         :documentation "Visible text of the cell.")
   (title :initform nil :initarg :title :accessor title
          :documentation "Tooltip. Nil uses `text'.")
   (row :initform nil :accessor row
        :documentation "The `table-row' this cell belongs to."))
  (:documentation "One cell of a `table-row'."))

(defun cell-title (cell)
  "Return the tooltip CELL shows."
  (display-text (or (title cell) (text cell))))

(defun update-cell-row (cell)
  "Update the row that contains CELL."
  (when (row cell)
    (queue-update (row cell))))

(defmethod (setf text) :after (text (cell table-cell))
  (declare (ignorable text))
  (update-cell-row cell))

(defmethod (setf title) :after (title (cell table-cell))
  (declare (ignorable title))
  (update-cell-row cell))

(export-always 'table-row)
(export-always 'cells)
(export-always 'key)
(export-always 'selected-p)
(export-always 'marked-p)
(export-always 'control-action)
(export-always 'mark-action)
(defclass table-row (ui-element)
  ((key :initform nil :initarg :key :accessor key
        :documentation "Caller-defined identity. The table does not display it.")
   (cells :initform '() :initarg :cells :accessor cells
          :documentation "The `table-cell' objects in this row, left to right.")
   (selected-p :initform nil :initarg :selected-p :accessor selected-p
               :type boolean
               :documentation "Whether this row is the table selection.
The selected row's element id is \"selection\". Setting this clears
it on the other rows of the table.")
   (marked-p :initform nil :initarg :marked-p :accessor marked-p
             :type boolean
             :documentation "Whether this row shows the marked state.
A marked row has the class \"marked\". Its checkbox is checked when
the table shows marks.")
   (action :initform "" :initarg :action :accessor action
           :documentation "JavaScript run when a cell in this row is clicked.")
   (control-action :initform "" :initarg :control-action :accessor control-action
                   :documentation "JavaScript run when a cell is control-clicked.
A plain click runs `action'.")
   (mark-action :initform "" :initarg :mark-action :accessor mark-action
                :documentation "JavaScript run when the mark checkbox changes.")
   (table :initform nil :accessor table
          :documentation "The `table' this row belongs to."))
  (:documentation "One row of a `table'.
Every cell takes the row's click actions. A row shorter than the table's
columns is padded with empty cells."))

(defun row-onclick (row)
  "Return the JavaScript ROW's cells run when clicked, or nil."
  (let ((action (js-handler (action row)))
        (control (js-handler (control-action row))))
    (cond
      ((and action control)
       (format nil "if (event.ctrlKey) {~a} else {~a}" control action))
      (control
       (format nil "if (event.ctrlKey) {~a}" control))
      (action))))

(defun row-marks-p (row)
  "Return whether ROW shows a mark checkbox."
  (and (table row) (marks-p (table row))))

(defun pad-count (row)
  "Return how many empty cells ROW needs to fill its table."
  (let ((table (table row)))
    (if table
        (max 0 (- (length (columns table)) (length (cells row))))
        0)))

(defmethod initialize-instance :after ((row table-row) &key)
  (dolist (cell (cells row))
    (setf (row cell) row)))

(defmethod (setf cells) :after (cells (row table-row))
  (dolist (cell cells)
    (setf (row cell) row))
  (queue-update row))

(defmethod (setf selected-p) :after (selected-p (row table-row))
  (when (and selected-p (table row))
    (dolist (other (rows (table row)))
      (when (and (not (eq other row)) (selected-p other))
        (setf (slot-value other 'selected-p) nil)
        (queue-update other))))
  (queue-update row))

(defmethod (setf marked-p) :after (marked-p (row table-row))
  (declare (ignorable marked-p))
  (queue-update row))

(defmethod (setf action) :after (action (row table-row))
  (declare (ignorable action))
  (queue-update row))

(defmethod (setf control-action) :after (action (row table-row))
  (declare (ignorable action))
  (queue-update row))

(defmethod (setf mark-action) :after (action (row table-row))
  (declare (ignorable action))
  (queue-update row))

(export-always 'texts)
(defmethod texts ((row table-row))
  "Return the text of each cell in ROW."
  (mapcar #'text (cells row)))

(defmethod (setf texts) (texts (row table-row))
  "Replace ROW's cells with TEXTS.
A string is cell text. A `table-cell' is used as itself."
  (setf (cells row)
        (mapcar (lambda (text)
                  (if (typep text 'table-cell)
                      text
                      (make-instance 'table-cell :text (display-text text))))
                texts))
  texts)

(defmethod to-html ((row table-row))
  (let ((onclick (row-onclick row)))
    (spinneret:with-html
      (:tr :id (if (selected-p row) "selection" (id row))
           :data-id (id row)
           :class (when (marked-p row) "marked")
           (when (row-marks-p row)
             (:td :class "mark"
                  (:input :type "checkbox"
                          :checked (marked-p row)
                          :onchange (js-handler (mark-action row)))))
           (dolist (cell (cells row))
             (:td :title (cell-title cell)
                  :onclick onclick
                  (display-text (text cell))))
           (loop repeat (pad-count row)
                 do (:td :title ""
                         :onclick onclick
                         ""))))))

(export-always 'make-table-row)
(defun make-table-row (texts &key key selected-p marked-p
                               action control-action mark-action)
  "Return a row whose cells show TEXTS.
A string is the cell text. A `table-cell' is used as itself.
ACTION runs on a click, CONTROL-ACTION on a control-click, and
MARK-ACTION when the mark checkbox changes."
  (make-instance 'table-row
                 :key key
                 :selected-p selected-p
                 :marked-p marked-p
                 :action (or action "")
                 :control-action (or control-action "")
                 :mark-action (or mark-action "")
                 :cells (mapcar (lambda (text)
                                  (if (typep text 'table-cell)
                                      text
                                      (make-instance 'table-cell
                                                     :text (display-text text))))
                                texts)))

(defun ensure-single-selection (table)
  "Leave TABLE with at most one selected row, the first one."
  (let ((selected (remove-if-not #'selected-p (rows table))))
    (dolist (extra (rest selected))
      (setf (slot-value extra 'selected-p) nil))))

(export-always 'table)
(export-always 'columns)
(export-always 'rows)
(export-always 'marks-p)
(export-always 'header-p)
(export-always 'header-visible-p)
(export-always 'html-class)
(defclass table (ui-element)
  ((columns :initform '() :initarg :columns :accessor columns
            :documentation "The `column' objects, left to right.
The mark checkbox is not one of these columns.")
   (rows :initform '() :initarg :rows :accessor rows
         :documentation "The `table-row' objects, top to bottom.
Rows with fewer cells than `columns' are drawn with empty cells
at the end. Setting `rows' redraws a connected table.")
   (marks-p :initform nil :initarg :marks-p :accessor marks-p
            :type boolean
            :documentation "Whether every row has a mark checkbox.
The checkbox column is 28px wide and has the class \"mark\".")
   (header-p :initform t :initarg :header-p :accessor header-p
             :type boolean
             :documentation "Whether the table has a heading row.")
   (header-visible-p :initform t :initarg :header-visible-p
                     :accessor header-visible-p
                     :type boolean
                     :documentation "Whether the heading row is shown.
A hidden heading stays in the table, so the column widths stay put.")
   (html-class :initform nil :initarg :html-class :accessor html-class
               :documentation "Class attribute of the table element.
Nil omits it."))
  (:documentation "A table drawn with `to-html' and then updated in place.
The selected row uses the element id \"selection\". A marked row uses
the class \"marked\". Cell text is escaped. Clicking a cell runs the
row's `action', and a control-click runs its `control-action'."))

(defun header-display (table)
  "Return the CSS display value of TABLE's heading row."
  (if (header-visible-p table)
      "display:revert;"
      "display:none;"))

(defmethod initialize-instance :after ((table table) &key)
  (dolist (row (rows table))
    (setf (table row) table))
  (ensure-single-selection table))

(defmethod (setf rows) :before (new-rows (table table))
  (when (slot-boundp table 'rows)
    (dolist (row (rows table))
      (unless (member row new-rows)
        (setf (table row) nil)))))

(defmethod (setf rows) :after (rows (table table))
  (dolist (row rows)
    (setf (table row) table))
  (ensure-single-selection table)
  (when (slot-boundp table 'buffer)
    (dolist (row rows)
      (connect row (buffer table))))
  (queue-update table))

(defmethod (setf columns) :after (columns (table table))
  (declare (ignorable columns))
  (queue-update table))

(defmethod (setf marks-p) :after (marks-p (table table))
  (declare (ignorable marks-p))
  (queue-update table))

(defmethod (setf header-p) :after (header-p (table table))
  (declare (ignorable header-p))
  (queue-update table))

(defmethod (setf header-visible-p) :after (visible-p (table table))
  (declare (ignorable visible-p))
  (queue-update table))

(defmethod (setf html-class) :after (html-class (table table))
  (declare (ignorable html-class))
  (queue-update table))

(defmethod connect :after ((table table) buffer)
  (dolist (row (rows table))
    (setf (table row) table)
    (connect row buffer)))

(defmethod disconnect :after ((table table))
  (dolist (row (rows table))
    (disconnect row)))

(export-always 'table-inner-html)
(defun table-inner-html (table)
  "Return the HTML inside TABLE."
  (spinneret:with-html-string
    (:colgroup
     (when (marks-p table)
       (:col :class "mark" :style "width: 28px"))
     (dolist (column (columns table))
       (:col :style (column-style column))))
    (when (header-p table)
      (:tr :style (header-display table)
           (when (marks-p table)
             (:th :class "mark" " "))
           (dolist (column (columns table))
             (:th (display-text (text column))))))
    (dolist (row (rows table))
      (to-html row))))

(defmethod to-html ((table table))
  (spinneret:with-html
    (:table :id (id table)
            :class (html-class table)
            (:raw (table-inner-html table)))))
