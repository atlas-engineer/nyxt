(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass ring ()
  ((items :accessor items
          :initarg :items
          :initform (make-array 1000 :initial-element nil)
          :type array)
   (item-count :accessor item-count
               :initform 0)
   (head-index :accessor head-index
               :initform 0
               :documentation "Index of oldest item.")))

(defmethod ring-size ((ring ring))
  "Return the maximum number of elements it can contain."
  (length (items ring)))

(defmethod ring-index ((ring ring) index)
  "Return index converted to internal ring index, where items are ordered from newest to oldest."
  (mod (1- (+ (head-index ring)
              (- (item-count ring) index)))
       (ring-size ring)))

@export
(defmethod ring-insert ((ring ring) new-item)
  "Insert item into RING.
If RING is full, replace the oldest item.
Return NEW-ITEM."
  (prog1 (setf (aref (items ring)
                     (mod (+ (head-index ring) (item-count ring))
                          (ring-size ring)))
               new-item)
    (if (= (item-count ring) (length (items ring)))
        (setf (head-index ring) (mod (1+ (head-index ring)) (ring-size ring)))
        (incf (item-count ring)))))

(defmethod ring-ref ((ring ring) index)
  "Return value from items by INDEX where 0 INDEX is most recent."
  (aref (items ring) (ring-index ring index)))

(defmethod ring-recent-list ((ring ring))
  "Return list of items ordered by most recent."
  (loop for index from 0 below (ring-size ring)
        for item = (ring-ref ring index)
        when item collect item))

@export
(defmethod ring-insert-clipboard ((ring ring))
  "Check if clipboard-content is most recent entry in RING.
If not, insert clipboard-content into RING.
Return most recent entry in RING."
  (let ((clipboard-content (trivial-clipboard:text)))
    (unless (string= clipboard-content (ring-ref ring 0))
      (ring-insert ring clipboard-content)))
  (ring-ref ring 0))

@export
(defun ring-completion-fn (ring)
  (let ((ring-items (ring-recent-list ring)))
    (lambda (input)
      (fuzzy-match input ring-items))))
