(in-package :next)

(defclass ring ()
  ((items :accessor items
          :initarg :items
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
  "Convert to internal ring index, where items are ordered from newest to oldest."
  (mod (1- (+ (head-index ring) (- (item-count ring) index))) (ring-size ring)))

(defmethod ring-insert ((ring ring) new-item)
  "Insert item into RING.
If RING is full, delete oldest item."
  (setf (aref (items ring)
              (mod (+ (head-index ring) (item-count ring))
                   (ring-size ring))) new-item)
  (setf (item-count ring) (1+ (item-count ring))))

(defmethod ring-ref ((ring ring) index)
  "Return from RING using conversion to internal index."
  (aref (items ring) (ring-index ring index)))

(defmethod ring-recent-list ((ring ring))
  "Return list of items ordered by most recent."
  (loop for index from 0 to (1- (ring-size ring))
        for item = (ring-ref ring index)
        if (not (null item)) collect item))
