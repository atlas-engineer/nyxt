(in-package :ring)

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

(defmethod size ((ring ring))
  "Return the maximum number of elements it can contain."
  (length (items ring)))

(defmethod index ((ring ring) index)
  "Return index converted to internal ring index, where items are ordered from newest to oldest."
  (mod (1- (+ (head-index ring)
              (- (item-count ring) index)))
       (size ring)))

(defmethod insert ((ring ring) new-item)
  "Insert item into RING.
If RING is full, replace the oldest item.
Return NEW-ITEM."
  (prog1 (setf (aref (items ring)
                     (mod (+ (head-index ring) (item-count ring))
                          (size ring)))
               new-item)
    (if (= (item-count ring) (size ring))
        (setf (head-index ring) (mod (1+ (head-index ring)) (size ring)))
        (incf (item-count ring)))))

(defmethod ref ((ring ring) index)
  "Return value from items by INDEX where 0 INDEX is most recent."
  (aref (items ring) (index ring index)))

(defmethod recent-list ((ring ring))
  "Return list of items ordered by most recent."
  (loop for index from 0 below (size ring)
        for item = (ref ring index)
        when item collect item))

(declaim (ftype (function (ring) ring) copy))
(defun copy (ring)
  "Return a copy of RING."
  (let ((copy (make-instance 'ring :items nil)))
    (setf
     (head-index copy) (head-index ring)
     (item-count copy) (item-count ring)
     (items copy) (copy-seq (items ring)))
    copy))

(defun make (&key (size 1000))
  (make-instance 'ring :items (make-array size :initial-element nil)))

(defmethod pop-most-recent((ring ring))
  "Return the most-recently-added item in RING, and remove it from the RING."
    (delete-index ring 0))

(defmethod shift-left ((ring ring) start end)
  (loop for index from start below end
        do (setf (aref (items ring)
                       (mod index (size ring)))
                 (aref (items ring)
                       (mod (1+ index) (size ring)))))
  (setf (aref (items ring) (mod end (size ring))) nil))

(defmethod shift-right ((ring ring) start end)
  (loop for index from start above end
        do (setf (aref (items ring)
                       (mod index (size ring)))
                 (aref (items ring)
                       (mod (1- index) (size ring)))))
  (setf (aref (items ring) (mod end (size ring))) nil))

(defmethod delete-index ((ring ring) index)
  "Delete the item at external index and move items to fill the space."
  (let* ((internal-index (index ring index))
         (deleted-item (aref (items ring) (mod internal-index (size ring))))
         (head (head-index ring))
         (midpoint (mod (floor (+ head (item-count ring)) 2)
                        (size ring))))
    (if (< midpoint internal-index)
        (shift-left ring internal-index (+ head (item-count ring)))
        (progn
          (shift-right ring internal-index head)
          (setf (head-index ring) (mod (1+ (head-index ring)) (size ring)))))
    (decf (item-count ring))
    deleted-item))

(defmethod delete-match ((ring ring) match-test)
  (let ((match-index (position-if match-test (recent-list ring))))
    (when match-index (delete-index ring match-index))))
