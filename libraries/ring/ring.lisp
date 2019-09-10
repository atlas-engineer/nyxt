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

(defmethod ring-size ((ring ring))
  "Return the maximum number of elements it can contain."
  (length (items ring)))

(defmethod ring-index ((ring ring) index)
  "Return index converted to internal ring index, where items are ordered from newest to oldest."
  (mod (1- (+ (head-index ring)
              (- (item-count ring) index)))
       (ring-size ring)))

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
(defun ring-completion-fn (ring)
  (let ((ring-items (ring-recent-list ring)))
    (lambda (input)
      (fuzzy-match input ring-items))))

@export
(declaim (ftype (function (ring) ring) ring-copy))
(defun ring-copy (ring)
  "Return a copy of RING."
  (let ((copy (make-instance 'ring :items nil)))
    (setf
     (head-index copy) (head-index ring)
     (item-count copy) (item-count ring)
     (items copy) (copy-seq (items ring)))
    copy))

(defun make-ring ()
  (make-instance 'ring))
