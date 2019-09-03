(in-package :next)

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

(defmethod +1-ring-index ((ring ring) index)
  "Add one to index, with wraparound."
  (let ((new-index (1+ index)))
    (if (eq new-index (length (items ring)))
        0 new-index)))

(defmethod -1-ring-index ((ring ring) index)
  "Minus one from index, with wraparound."
  (let ((new-index (1- index)))
    (if (eq new-index -1)
        (1- (length (items ring))) new-index)))

(defmethod ring-index ((ring ring) index)
  "Convert to internal ring index, where items are ordered from newest to oldest."
  (mod (1- (+ (head-index ring)
              (- (item-count ring) index)))
       (ring-size ring)))

(defmethod ring-insert ((ring ring) new-item)
  "Insert item into RING.
If RING is full, replace the oldest item."
  (prog1 (setf (aref (items ring)
               (mod (+ (head-index ring) (item-count ring))
                    (ring-size ring)))
               new-item)
    (if (= (item-count ring) (length (items ring)))
        (setf (head-index ring) (+1-ring-index ring (head-index ring)))
        (incf (item-count ring)))))

(defmethod ring-ref ((ring ring) index)
  "Return value from items by INDEX where 0 INDEX is most recent."
  (aref (items ring) (ring-index ring index)))

(defmethod ring-recent-list ((ring ring))
  "Return list of items ordered by most recent."
  (loop for index from 0 below (ring-size ring)
        for item = (ring-ref ring index)
        when item collect item))

(defmethod ring-insert-clipboard ((ring ring))
  "Check if clipboard-content is most recent entry in RING.
If not, insert clipboard-content into RING.
Return most recent entry in RING."
  (let ((clipboard-content (trivial-clipboard:text)))
    (unless (string= clipboard-content (ring-ref ring 0))
      (ring-insert ring clipboard-content)))
  (ring-ref ring 0))

(defun ring-completion-fn (ring)
  (let ((ring-items (ring-recent-list ring)))
    (lambda (input)
      (fuzzy-match input ring-items))))

(define-command paste-from-ring ()
  "Show `interface' clipboard ring and paste selected entry."
  (with-result (ring-item (read-from-minibuffer
                           (minibuffer *interface*)
                           :completion-function (ring-completion-fn
                                                 (clipboard-ring *interface*))))
    (%paste :input-text ring-item)))
