;;;; cocoa-utilities.lisp


#|
The MIT license.

Copyright (c) 2013 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

(in-package :interface)

(defmacro on-main-thread (&rest actions)
  `(ccl::call-in-event-process
    #'(lambda ()
	,@actions)))

(defun ns-to-lisp-string (ns-str)
  (if (and (not (eql (%null-ptr) ns-str)) (plusp (#/length ns-str)))
      (ccl::%get-utf-8-cstring (#/UTF8String ns-str))
      ""))

(defun lisp-to-ns-string (lisp-str)
  (ccl::%make-nsstring lisp-str))

(defun view-p (thing)
  (typep thing 'ns:ns-view))

(defun obj-if-not-null (ns-obj)
  (if (eql ns-obj (%null-ptr))
      nil
      ns-obj))

(defmethod superview ((v ns:ns-view))
  (obj-if-not-null (#/superview v)))

(defmethod common-superview (v1 v2)
  (cond ((and (view-p v1) (view-p v2))
	 (if (eql v1 v2)
	     (superview v1)
	     (obj-if-not-null (#/ancestorSharedWithView: v1 v2))))
	((view-p v1)
	 (superview v1))
	((view-p v2)
	 (superview v2))
	(t
	 nil)))

(defun relation-convert (rel-key)
  (case rel-key
    (:<= #$NSLayoutRelationLessThanOrEqual)
    (:= #$NSLayoutRelationEqual)
    (:>= #$NSLayoutRelationGreaterThanOrEqual)
    (t #$NSLayoutRelationEqual)))

(defun attribute-convert (att-key)
  (case att-key
    (:left #$NSLayoutAttributeLeft)
    (:right #$NSLayoutAttributeRight)
    (:top #$NSLayoutAttributeTop)
    (:bottom #$NSLayoutAttributeBottom)
    (:leading #$NSLayoutAttributeLeading)
    (:trailing #$NSLayoutAttributeTrailing)
    (:width #$NSLayoutAttributeWidth)
    (:height #$NSLayoutAttributeHeight)
    (:center-x #$NSLayoutAttributeCenterX)
    (:center-y #$NSLayoutAttributeCenterY)
    (:baseline #$NSLayoutAttributeBaseline)
    (:none #$NSLayoutAttributeNotAnAttribute)
    (t #$NSLayoutAttributeNotAnAttribute)))

(defun make-constraint (&key
			  (install-view nil install-view-provided)
			  (priority nil)
			  item1
			  (att1 :width)
			  (relation :=)
			  (item2 nil)
			  (att2 nil) ;; defaults to att1 if item2 is not null
			  (mult 1)
			  (const 0))
  (unless install-view-provided
    (setf install-view (common-superview item1 item2)))
  (when (and (view-p item1)
	     install-view
	     (not (eql item1 install-view))
	     (#/translatesAutoresizingMaskIntoConstraints item1))
    ;; make sure no automatic constraints are used for this view
    (#/setTranslatesAutoresizingMaskIntoConstraints: item1 #$NO))
  (when (and (view-p item2)
	     install-view
	     (not (eql item2 install-view))
	     (#/translatesAutoresizingMaskIntoConstraints item2))
    ;; make sure no automatic constraints are used for this view
    (#/setTranslatesAutoresizingMaskIntoConstraints: item2 #$NO))
  (let* ((rel (relation-convert relation))
	 (a1 (attribute-convert att1))
	 (a2 (cond (att2 (attribute-convert att2))
		   ((or (null item2) (eql item2 (%null-ptr))) (attribute-convert :none))
		   (t a1)))
	 (constraint (#/constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:
		      ns:ns-layout-constraint
		      item1
		      a1
		      rel
		      (or item2 (%null-ptr))
		      a2
		      (gui::cgfloat mult)
		      (gui::cgfloat const))))
    (when (numberp priority)
      (#/setPriority: constraint (float priority)))
    (when (view-p install-view)
      (on-main-thread
       (#/addConstraint: install-view constraint)))
    constraint))

(defmethod constrain-size-relative-to ((sized-view ns:ns-view) (relative-view ns:ns-view)
                                       &key
                                       (priority nil)
                                       (install-view (common-superview sized-view relative-view))
                                       (rel :=)
                                       (width t)
                                       (height t))
  ;; constrains sized-view relative to the size of relative-view
  ;; rel should be one of the keywords := :<= :>=
  (let ((constraints nil))
    (when width
      (push (make-constraint
	     :priority priority
	     :install-view install-view
	     :item1 sized-view
	     :att1 :width
	     :relation rel
	     :item2 relative-view
	     :att2 :width) 
            constraints))
    (when height
      (push (make-constraint
	     :priority priority
	     :install-view install-view
	     :item1 sized-view
	     :att1 :height
	     :relation rel
	     :item2 relative-view
	     :att2 :height)
            constraints))
    constraints))

(defclass controller (ns:ns-object)
  ((data :accessor data
         :initarg :data)
   (objects :accessor objects
            :initform nil)
   (col-ids :accessor col-ids
            :initarg :col-ids)
   (count-func :accessor count-func
               :initarg :count-func)
   (select-func :accessor select-func
                :initarg :select-func)
   (edited-func :accessor edited-func
                :initarg :edited-func)
   (added-func :accessor added-func
               :initarg :added-func)
   (removed-func :accessor removed-func
                 :initarg :removed-func)
   (add-child-func :accessor add-child-func
                   :initarg :add-child-func)
   (search-key :accessor search-key
               :initarg :search-key)
   (search-string :accessor search-string
                  :initform "")
   (search-test :accessor search-test
                :initarg :search-test)
   (search-results :accessor search-results
                   :initform nil)
   (prev-search-results :accessor prev-search-results
                        :initform nil)
   (level-info :accessor level-info)
   (max-level :accessor max-level
              :initform 0)
   (column-info :accessor column-info)
   (nib-initialized :accessor nib-initialized)
   (view-class :accessor view-class)
   (single-level :accessor single-level
                 :initform t)
   (bind-path :accessor bind-path
              :initform nil)
   (bind-obj :accessor bind-obj
             :initform nil)
   (reflect-to-bound-object :accessor reflect-to-bound-object
                            :initform t)
   (observer-obj :accessor observer-obj
                 :initform nil)
   (can-remove :foreign-type #>BOOL :accessor can-remove)
   (can-insert :foreign-type #>BOOL :accessor can-insert)
   (can-add-child :foreign-type #>BOOL :accessor can-add-child)
   (can-search-next :foreign-type #>BOOL :accessor can-search-next)
   (can-search-prev :foreign-type #>BOOL :accessor can-search-prev)
   (has-selection :foreign-type #>BOOL :accessor has-selection)
   (func-owner :accessor func-owner
               :initarg :func-owner)
   (view :foreign-type :id :accessor view))
  (:default-initargs
    :root nil
    :root-type nil
    :col-ids nil
    :count-func nil
    :select-func nil
    :edited-func nil
    :added-func nil
    :removed-func nil
    :add-child-func nil
    :func-owner nil
    :undo-doc nil
    :undo-name "table"
    :search-key #'identity
    :search-test #'string-equal)
  (:metaclass ns:+ns-object))

(objc:defmethod (#/numberOfRowsInTableView: #>NSInteger) 
                ((self controller) (tab :id))
  ;; Assumes that objects is some type of sequence
  ;; Subclass should override this method if that is not true.
  (declare (ignore tab))
  (with-slots (data) self
    (length data)))

(objc:defmethod (#/tableView:objectValueForTableColumn:row: :id) 
                ((self controller) 
                 (tab :id)
                 (col :id)
                 (row #>NSInteger))
  (declare (ignore tab))
  (lisp-to-ns-string (nth row (data self))))

(defmethod data-changed ((self controller))
  (#/reloadData (view self)))
