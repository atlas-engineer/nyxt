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
