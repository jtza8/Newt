; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass interval ()
  ((left :initarg :left
          :initform (error "Left-side unspecified.")
          :reader left)
   (right :initarg :right
          :initform (error "Right-side unspecified.")
          :reader right)))

(defun interval (left right)
  (when (> left right) (error "Left more than right."))
  (make-instance 'interval :left left :right right))

(defun interval-equal (&rest intervals)
  (loop
     for current in intervals and next in (cdr intervals)
     unless (and (equal-enough (left current) (left next))
                 (equal-enough (right current) (right next)))
     do (return nil)
     finally (return t)))

(defmethod expand ((interval interval) &rest numbers)
  (with-slots (left right) interval
    (dolist (number numbers interval)
      (cond ((< number left) (setf left number))
            ((> number right) (setf right number))))))

(defmethod overlaps ((a interval) (b interval))
  (and (equal-enough-or < (left a) (right b))
       (equal-enough-or > (right a) (left b))))

(defmethod within ((a interval) (b interval))
  (and (equal-enough-or > (left a) (left b))
       (equal-enough-or < (right a) (right b))))

(defmethod print-object ((interval interval) stream)
  (with-slots (left right) interval
    (format stream "(interval ~a ~a)" left right)))

(defun sort-interval-set (set)
  (sort (copy-seq set)
        (lambda (a b)
          (equal-enough-or < (left a) (left b)))))

(defun fuse-interval-set (set)
  (loop
     with sorted-set = (sort-interval-set set) and fused-set = '()
     with current-interval = (car sorted-set)
     for new-interval in (cdr sorted-set)
     if (overlaps current-interval new-interval)
       do (expand current-interval (left new-interval) (right new-interval))
     else
       do (progn (push current-interval fused-set)
                 (setf current-interval new-interval))))

(defun interval-sets-overlap (a b)
  (dolist (element a t)
    (unless (find element b :test #'overlaps)
      (return nil))))