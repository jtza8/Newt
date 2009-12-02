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

(defmacro interval-side-compare (side-a side-b more-or-less a b)
  `(or (,more-or-less (,side-a ,a) (,side-b ,b))
       (almost-equal (,side-a ,a) (,side-b ,b))))

(defun interval-equal (&rest intervals)
  (do ((loop-intervals intervals (cdr loop-intervals)))
      ((= (length loop-intervals) 1) t)
    (let ((current (car loop-intervals))
          (next (cadr loop-intervals)))
      (unless (and (almost-equal (left current) (left next))
                   (almost-equal (right current) (right next)))
        (return nil)))))

(defmethod expand ((interval interval) &rest numbers)
  (with-slots (left right) interval
    (dolist (number numbers interval)
      (cond ((< number left) (setf left number))
            ((> number right) (setf right number))))))

(defmethod overlaps ((a interval) (b interval))
  (and (interval-side-compare left right < a b)
       (interval-side-compare right left > a b)))

(defmethod within ((a interval) (b interval))
  (and (interval-side-compare left left > a b)
       (interval-side-compare right right < a b)))

(defmethod print-object ((interval interval) stream)
  (with-slots (left right) interval
    (format stream "(interval ~a ~a)" left right)))

(defun sort-interval-set (set)
  (sort (copy-seq set)
        (lambda (a b)
          (interval-side-compare left left < a b))))

(defun fuse-interval-set (set)
  (let ((set (sort-interval-set set)))
    (do ((loop-set set (cdr loop-set))
         (interval (car set))
         (new-set '()))
        ((= (length loop-set) 1))
      (let ((new-interval (car loop-set)))
        (cond ((overlaps interval new-interval)
               (setf interval (expand interval
                                      (left new-interval)
                                      (right new-interval))))
              (t
               (push interval new-set)
               (setf interval new-interval)))))))

(defun interval-sets-overlap (a b)
  (dolist (element a t)
    (unless (find element b :test #'overlaps)
      (return nil))))