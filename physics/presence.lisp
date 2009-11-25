; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass presence ()
  ((x :initarg :x
      :initform 0
      :accessor x)
   (y :initarg :y
      :initform 0
      :accessor y)
   (shape :initform '()
          :initarg :shape
          :reader shape)
   (axes :initform '())))

(defmethod calculate-axes ((presence presence))
  (setf (slot-value presence 'axes)
        (do* ((shape (shape presence) (cdr shape))
              (previous-point (car (last shape)) current-point)
              (current-point (car shape) (car shape))
              (axes '()))
             ((eql shape '())
              (delete-duplicates axes :test #'uvector-equal))
          (let* ((delta (vector (- (elt previous-point 0)
                                   (elt current-point 0))
                                (- (elt previous-point 1)
                                   (elt current-point 1))))
                 (radius (sqrt (+ (expt (elt delta 0) 2)
                                  (expt (elt delta 1) 2)))))
            (push (uvector (/ (elt delta 0) radius)
                           (/ (elt delta 1) radius))
                  axes)))))

(defmethod project-onto-axis ((presence presence) axis)
  (with-slots (x y shape) presence
    (do ((shape shape (cdr shape))
         (offset (+ (* x (x axis)) (* y (y axis))))
         (minimum nil)
         (maximum nil))
        ((eql shape '()) (list (+ minimum offset)
                               (+ maximum offset)))
      (let* ((point (car shape))
             (dot-product (+ (* (elt point 0) (x axis))
                             (* (elt point 1) (y axis)))))
        (cond ((eql minimum nil)
               (setf minimum dot-product
                     maximum dot-product))
              ((< dot-product minimum)
               (setf minimum dot-product))
              ((> dot-product maximum)
               (setf maximum dot-product)))))))