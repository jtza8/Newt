; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass shape ()
  ((x :initarg :x
      :initform 0
      :accessor x)
   (y :initarg :y
      :initform 0
      :accessor y)
   (points :initform '()
           :initarg :points
           :reader points)
   (axes :initform '()
         :reader axes)))

(defmethod initialize-instance :after ((shape shape) &key)
  (calculate-axes shape))

(defmethod calculate-axes ((shape shape))
  (setf (slot-value shape 'axes)
        (do* ((points (points shape) (cdr points))
              (previous-point (car (last points)) current-point)
              (current-point (car points) (car points))
              (axes '()))
             ((eql points '()) axes)
          (let* ((radius (distance previous-point current-point)))
            (pushnew (uvector (/ (x-delta previous-point current-point) radius)
                              (/ (y-delta previous-point current-point) radius))
                     axes
                     :test #'uvector-equal)))))

(defmethod project-onto-axis ((shape shape) axis)
  (with-slots (x y points) shape
    (do ((points points (cdr points))
         (offset (+ (* x (x axis)) (* y (y axis))))
         (minimum nil)
         (maximum nil))
        ((eql points '()) (list (+ minimum offset)
                                (+ maximum offset)))
      (let* ((point (car points))
             (dot-product (+ (* (x point) (x axis))
                             (* (y point) (y axis)))))
        (cond ((eql minimum nil)
               (setf minimum dot-product
                     maximum dot-product))
              ((< dot-product minimum)
               (setf minimum dot-product))
              ((> dot-product maximum)
               (setf maximum dot-product)))))))

(defmethod collides-with ((shape-one shape) (shape-two shape))
  (let ((axes (union (axes shape-one)
                     (axes shape-two)
                     :test #'uvector-equal)))
    (dolist (axis axes t)
      (destructuring-bind (min-one max-one)
          (project-onto-axis shape-one axis)
        (destructuring-bind (min-two max-two)
            (project-onto-axis shape-two axis)
          (when (or (> min-one max-two)
                    (> min-two max-one))
            (return nil)))))))