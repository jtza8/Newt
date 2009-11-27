; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass matter ()
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
         :reader axes)
   (mass :initform 1
         :initarg :mass
         :accessor mass)
   (displacement :initform 0
                 :initarg :displacement
                 :accessor displacement)
   (velocity :initform 0
             :initarg :velocity
             :accessor velocity)
   (acceleration :initform 0
                 :initarg :acceleration
                 :accessor acceleration)))

(defmethod initialize-instance :after ((matter matter) &key)
  (calculate-axes matter))

(defmethod calculate-axes ((matter matter))
  (setf (slot-value matter 'axes)
        (do* ((points (points matter) (cdr points))
              (previous-point (car (last points)) current-point)
              (current-point (car points) (car points))
              (axes '()))
             ((eql points '()) axes)
          (let* ((radius (distance previous-point current-point)))
            (pushnew (uvector (/ (x-delta previous-point current-point) radius)
                              (/ (y-delta previous-point current-point) radius))
                     axes
                     :test #'uvector-equal)))))

(defmethod project-onto-axis ((matter matter) axis)
  (with-slots (x y points) matter
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

(defmethod collides-with ((matter-one matter) (matter-two matter))
  (let ((axes (union (axes matter-one)
                     (axes matter-two)
                     :test #'uvector-equal)))
    (dolist (axis axes t)
      (destructuring-bind (min-one max-one)
          (project-onto-axis matter-one axis)
        (destructuring-bind (min-two max-two)
            (project-onto-axis matter-two axis)
          (when (or (> min-one max-two)
                    (> min-two max-one))
            (return nil)))))))

(defmethod calculate-displacement ((matter matter) time)
  (with-slots (displacement velocity acceleration mass) matter
    (setf displacement
          (- (* velocity time)
             (* 1/2 acceleration time time)))))