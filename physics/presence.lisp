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
   (points :initform '()
           :initarg :points
           :reader points)
   (axes :initform '()
         :reader axes)))

(defmethod initialize-instance :after ((presence presence) &key)
  (calculate-axes presence))

(defmethod calculate-axes ((presence presence))
  (setf (slot-value presence 'axes)
        (do* ((points (points presence) (cdr points))
              (previous-point (car (last points)) current-point)
              (current-point (car points) (car points))
              (axes '()))
             ((eql points '()) axes)
          (let* ((radius (distance previous-point current-point)))
            (pushnew (uvector (/ (x-delta previous-point current-point) radius)
                              (/ (y-delta previous-point current-point) radius))
                     axes
                     :test #'uvector-equal)))))

(defmethod project-onto-axis ((presence presence) axis)
  (with-slots (x y points) presence
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

(defmethod collides-with ((presence-one presence) (presence-two presence))
  (let ((axes (union (axes presence-one)
                     (axes presence-two)
                     :test #'uvector-equal)))
    (dolist (axis axes t)
      (destructuring-bind (min-one max-one)
          (project-onto-axis presence-one axis)
        (destructuring-bind (min-two max-two)
            (project-onto-axis presence-two axis)
          (when (or (> min-one max-two)
                    (> min-two max-one))
            (return nil)))))))