; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass matter (presence)
  ((shapes :initform '()
           :initarg :shapes
           :reader shapes)
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

(defmethod calculate-axes ((matter matter))
  (with-slots (axes shapes) matter
    (dolist (shape shapes axes)
      (setf axes (union axes (axes shape) :test #'uvector-equal)))))

(defmethod project-onto-axis ((matter matter) axis)
  (with-slots (shapes x y) matter
    (let ((interval-set '())
          (offset (+ (* x (x axis)) (* y (y axis)))))
      (dolist (shape shapes interval-set)
        (let ((projection (project-onto-axis shape axis)))
          (push (interval (+ offset (left projection))
                          (+ offset (right projection)))
                interval-set))))))

(defmethod collides-with ((a matter) (b matter))
  (dolist (axis (union (axes a) (axes b) :test #'uvector-equal) t)
    (let ((set-a (project-onto-axis a axis))
          (set-b (project-onto-axis b axis)))
      (unless (interval-sets-overlap set-a set-b)
        (return nil)))))

(defmethod calculate-displacement ((matter matter) time)
  (with-slots (displacement velocity acceleration mass) matter
    (setf displacement
          (- (* velocity time)
             (* 1/2 acceleration time time)))))