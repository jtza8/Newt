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

; Need to convert to sets of intervals.
(defmethod project-onto-axis ((matter matter) axis)
  (with-slots (shapes x y) matter
    (let ((total-min nil)
          (total-max nil)
          (offset (+ (* x (x axis)) (* y (y axis)))))
      (dolist (shape shapes (list (+ total-min offset)
                                  (+ total-max offset)))
        (destructuring-bind (min max) (project-onto-axis shape axis)
          (when (or (eql total-max nil) (> max total-max))
            (setf total-max max))
          (when (or (eql total-min nil) (< min total-min))
            (setf total-min min)))))))

(defmethod calculate-displacement ((matter matter) time)
  (with-slots (displacement velocity acceleration mass) matter
    (setf displacement
          (- (* velocity time)
             (* 1/2 acceleration time time)))))