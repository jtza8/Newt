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
   (shapes :initform '()
           :initarg :shapes
           :reader shapes)
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

(defmethod project-onto-axis ((matter matter) axis)
  (with-slots (shapes) matter
    (let ((total-min nil) (total-max nil))
      (dolist (shape shapes (list total-min total-max))
        (destructuring-bind (min max) (project-onto-axis shape axis)
          (when (or (eql total-max nil) (> max total-max))
            (setf total-max max))
          (when (or (eql total-min nil) (< min total-min))
            (setf total-min min)))))))

(defmethod collides-with ((matter-one matter) (matter-two matter))
  ())

(defmethod calculate-displacement ((matter matter) time)
  (with-slots (displacement velocity acceleration mass) matter
    (setf displacement
          (- (* velocity time)
             (* 1/2 acceleration time time)))))