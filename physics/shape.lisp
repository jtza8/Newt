; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass shape (presence)
  ((points :initform '()
           :initarg :points
           :reader points)))

(defmethod calculate-axes ((shape shape))
  (setf (slot-value shape 'axes)
        (loop
           with axes = '()
           for previous-point = (car (last (points shape))) then current-point
           and current-point in (points shape)
           for radius = (distance previous-point current-point)
           do (pushnew (uvector (/ (x-delta previous-point current-point)
                                   radius)
                                (/ (y-delta previous-point current-point)
                                   radius))
                       axes
                       :test #'uvector-equal)
           finally (return axes))))

(defmethod project-onto-axis ((shape shape) axis)
  (with-slots (x y points) shape
    (loop
       with interval = nil
       for point in points
       for projected = (+ (+ (* x (x axis)) (* y (y axis))); Offset
                          (* (x point) (x axis))
                          (* (y point) (y axis)))
       if (eq interval nil)
         do (setf interval (interval projected projected))
       else
         do (expand interval projected)
       finally (return interval))))

(defmethod collides-with ((a shape) (b shape))
  (let ((axes (nunion (axes a) (axes b) :test #'uvector-equal)))
    (dolist (axis axes t)
      (unless (overlaps (project-onto-axis a axis)
                        (project-onto-axis b axis))
        (return nil)))))