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
        (do* ((loop-points (points shape) (cdr loop-points))
              (previous-point (car (last loop-points)) current-point)
              (current-point (car loop-points) (car loop-points))
              (axes '()))
             ((eql loop-points '()) axes)
          (let* ((radius (distance previous-point current-point)))
            (pushnew (uvector (/ (x-delta previous-point current-point) radius)
                              (/ (y-delta previous-point current-point) radius))
                     axes
                     :test #'uvector-equal)))))

(defmethod project-onto-axis ((shape shape) axis)
  (with-slots (x y points) shape
    (do ((loop-points points (cdr loop-points))
         (interval nil))
        ((eql loop-points '()) interval)
      (let* ((point (car loop-points))
             (projected (+ (+ (* x (x axis)) (* y (y axis))); Offset
                           (* (x point) (x axis))
                           (* (y point) (y axis)))))
        (if (eq interval nil)
            (setf interval (interval projected projected))
            (expand interval projected))))))

(defmethod collides-with ((a shape) (b shape))
  (let ((axes (nunion (axes a) (axes b) :test #'uvector-equal)))
    (dolist (axis axes t)
      (unless (overlaps (project-onto-axis a axis)
                        (project-onto-axis b axis))
        (return nil)))))