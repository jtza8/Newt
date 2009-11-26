; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass point ()
  ((x :initarg :x
      :reader x)
   (y :initarg :y
      :reader y)))

(defmethod x-delta ((point-a point) (point-b point))
  (- (x point-b) (x point-a)))

(defmethod y-delta ((point-a point) (point-b point))
  (- (y point-b) (y point-a)))

(defmethod distance ((point-a point) (point-b point))
  (let ((x-delta (- (x point-b) (x point-a)))
        (y-delta (- (y point-b) (y point-a))))
    (float (sqrt (+ (expt x-delta 2) (expt y-delta 2))) 0.0d0)))

(defmethod point (x y)
  (make-instance 'point :x x :y y))