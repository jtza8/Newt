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
