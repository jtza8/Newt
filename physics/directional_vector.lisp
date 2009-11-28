; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass directional-vector ()
  ((direction :initform (error "Direction not specified.")
              :initarg :direction
              :reader direction)
   (magnitude :initform (error "Magnitude not specified.")
              :initarg :magnitude
              :reader magnitude)))

(defun dvector (direction magnitude)
  (make-instance 'directional-vector :direction direction :magnitude magnitude))

(defmethod x-magnitude ((dvec directional-vector))
  (with-slots (direction magnitude) dvec
    (* (x direction) magnitude)))

(defmethod y-magnitude ((dvec directional-vector))
  (with-slots (direction magnitude) dvec
    (* (y direction) magnitude)))

(defun dvector-equal (&rest dvecs)
  (do ((dvecs dvecs (cdr dvecs)))
      ((eql (length dvecs) 1) t)
    (let ((current (car dvecs))
          (next (cadr dvecs)))
    (unless (and (uvector-equal (direction current) (direction next))
                 (almost-equal (magnitude current) (magnitude next)))
      (return nil)))))


(defmethod dvector+ ((dvec-a directional-vector)
                     (dvec-b directional-vector))
  (let* ((x-sum (+ (x-magnitude dvec-a) (x-magnitude dvec-b)))
         (y-sum (+ (y-magnitude dvec-a) (y-magnitude dvec-b)))
         (magnitude (sqrt (+ (expt x-sum 2)
                             (expt y-sum 2)))))
    (dvector (uvector (/ x-sum magnitude)
                      (/ y-sum magnitude))
             magnitude)))

(defmethod dvector- ((dvec-a directional-vector)
                     (dvec-b directional-vector))
  (dvector+ dvec-a (dvector (direction dvec-b)
                            (- (magnitude dvec-b)))))