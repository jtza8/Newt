; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass unit-vector ()
  ((x :initarg :x
      :initform (error "unit-vector is immutable and needs x value")
      :reader x)
   (y :initarg :y
      :initform (error "unit-vector is immutable and needs y value")
      :reader y)))

(defmethod initialize-instance :after ((unit-vector unit-vector) &key)
  (with-slots (x y) unit-vector
    (cond ((< x 0) (setf x (- x) y (- y)))
          ((almost-equal x 0.0d0) (setf x 0.0d0 y 1.0d0))
          ((almost-equal y 0.0d0) (setf x 1.0d0 y 0.0d0)))))

(defun uvector (x y)
  (make-instance 'unit-vector :x x :y y))

(defun uvector-equal (&rest uvecs)
  (do ((uvecs uvecs (cdr uvecs)))
      ((eql (length uvecs) 1) t)
    (let ((current (car uvecs))
          (next (cadr uvecs)))
    (unless (and (almost-equal (x current) (x next))
                 (almost-equal (y current) (y next)))
      (return nil)))))