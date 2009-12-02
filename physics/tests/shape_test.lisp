; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass shape-test (test-case)
  (simple-poly
   octagon))

(defmethod set-up ((test shape-test))
  (with-slots (simple-poly octagon motigon) test
    (setf simple-poly
          (make-instance 'shape :points (list (point 50 0) (point 100 50)
                                              (point 100 80) (point 80 200)
                                              (point 0 100)))
          octagon
          (make-instance 'shape :points (list (point 30 0) (point 60 0)
                                              (point 90 30) (point 90 60)
                                              (point 60 90) (point 30 90)
                                              (point 0 60) (point 0 30))))))

(def-test-method test-calculate-axis ((test shape-test))
  (with-slots (simple-poly octagon) test
    (assert-equal (length (axes octagon)) 4)
    (assert-equal (length (axes simple-poly)) 5)))

(def-test-method test-project-onto-axis ((test shape-test))
  (with-slots (simple-poly octagon) test
    (assert-projection octagon (uvector 0.0d0 1.0d0) (interval 0.0d0 90.0d0))
    (assert-projection octagon (uvector 1.0d0 0.0d0) (interval 0.0d0 90.0d0))
    (assert-projection simple-poly (uvector 1.0d0 0.0d0) (interval 0.0d0 100.0d0))
    (assert-projection simple-poly (uvector 0.0d0 1.0d0) (interval 0.0d0 200.0d0))
    (assert-projection octagon
                    (uvector 0.707106781186548d0 0.707106781186548d0)
                    (interval 21.213203435596426d0 106.066017177982136d0))
    (setf (x octagon) 25
          (y octagon) 37)
    (assert-projection octagon (uvector 1.0d0 0.0d0) (interval 25.0d0 115.0d0))
    (assert-projection octagon (uvector 0.0d0 1.0d0) (interval 37.0d0 127.0d0))))

(def-test-method test-collides-with ((test shape-test))
  (with-slots (simple-poly octagon) test
    (assert-true (collides-with octagon simple-poly))
    (setf (x octagon) 100)
    (assert-true (collides-with octagon simple-poly))
    (setf (x octagon) 101)
    (assert-false (collides-with octagon simple-poly))
    (setf (x octagon) 199
          (y octagon) 215)
    (assert-false (collides-with octagon simple-poly))))
