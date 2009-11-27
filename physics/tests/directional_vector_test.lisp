; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass directional-vector-test (test-case)
  ())

(def-test-method test-magnitude ((test directional-vector-test))
  (let ((dvec (dvector (uvector 0.7d0 0.2d0) 10)))
    (assert-true (almost-equal (x-magnitude dvec) 7.0d0))
    (assert-true (almost-equal (y-magnitude dvec) 2.0d0))))

(def-test-method test-dvector-arithmetic ((test directional-vector-test))
  (let* ((dvec-a (dvector (uvector 1.0d0 0.0d0) 10))
         (dvec-b (dvector (uvector 0.0d0 1.0d0) 10))
         (dvec-c (dvector+ dvec-a dvec-b))
         (dvec-d (dvector- dvec-a dvec-b)))
    (assert-true (almost-equal (magnitude dvec-c) 14.14213562373095d0))
    (assert-true (uvector-equal (uvector 0.707106781186548d0
                                         0.707106781186548d0)
                                (direction dvec-c)))
    (assert-true (almost-equal (magnitude dvec-d) 14.14213562373095d0))
    (assert-true (uvector-equal (uvector 0.707106781186548d0
                                         -0.707106781186548d0)
                                (direction dvec-d)))))