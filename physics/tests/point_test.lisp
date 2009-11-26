; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass point-test (test-case)
  ())

(def-test-method delta-test ((test point-test))
  (let ((point-a (point 3 7))
        (point-b (point 12 2)))
    (assert-equal 9 (x-delta point-a point-b))
    (assert-equal -5 (y-delta point-a point-b))))

(def-test-method distance-test ((test point-test))
  (assert-equal 5.0d0 (distance (point 8 5) (point 11 9))))