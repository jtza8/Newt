; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass unit-vector-test (test-case)
  ())

(def-test-method test-standardisation ((test unit-vector-test))
  (assert-true (uvector-equal (uvector 0.23d0 0.14d0)
                              (uvector 0.23d0 0.14d0)))
  (assert-true (uvector-equal (uvector 1.0d0 0.0d0)
                              (uvector -1.0d0 0.0d0)))
  (assert-true (uvector-equal (uvector 0.0d0 1.0d0)
                              (uvector 0.0d0 -1.0d0)))
  (assert-true (uvector-equal (uvector 0.23d0 -0.14d0)
                              (uvector -0.23d0 0.14d0)))
  (assert-true (uvector-equal (uvector 0.23d0 -0.14d0)
                              (uvector 0.23d0 -0.14d0))))