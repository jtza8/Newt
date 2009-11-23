; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass presence-test (test-case)
  (simple-poly
   octagon))

(defmethod set-up ((test presence-test))
  (with-slots (simple-poly octagon) test
    (setf simple-poly
          (make-instance 'presence :shape '(#(50 0) #(100 50)
                                            #(100 80) #(80 200)
                                            #(0 100)))
          octagon
          (make-instance 'presence :shape '(#(30 0) #(60 0)
                                            #(90 30) #(90 60)
                                            #(60 90) #(30 90)
                                            #(0 60) #(0 30))))))

(def-test-method test-calculate-axis ((test presence-test))
  (with-slots (simple-poly octagon) test
    (assert-equal (calculate-axes simple-poly) nil)))