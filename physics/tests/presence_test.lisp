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
    (assert-equal (length (calculate-axes octagon)) 4)
    (assert-equal (length (calculate-axes simple-poly)) 5)))

(def-test-method test-project-onto-axis ((test presence-test))
  (defun assert-min-max (presence axis expected)
    (let* ((min-max (project-onto-axis presence axis))
           (message (format nil "Expected: ~a Got: ~a" expected min-max)))
      (assert-true (almost-equal (car expected) (car min-max)) message)
      (assert-true (almost-equal (cadr expected) (cadr min-max)) message)))
  (with-slots (simple-poly octagon) test
    (assert-min-max octagon (uvector 0.0d0 1.0d0) '(0.0d0 90.0d0))
    (assert-min-max octagon (uvector 1.0d0 0.0d0) '(0.0d0 90.0d0))
    (assert-min-max simple-poly (uvector 1.0d0 0.0d0) '(0.0d0 100.0d0))
    (assert-min-max simple-poly (uvector 0.0d0 1.0d0) '(0.0d0 200.0d0))
    (assert-min-max octagon
                    (uvector 0.707106781186548d0 0.707106781186548d0)
                    '(21.213203435596426d0 106.066017177982136d0))
    (setf (x octagon) 25
          (y octagon) 37)
    (assert-min-max octagon (uvector 1.0d0 0.0d0) '(25.0d0 115.0d0))
    (assert-min-max octagon (uvector 0.0d0 1.0d0) '(37.0d0 127.0d0))))
