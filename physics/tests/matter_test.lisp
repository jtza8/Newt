; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass matter-test (test-case)
  (simple-matter
   compound-matter))

(defmethod set-up ((test matter-test))
  (let ((simple-poly (make-instance 'shape
                                    :points (list (point 50 0) (point 100 50)
                                                  (point 100 80) (point 80 200);
                                                  (point 0 100))))
        (octagon (make-instance 'shape
                                :x 15
                                :y 200
                                :points (list (point 30 0) (point 60 0)
                                              (point 90 30) (point 90 60)
                                              (point 60 90) (point 30 90)
                                              (point 0 60) (point 0 30)))))
    (with-slots (simple-matter compound-matter) test
      (setf simple-matter
            (make-instance 'matter :shapes (list simple-poly)))
      (setf compound-matter
            (make-instance 'matter :shapes (list simple-poly octagon))))))

(def-test-method test-calculate-axes ((test matter-test))
  (with-slots (simple-matter compound-matter) test
    (assert-equal 5 (length (axes simple-matter)))
    (assert-equal 7 (length (axes compound-matter)))))

;(def-test-method test-project-onto-axis ((test matter-test))
;  (with-slots (simple-matter compound-matter) test
;    (assert-set-projection simple-matter
;                    (uvector 1.0d0 0.0d0)
;                    (interval 0.0d0 100.0d0))
;    (assert-set-projection simple-matter
;                    (uvector 0.0d0 1.0d0)
;                    (interval 0.0d0 200.0d0))
;    (assert-set-projection compound-matter
;                    (uvector 1.0d0 0.0d0)
;                    (interval 0.0d0 105.0d0))
;    (assert-set-projection compound-matter
;                    (uvector 0.0d0 1.0d0)
;                    (interval 0.0d0 290.0d0))
;    (setf (x compound-matter) 25
;          (y compound-matter) 82)
;    (assert-set-projection compound-matter
;                    (uvector 1.0d0 0.0d0)
;                    (interval 25.0d0 130.0d0))
;    (assert-set-projection compound-matter
;                    (uvector 0.0d0 1.0d0)
;                    (interval 82.0d0 372.0d0))))

;(def-test-method test-collides-with ((test matter-test))
;  (with-slots (simple-matter compound-matter) test
;    (assert-true (collides-with simple-matter compound-matter))
;    (setf (x compound-matter) 25
;          (y compound-matter) 199)
;    (assert-true (collides-with simple-matter compound-matter))))