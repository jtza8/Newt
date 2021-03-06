; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass interval-test (test-case)
  ())

(def-test-method test-interval-equal ((test interval-test))
  (let ((a (interval -5 7))
        (b (interval 3 9)))
    (assert-true (interval-equal a a))
    (assert-true (interval-equal b b b))
    (assert-false (interval-equal a b))))

(def-test-method test-expand ((test interval-test))
  (let ((interval (interval 3 5)))
    (assert-equal 3 (left interval))
    (assert-equal 5 (right interval))
    (expand interval 8 -1)
    (assert-equal -1 (left interval))
    (assert-equal 8 (right interval))))

(def-test-method test-overlaps ((test interval-test))
  (let ((a (interval -100 100))
        (b (interval 50 200))
        (c (interval -10 50))
        (d (interval 200 700)))
    (assert-true (overlaps a b))
    (assert-true (overlaps b a))
    (assert-true (overlaps b c))
    (assert-true (overlaps b d))))

(def-test-method test-within ((test interval-test))
  (let ((a (interval 3 7))
        (b (interval -100 100))
        (c (interval 95 123))
        (d (interval -123 -95)))
    (assert-true (within a b))
    (assert-true (within b b))
    (assert-false (within b a))
    (assert-false (within c b))
    (assert-false (within d b))))

(def-test-method test-sort-interval-set ((test interval-test))
  (loop
     for a in (list (interval 1 35) (interval 12 23) (interval 34 43))
     and b in (sort-interval-set (list (interval 34 43)
                                       (interval 1 35)
                                       (interval 12 23)))
     doing (assert-true a b)))

(def-test-method test-fuse-interval-set ((test interval-test))
  (let ((set (list (interval -10 6)
                   (interval 3 8)
                   (interval 10 23)
                   (interval 9 50))))
    (assert-equal 0 (length (intersection (fuse-interval-set set)
                                          (list (interval -10 8)
                                                (interval 10 50))
                                          :test #'interval-equal)))))

(def-test-method test-interval-sets-overlap ((test interval-test))
  (let* ((a (list (interval 30 54)
                  (interval 3 4)))
         (b (list (interval 4 12)
                  (interval 20 31)))
         (c (append a (list (interval 60 72)))))
    (assert-true (interval-sets-overlap a b))
    (assert-false (interval-sets-overlap c b))))
