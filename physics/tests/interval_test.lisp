; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass interval-test (test-case)
  ())

(def-test-method test-expand ((test interval-test))
  (let ((interval (interval 3 5)))
    (assert-equal 3 (lower interval))
    (assert-equal 5 (upper interval))
    (setf interval (expand interval '(8 -1)))
    (assert-equal -1 (lower interval))
    (assert-equal 8 (upper interval))))

(def-test-method test-overlaps ((test interval-test))
  (let ((interval-a (interval -100 100))
        (interval-b (interval 50 200))
        (interval-c (interval 30 50 :upper-inclusive nil))
        (interval-d (interval 100 520 :lower-inclusive nil))
        (interval-e (interval -10 50))
        (interval-f (interval 200 700)))
    (assert-true (overlaps interval-a interval-b))
    (assert-true (overlaps interval-b interval-a))
    (assert-false (overlaps interval-b interval-c))
    (assert-false (overlaps interval-a interval-d))
    (assert-true (overlaps interval-b interval-e))
    (assert-true (overlaps interval-b interval-f))))

(def-test-method test-within ((test interval-test))
  (let ((interval-a (interval 3 7))
        (interval-b (interval -100 100))
        (interval-c (interval 95 123))
        (interval-d (interval -123 -95))
        (interval-e (interval 7 123
                              :lower-inclusive nil
                              :upper-inclusive nil))
        (interval-f (interval -100 100
                              :lower-inclusive nil
                              :upper-inclusive nil)))
    (assert-true (within interval-a interval-b))
    (assert-true (within interval-b interval-b))
    (assert-false (within interval-b interval-a))
    (assert-false (within interval-c interval-b))
    (assert-false (within interval-d interval-b))
    (assert-false (within interval-a interval-e))
    (assert-true (within interval-e interval-e))
    (assert-true (within interval-f interval-b))))