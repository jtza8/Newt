; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass interval-test (test-case)
  ())

(def-test-method test-interval-equal ((test interval-test))
  (let ((a (interval -5 7))
        (b (interval 3 9))
        (c (interval -5 7 :left-closed nil))
        (d (interval -5 7 :right-closed nil)))
    (assert-true (interval-equal a a))
    (assert-false (interval-equal a b))
    (assert-false (interval-equal a c))
    (assert-false (interval-equal a d))))

(def-test-method test-interval-side-compare ((test interval-test))
  (let ((a (interval 2 4))
        (b (interval 3 5)))
    (assert-true (interval-side-compare left left < a a))
    (assert-false (interval-side-compare left left > a b))))

(def-test-method test-expand ((test interval-test))
  (let ((interval (interval 3 5)))
    (assert-equal 3 (left interval))
    (assert-equal 5 (right interval))
    (setf interval (expand interval '(8 -1)))
    (assert-equal -1 (left interval))
    (assert-equal 8 (right interval))))

(def-test-method test-overlaps ((test interval-test))
  (let ((a (interval -100 100))
        (b (interval 50 200))
        (c (interval 30 50 :right-closed nil))
        (d (interval 100 520 :left-closed nil))
        (e (interval -10 50))
        (f (interval 200 700)))
    (assert-true (overlaps a b))
    (assert-true (overlaps b a))
    (assert-false (overlaps b c))
    (assert-false (overlaps a d))
    (assert-true (overlaps b e))
    (assert-true (overlaps b f))))

(def-test-method test-within ((test interval-test))
  (let ((a (interval 3 7))
        (b (interval -100 100))
        (c (interval 95 123))
        (d (interval -123 -95))
        (e (interval 7 123
                              :left-closed nil
                              :right-closed nil))
        (f (interval -100 100
                              :left-closed nil
                              :right-closed nil)))
    (assert-true (within a b))
    (assert-true (within b b))
    (assert-false (within b a))
    (assert-false (within c b))
    (assert-false (within d b))
    (assert-false (within a e))
    (assert-true (within e e))
    (assert-true (within f b))))

(def-test-method test-print-object ((test interval-test))
  (assert-equal "(interval 0.001d0 0.5d0)"
                (print-object (interval 0.001d0 0.5d0) nil))
  (assert-equal "(interval 0.1d0 0.5d0 :left-closed NIL)"
                (print-object (interval 0.1d0 0.5d0 :left-closed nil) nil))
  (assert-equal "(interval 0.1d0 0.5d0 :right-closed NIL)"
                (print-object (interval 0.1d0 0.5d0 :right-closed nil) nil))
  (assert-equal "(interval 0.1d0 0.5d0 :left-closed NIL :right-closed NIL)"
                (print-object (interval 0.1d0 0.5d0
                                        :left-closed nil
                                        :right-closed nil)
                              nil)))

(def-test-method test-sort-interval-set ((test interval-test))
  (let* ((a (list (interval 34 43) (interval 1 35) (interval 12 23)))
         (b (list (interval 1 35) (interval 12 23) (interval 34 43)))
         (c (sort-interval-set a)))
    (do ((loop-b b (cdr loop-b))
         (loop-c c (cdr loop-c)))
        ((or (eq loop-b nil) (eq loop-c nil)))
      (assert-true (interval-equal (car loop-b) (car loop-c))))))

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
  (let ((a (list (interval 3 4)
                 (interval 30 54)))
        (b (list (interval 4 12)
                 (interval 20 31))))
    (assert-true (interval-sets-overlap a b))))
