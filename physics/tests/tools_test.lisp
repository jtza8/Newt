; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass tools-test (test-case)
  (accuracy))

(defmethod set-up ((test tools-test))
  (with-slots (accuracy) test
    (setf accuracy *equal-enough-accuracy*)))

(defmethod tear-down ((test tools-test))
  (with-slots (accuracy) test
    (setf *equal-enough-accuracy* accuracy)))

(def-test-method test-equal-enough-upto ((test tools-test))
  (assert-true (equal-enough-upto 10 0.01234567891011 0.01234567891012))
  (assert-false (equal-enough-upto 10 0.01234567891011 0.01234567841011)))

(def-test-method test-equal-enough ((test tools-test))
  (setf *equal-enough-accuracy* 2)
  (assert-true (equal-enough 0.123 0.124)))

(def-test-method test-equal-enough-or ((test tools-test))
  (setf *equal-enough-accuracy* 2)
  (assert-true (equal-enough-or < 0.1d0 1.0d0))
  (assert-true (equal-enough-or < 0.1d0 0.1d0))
  (assert-false (equal-enough-or < 1.0d0 0.1d0)))