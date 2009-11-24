; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass tools-test (test-case)
  (accuracy))

(defmethod set-up ((test tools-test))
  (with-slots (accuracy) test
    (setf accuracy *almost-equal-accuracy*)))

(defmethod tear-down ((test tools-test))
  (with-slots (accuracy) test
    (setf *almost-equal-accuracy* accuracy)))

(def-test-method test-equal-to-n ((test tools-test))
  (assert-true (equal-to-n 10 0.01234567891011 0.01234567891012))
  (assert-false (equal-to-n 10 0.01234567891011 0.01234567841011)))

(def-test-method test-almost-equal ((test tools-test))
  (setf *almost-equal-accuracy* 2)
  (assert-true (almost-equal 0.123 0.124)))