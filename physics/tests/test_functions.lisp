; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defun assert-projection (projectable axis expected)
  (let* ((interval (project-onto-axis projectable axis))
         (message (format nil "Expected: ~a Got: ~a" expected interval)))
    (assert-true (interval-equal expected interval) message)))

(defun assert-set-projection (matter axis expected-list)
  (let* ((actual-list (sort-interval-set (project-onto-axis matter axis)))
         (expected-list (sort-interval-set expected-list))
         (message (format nil "Expected: ~a Got: ~a"
                          expected-list actual-list)))
    (assert-equal (length expected-list) (length actual-list) message)
    (loop
       for expected in expected-list and actual in actual-list
       do (assert-true (interval-equal expected actual) message))))
