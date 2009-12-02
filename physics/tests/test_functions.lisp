; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defun assert-projection (projectable axis expected)
  (let* ((interval (project-onto-axis projectable axis))
         (message (format nil "Expected: ~a Got: ~a" expected interval)))
    (assert-true (interval-equal expected interval) message)))

(defun assert-set-projection (matter axis expected)
  (let* ((actual (project-onto-axis matter axis))
         (message (format nil "Expected: ~a Got: ~a" expected actual)))
    (assert-equal (length expected) (length actual))
    (do ((loop-expected expected (cdr loop-expected))
         (loop-actual actual (cdr actual)))
        ((or (eq (car loop-expected) '())
             (eq (car loop-actual) '())))
      (assert-true (interval-equal (car loop-expected) (car loop-actual))
                   message))))
