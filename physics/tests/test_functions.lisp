; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defun assert-min-max (projectable axis expected)
  (let* ((min-max (project-onto-axis projectable axis))
         (message (format nil "Expected: ~a Got: ~a" expected min-max)))
    (assert-true (interval-equal expected min-max) message)))
