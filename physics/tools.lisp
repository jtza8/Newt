; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defparameter *almost-equal-accuracy* 10)

(defun equal-to-n (accuracy &rest floats)
  (do ((floats floats (cdr floats)))
      ((= (length floats) 1) t)
    (let ((current (car floats))
          (next (cadr floats)))
      (when (> (abs (- current next)) (expt 10 (- accuracy)))
        (return nil)))))

(defun almost-equal (&rest floats)
  (apply #'equal-to-n (cons *almost-equal-accuracy* floats)))