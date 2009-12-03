; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defparameter *equal-enough-accuracy* 8)

(defun equal-enough-upto (accuracy &rest floats)
  (loop
     for current in floats and next in (cdr floats)
     when (> (abs (- current next)) (expt 10 (- (1+ accuracy))))
       do (return nil)
     finally (return t)))

(defun equal-enough (&rest floats)
  (apply #'equal-enough-upto *equal-enough-accuracy* floats))

(defmacro equal-enough-or (more-or-less-than &rest floats)
  `(or ,(cons more-or-less-than floats)
       ,(cons 'equal-enough floats)))