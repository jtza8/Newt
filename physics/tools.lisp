; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defparameter *equal-enough-accuracy* 8)

(defun equal-enough-upto (accuracy &rest floats)
  (do ((floats floats (cdr floats)))
      ((= (length floats) 1) t)
    (let ((current (car floats))
          (next (cadr floats)))
      (when (> (abs (- current next)) (expt 10 (- (1+ accuracy))))
        (return nil)))))

(defun equal-enough (&rest floats)
  (apply #'equal-enough-upto *equal-enough-accuracy* floats))

(defmacro equal-enough-or (more-or-less-than &rest floats)
  `(or ,(cons more-or-less-than floats)
       ,(cons 'equal-enough floats)))