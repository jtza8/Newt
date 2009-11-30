; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass interval ()
  ((lower :initarg :lower
          :initform (error "No lower bound specified.")
          :reader lower)
   (upper :initarg :upper
          :initform (error "No upper bound specified.")
          :reader upper)
   (lower-inclusive :initarg :lower-inclusive
                    :initform t
                    :reader lower-inclusive)
   (upper-inclusive :initarg :upper-inclusive
                    :initform t
                    :reader upper-inclusive)))

(defun interval (lower upper &key (lower-inclusive t) (upper-inclusive t))
  (when (> lower upper) (error "lower bound more than upper bound"))
  (make-instance 'interval
                 :lower lower
                 :upper upper
                 :lower-inclusive lower-inclusive
                 :upper-inclusive upper-inclusive))

(defmethod expand ((interval interval) numbers
                   &key (lower-inclusive t) (upper-inclusive t))
  (let ((lower (lower interval))
        (upper (upper interval)))
    (dolist (number numbers (interval lower upper
                                      :lower-inclusive lower-inclusive
                                      :upper-inclusive upper-inclusive))
      (cond ((< number lower) (setf lower number))
            ((> number upper) (setf upper number))))))

(defmethod overlaps ((interval-a interval) (interval-b interval))
  (or (and (< (lower interval-a) (upper interval-b))
           (> (upper interval-a) (lower interval-b)))
      (and (and (lower-inclusive interval-a) (upper-inclusive interval-b))
           (= (lower interval-a) (upper interval-b)))
      (and (and (upper-inclusive interval-a) (lower-inclusive interval-b))
           (= (upper interval-a) (lower interval-b)))))

(defmethod within ((interval-a interval) (interval-b interval))
  (or (and (or (> (lower interval-a) (lower interval-b))
               (and (or (lower-inclusive interval-b)
                        (and (not (lower-inclusive interval-b))
                             (not (lower-inclusive interval-a))))
                    (= (lower interval-a) (lower interval-b))))
           (or (< (upper interval-a) (upper interval-b))
               (and (or (upper-inclusive interval-b)
                        (and (not (upper-inclusive interval-b))
                             (not (upper-inclusive interval-a))))
                    (= (upper interval-a) (upper interval-b)))))))