; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass interval ()
  ((left :initarg :left
          :initform (error "Left-side unspecified.")
          :reader left)
   (right :initarg :right
          :initform (error "Right-side unspecified.")
          :reader right)
   (left-closed :initarg :left-closed
                    :initform t
                    :reader left-closed)
   (right-closed :initarg :right-closed
                    :initform t
                    :reader right-closed)))

(defun interval (left right &key (left-closed t) (right-closed t))
  (when (> left right) (error "Left more than right."))
  (make-instance 'interval
                 :left left
                 :right right
                 :left-closed left-closed
                 :right-closed right-closed))

(defmacro interval-side-compare (side-a side-b more-or-less a b)
  (let ((side-a-closed (intern (format nil "~A-CLOSED" side-a)))
        (side-b-closed (intern (format nil "~A-CLOSED" side-b))))
    `(or (,more-or-less (,side-a ,a) (,side-b ,b))
         (and (or (,side-b-closed ,b)
                  (and (not (,side-b-closed ,b))
                       (not (,side-a-closed ,a))))
              (almost-equal (,side-a ,a) (,side-b ,b))))))

(defun interval-equal (&rest intervals)
  (do ((loop-intervals intervals (cdr loop-intervals)))
      ((= (length loop-intervals) 1) t)
    (let ((current (car loop-intervals))
          (next (cadr loop-intervals)))
      (unless (and (eq (left-closed current) (left-closed next))
                   (eq (right-closed current) (right-closed next))
                   (almost-equal (left current) (left next))
                   (almost-equal (right current) (right next)))
        (return nil)))))

(defmethod expand ((interval interval) numbers
                   &key (left-closed t) (right-closed t))
  (let ((left (left interval))
        (right (right interval)))
    (dolist (number numbers (interval left right
                                      :left-closed left-closed
                                      :right-closed right-closed))
      (cond ((< number left) (setf left number))
            ((> number right) (setf right number))))))

(defmethod overlaps ((a interval) (b interval))
  (and (interval-side-compare left right < a b)
       (interval-side-compare right left > a b)))

(defmethod within ((a interval) (b interval))
  (and (interval-side-compare left left > a b)
       (interval-side-compare right right < a b)))

(defmethod print-object (object stream)
  ())

; Destructive.
;(defun fuse-interval-set (set)
;  (let ((set (sort set (lambda (a b) 
;                         (interval-side-compare left left < a b)))))
;    (do ((loop-set set (cdr loop-set))
;         (interval (car loop-set))
;         (new-set '()))
;        ((= (length loop-set) 1))
;      (let ((new-interval (car loop-set)))
;        (cond ((overlaps interval new-interval)))))))
;               (setf interval (expand interval 
;                                      (left new-interval)
;                                      (right new-interval))))
;              (t
;               (push interval new-set)
;               (setf interval new-interval)))))))