; Copyright 2009 Jens Thiede.  All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :newt-physics)

(defclass presence ()
  ((x :initarg :x
      :initform 0
      :accessor x)
   (y :initarg :y
      :initform 0
      :accessor y)
   (axes :initform '()
         :reader axes)))

(defmethod initialize-instance :after ((presence presence) &key)
  (calculate-axes presence))

(defgeneric calculate-axes (presence))
(defgeneric project-onto-axis (presence axis))
(defgeneric collides-with (presence-a presence-b))