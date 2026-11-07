(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.math.matrix/test
  (:local-nicknames
   (#:m2 #:mfiano.lib.math.matrix.matrix2.f32)
   (#:m2d #:mfiano.lib.math.matrix.matrix2.f64)
   (#:m3 #:mfiano.lib.math.matrix.matrix3.f32)
   (#:m3d #:mfiano.lib.math.matrix.matrix3.f64)
   (#:s #:mfiano.lib.math.scalar.f32)
   (#:sd #:mfiano.lib.math.scalar.f64)
   (#:v2 #:mfiano.lib.math.vector.vector2.f32)
   (#:v2d #:mfiano.lib.math.vector.vector2.f64)
   (#:v3 #:mfiano.lib.math.vector.vector3.f32)
   (#:v3d #:mfiano.lib.math.vector.vector3.f64))
  (:use #:cl #:1am))

(in-package #:mfiano.lib.math.matrix/test)

(defun a2 ()
  (m2:new 1f0 2f0 3f0 4f0))

(defun a2d ()
  (m2d:new 1d0 2d0 3d0 4d0))

(defun b2 ()
  (m2:new 5f0 6f0 7f0 8f0))

(defun b2d ()
  (m2d:new 5d0 6d0 7d0 8d0))

(defun a3 ()
  (m3:new 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 9f0))

(defun a3d ()
  (m3d:new 1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0 9d0))

(defun b3 ()
  (m3:new 9f0 8f0 7f0 6f0 5f0 4f0 3f0 2f0 1f0))

(defun b3d ()
  (m3d:new 9d0 8d0 7d0 6d0 5d0 4d0 3d0 2d0 1d0))

(defun c3 ()
  (m3:new 2f0 1f0 1f0 0f0 3f0 1f0 1f0 2f0 2f0))

(defun c3d ()
  (m3d:new 2d0 1d0 1d0 0d0 3d0 1d0 1d0 2d0 2d0))
