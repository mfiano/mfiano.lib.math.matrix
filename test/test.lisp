(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.math.matrix/test
  (:local-nicknames
   (#:m2 #:mfiano.lib.math.matrix.matrix2.f32)
   (#:v2d #:mfiano.lib.math.matrix.matrix2.f64)
   (#:v3 #:mfiano.lib.math.matrix.matrix3.f32)
   (#:v3d #:mfiano.lib.math.matrix.matrix3.f64))
  (:use #:mfiano.lib.base.util #:1am))

(in-package #:mfiano.lib.math.matrix/test)
