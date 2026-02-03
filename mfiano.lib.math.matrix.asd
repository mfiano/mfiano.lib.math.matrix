(asdf:defsystem #:mfiano.lib.math.matrix
  :description "A fast 2D/3D matrix math library"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mfiano.lib.math.matrix"
  :encoding :utf-8
  :depends-on
  (#:mfiano.lib.base
   #:mfiano.lib.math.scalar
   #:mfiano.lib.math.vector)
  :in-order-to ((test-op (test-op #:mfiano.lib.math.matrix/test)))
  :pathname "src"
  :serial t
  :components
  ((:file "mat2-f32")
   (:file "mat2-f64")
   (:file "mat3-f32")
   (:file "mat3-f64")))

(asdf:defsystem #:mfiano.lib.math.matrix/test
  :description "Unit tests for mfiano.lib.math.matrix"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mfiano.lib.math.matrix"
  :encoding :utf-8
  :depends-on
  (#:1am
   #:mfiano.lib.math.matrix)
  :perform (test-op (o c) (symbol-call '#:1am '#:run))
  :pathname "test"
  :serial t
  :components
  ((:file "test")))
