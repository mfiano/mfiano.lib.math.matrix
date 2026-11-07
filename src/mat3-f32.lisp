(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.math.matrix.matrix3.f32
  (:local-nicknames
   (#:s #:mfiano.lib.math.scalar.f32)
   (#:v2 #:mfiano.lib.math.vector.vector2.f32)
   (#:v3 #:mfiano.lib.math.vector.vector3.f32))
  (:shadow #:= #:+ #:- #:* #:lerp #:values #:matrix)
  (:use #:mfiano.lib.base.util)
  ;; type
  (:export
   #:matrix)
  ;; constructor
  (:export
   #:new)
  ;; constants
  (:export
   #:+zero+
   #:+id+)
  ;; accessors
  (:export
   #:values
   #:with-values)
  ;; operators
  (:export
   #:=
   #:+
   #:+!
   #:-
   #:-!
   #:*
   #:*!
   #:*v3
   #:*v3!
   #:*v3-values
   #:copy
   #:copy!
   #:determinant
   #:get-angle
   #:get-axis
   #:get-axis!
   #:get-column-values
   #:get-column-vector
   #:get-column-vector!
   #:get-row-values
   #:get-row-vector
   #:get-row-vector!
   #:get-scale
   #:get-scale!
   #:get-scale-values
   #:get-translation
   #:get-translation!
   #:get-translation-values
   #:id
   #:id!
   #:id?
   #:invert
   #:invert!
   #:set-column-values
   #:set-column-values!
   #:set-column-vector
   #:set-column-vector!
   #:set-column-vectors
   #:set-column-vectors!
   #:set-diagonal-vector
   #:set-diagonal-vector!
   #:set-diagonal-values
   #:set-diagonal-values!
   #:set-row-values
   #:set-row-values!
   #:set-row-vector
   #:set-row-vector!
   #:set-row-vectors
   #:set-row-vectors!
   #:transpose
   #:transpose!
   #:trs!
   #:trs
   #:zero
   #:zero!
   #:zero?))

(in-package #:mfiano.lib.math.matrix.matrix3.f32)

(deftype matrix () '(f32-array 9))

;;; Operations

(define-constant +zero+ (make-f32-array 9) :test #'equalp)

(define-constant +id+ (to-f32-array #(1f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 1f0)) :test #'equalp)

(define-ftype new (f32 f32 f32 f32 f32 f32 f32 f32 f32) matrix)
(declaim (inline new))
(defun new (m00 m10 m20 m01 m11 m21 m02 m12 m22)
  (declare (optimize (speed 3)))
  (let ((m (make-f32-array 9)))
    (setf (aref m 0) m00 (aref m 1) m10 (aref m 2) m20
          (aref m 3) m01 (aref m 4) m11 (aref m 5) m21
          (aref m 6) m02 (aref m 7) m12 (aref m 8) m22)
    m))

(define-ftype values (matrix) (cl:values f32 f32 f32 f32 f32 f32 f32 f32 f32))
(declaim (inline values))
(defun values (m)
  (cl:values (aref m 0) (aref m 1) (aref m 2)
             (aref m 3) (aref m 4) (aref m 5)
             (aref m 6) (aref m 7) (aref m 8)))

(defmacro with-values (((prefix m) &rest rest) &body body)
  (once-only (m)
    `(symbol-macrolet
         ((,prefix ,m)
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "00")) (aref ,m 0))
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "10")) (aref ,m 1))
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "20")) (aref ,m 2))
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "01")) (aref ,m 3))
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "11")) (aref ,m 4))
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "21")) (aref ,m 5))
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "02")) (aref ,m 6))
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "12")) (aref ,m 7))
          (,(format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (list prefix "22")) (aref ,m 8)))
       ,(if rest
            `(with-values ,rest ,@body)
            `(progn ,@body)))))

(define-ftype = (matrix matrix &key (:rel-tol f32) (:abs-tol f32)) boolean)
(declaim (inline =))
(defun = (m1 m2 &key (rel-tol 1f-6) (abs-tol 1f-6))
  (declare (optimize (speed 3)))
  (with-values ((a m1) (b m2))
    (and (s:= a00 b00 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a10 b10 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a20 b20 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a01 b01 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a11 b11 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a21 b21 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a02 b02 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a12 b12 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a22 b22 :rel-tol rel-tol :abs-tol abs-tol))))

(define-ftype zero! (matrix) matrix)
(declaim (inline zero!))
(defun zero! (out)
  (declare (optimize (speed 3)))
  (fill out 0f0)
  out)

(define-ftype zero () matrix)
(declaim (inline zero))
(defun zero ()
  (declare (optimize (speed 3)))
  (new 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))

(define-ftype zero? (matrix) boolean)
(declaim (inline zero?))
(defun zero? (m)
  (declare (optimize (speed 3)))
  (= m +zero+))

(define-ftype id! (matrix) matrix)
(declaim (inline id!))
(defun id! (out)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (setf o00 1f0 o10 0f0 o20 0f0
          o01 0f0 o11 1f0 o21 0f0
          o02 0f0 o12 0f0 o22 1f0))
  out)

(define-ftype id () matrix)
(declaim (inline id))
(defun id ()
  (declare (optimize (speed 3)))
  (id! (zero)))

(define-ftype id? (matrix) boolean)
(declaim (inline id?))
(defun id? (m)
  (declare (optimize (speed 3)))
  (= m +id+))

(define-ftype copy! (matrix matrix) matrix)
(declaim (inline copy!))
(defun copy! (out m)
  (declare (optimize (speed 3)))
  (replace out m)
  out)

(define-ftype copy (matrix) matrix)
(declaim (inline copy))
(defun copy (m)
  (copy! (zero) m))

(define-ftype +! (matrix matrix matrix) matrix)
(declaim (inline +!))
(defun +! (out m1 m2)
  (declare (optimize (speed 3)))
  (with-values ((o out) (a m1) (b m2))
    (setf o00 (cl:+ a00 b00) o10 (cl:+ a10 b10) o20 (cl:+ a20 b20)
          o01 (cl:+ a01 b01) o11 (cl:+ a11 b11) o21 (cl:+ a21 b21)
          o02 (cl:+ a02 b02) o12 (cl:+ a12 b12) o22 (cl:+ a22 b22))
    out))

(define-ftype + (matrix matrix) matrix)
(declaim (inline +))
(defun + (m1 m2)
  (declare (optimize (speed 3)))
  (+! (zero) m1 m2))

(define-ftype -! (matrix matrix matrix) matrix)
(declaim (inline -!))
(defun -! (out m1 m2)
  (declare (optimize (speed 3)))
  (with-values ((o out) (a m1) (b m2))
    (setf o00 (cl:- a00 b00) o10 (cl:- a10 b10) o20 (cl:- a20 b20)
          o01 (cl:- a01 b01) o11 (cl:- a11 b11) o21 (cl:- a21 b21)
          o02 (cl:- a02 b02) o12 (cl:- a12 b12) o22 (cl:- a22 b22))
    out))

(define-ftype - (matrix matrix) matrix)
(declaim (inline -))
(defun - (m1 m2)
  (declare (optimize (speed 3)))
  (-! (zero) m1 m2))

(define-ftype *! (matrix matrix matrix) matrix)
(declaim (inline *!))
(defun *! (out m1 m2)
  (declare (optimize (speed 3)))
  (with-values ((o out) (a m1) (b m2))
    (psetf o00 (cl:+ (cl:* a00 b00) (cl:* a01 b10) (cl:* a02 b20))
           o10 (cl:+ (cl:* a10 b00) (cl:* a11 b10) (cl:* a12 b20))
           o20 (cl:+ (cl:* a20 b00) (cl:* a21 b10) (cl:* a22 b20))
           o01 (cl:+ (cl:* a00 b01) (cl:* a01 b11) (cl:* a02 b21))
           o11 (cl:+ (cl:* a10 b01) (cl:* a11 b11) (cl:* a12 b21))
           o21 (cl:+ (cl:* a20 b01) (cl:* a21 b11) (cl:* a22 b21))
           o02 (cl:+ (cl:* a00 b02) (cl:* a01 b12) (cl:* a02 b22))
           o12 (cl:+ (cl:* a10 b02) (cl:* a11 b12) (cl:* a12 b22))
           o22 (cl:+ (cl:* a20 b02) (cl:* a21 b12) (cl:* a22 b22)))
    out))

(define-ftype * (matrix matrix) matrix)
(declaim (inline *))
(defun * (m1 m2)
  (declare (optimize (speed 3)))
  (*! (zero) m1 m2))

(define-ftype *v3! (v3:vector matrix v3:vector) v3:vector)
(declaim (inline *v3!))
(defun *v3! (out m v)
  (declare (optimize (speed 3)))
  (v3:with-values ((v v) (o out))
    (with-values ((m m))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)))))
  out)

(define-ftype *v3 (matrix v3:vector) v3:vector)
(declaim (inline *v3))
(defun *v3 (m v)
  (declare (optimize (speed 3)))
  (*v3! (v3:zero) m v))

(define-ftype *v3-values (matrix f32 f32 f32) (cl:values f32 f32 f32))
(declaim (inline *v3-values))
(defun *v3-values (m x y z)
  (with-values ((m m))
    (cl:values (cl:+ (cl:* m00 x) (cl:* m01 y) (cl:* m02 z))
               (cl:+ (cl:* m10 x) (cl:* m11 y) (cl:* m12 z))
               (cl:+ (cl:* m20 x) (cl:* m21 y) (cl:* m22 z)))))

(define-ftype transpose! (matrix matrix) matrix)
(declaim (inline transpose!))
(defun transpose! (out m)
  (declare (optimize (speed 3)))
  (with-values ((o (copy! out m)))
    (rotatef o10 o01)
    (rotatef o20 o02)
    (rotatef o21 o12))
  out)

(define-ftype transpose (matrix) matrix)
(declaim (inline transpose))
(defun transpose (m)
  (declare (optimize (speed 3)))
  (transpose! (id) m))

(define-ftype get-column-vector! (v3:vector matrix (integer 0 2)) v3:vector)
(declaim (inline get-column-vector!))
(defun get-column-vector! (out m index)
  (declare (optimize (speed 3)))
  (with-values ((m m))
    (v3:with-values ((o out))
      (ecase index
        (0 (setf ox m00 oy m10 oz m20))
        (1 (setf ox m01 oy m11 oz m21))
        (2 (setf ox m02 oy m12 oz m22)))))
  out)

(define-ftype get-column-vector (matrix (integer 0 2)) v3:vector)
(declaim (inline get-column-vector))
(defun get-column-vector (m index)
  (declare (optimize (speed 3)))
  (get-column-vector! (v3:zero) m index))

(define-ftype get-column-values (matrix (integer 0 2)) (cl:values f32 f32))
(declaim (inline get-column-values))
(defun get-column-values (m index)
  (declare (optimize (speed 3)))
  (with-values ((m m))
    (ecase index
      (0 (cl:values m00 m10 m20))
      (1 (cl:values m01 m11 m21))
      (2 (cl:values m02 m12 m22)))))

(define-ftype get-row-vector! (v3:vector matrix (integer 0 2)) v3:vector)
(declaim (inline get-row-vector!))
(defun get-row-vector! (out m index)
  (declare (optimize (speed 3)))
  (with-values ((m m))
    (v3:with-values ((o out))
      (ecase index
        (0 (setf ox m00 oy m01 oz m02))
        (1 (setf ox m10 oy m11 oz m12))
        (2 (setf ox m20 oy m21 oz m22)))))
  out)

(define-ftype get-row-vector (matrix (integer 0 2)) v3:vector)
(declaim (inline get-row-vector))
(defun get-row-vector (m index)
  (declare (optimize (speed 3)))
  (get-row-vector! (v3:zero) m index))

(define-ftype get-row-values (matrix (integer 0 2)) (cl:values f32 f32))
(declaim (inline get-row-values))
(defun get-row-values (m index)
  (declare (optimize (speed 3)))
  (with-values ((m m))
    (ecase index
      (0 (cl:values m00 m01 m02))
      (1 (cl:values m10 m11 m12))
      (2 (cl:values m20 m21 m22)))))

(define-ftype set-column-vector! (matrix v3:vector (integer 0 2)) matrix)
(declaim (inline set-column-vector!))
(defun set-column-vector! (out v index)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (v3:with-values ((v v))
      (ecase index
        (0 (setf o00 vx o10 vy o20 vz))
        (1 (setf o01 vx o11 vy o21 vz))
        (2 (setf o02 vx o12 vy o22 vz)))))
  out)

(define-ftype set-column-vector (matrix v3:vector (integer 0 2)) matrix)
(declaim (inline set-column-vector))
(defun set-column-vector (m v index)
  (declare (optimize (speed 3)))
  (set-column-vector! (copy m) v index))

(define-ftype set-column-vectors! (matrix v3:vector v3:vector v3:vector) matrix)
(declaim (inline set-column-vectors!))
(defun set-column-vectors! (out v1 v2 v3)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (v3:with-values ((v1 v1) (v2 v2) (v3 v3))
      (setf o00 v1x o10 v1y o20 v1z
            o01 v2x o11 v2y o21 v2z
            o02 v3x o12 v3y o22 v3z)))
  out)

(define-ftype set-column-vectors (matrix v3:vector v3:vector v3:vector) matrix)
(declaim (inline set-column-vectors))
(defun set-column-vectors (m v1 v2 v3)
  (declare (optimize (speed 3)))
  (set-column-vectors! (copy m) v1 v2 v3))

(define-ftype set-column-values! (matrix f32 f32 f32 (integer 0 2)) matrix)
(declaim (inline set-column-values!))
(defun set-column-values! (out x y z index)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (ecase index
      (0 (setf o00 x o10 y o20 z))
      (1 (setf o01 x o11 y o21 z))
      (2 (setf o02 x o12 y o22 z))))
  out)

(define-ftype set-column-values (matrix f32 f32 f32 (integer 0 2)) matrix)
(declaim (inline set-column-values))
(defun set-column-values (m x y z index)
  (declare (optimize (speed 3)))
  (set-column-values! (copy m) x y z index))

(define-ftype set-row-vector! (matrix v3:vector (integer 0 2)) matrix)
(declaim (inline set-row-vector!))
(defun set-row-vector! (out v index)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (v3:with-values ((v v))
      (ecase index
        (0 (setf o00 vx o01 vy o02 vz))
        (1 (setf o10 vx o11 vy o12 vz))
        (2 (setf o20 vx o21 vy o22 vz)))))
  out)

(define-ftype set-row-vector (matrix v3:vector (integer 0 2)) matrix)
(declaim (inline set-row-vector))
(defun set-row-vector (m v index)
  (declare (optimize (speed 3)))
  (set-row-vector! (copy m) v index))

(define-ftype set-row-vectors! (matrix v3:vector v3:vector v3:vector) matrix)
(declaim (inline set-row-vectors!))
(defun set-row-vectors! (out v1 v2 v3)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (v3:with-values ((v1 v1) (v2 v2) (v3 v3))
      (setf o00 v1x o01 v1y o02 v1z
            o10 v2x o11 v2y o12 v2z
            o20 v3x o21 v3y o22 v3z)))
  out)

(define-ftype set-row-vectors (matrix v3:vector v3:vector v3:vector) matrix)
(declaim (inline set-row-vectors))
(defun set-row-vectors (m v1 v2 v3)
  (declare (optimize (speed 3)))
  (set-row-vectors! (copy m) v1 v2 v3))

(define-ftype set-row-values! (matrix f32 f32 f32 (integer 0 2)) matrix)
(declaim (inline set-row-values!))
(defun set-row-values! (out x y z index)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (ecase index
      (0 (setf o00 x o01 y o02 z))
      (1 (setf o10 x o11 y o12 z))
      (2 (setf o20 x o21 y o22 z))))
  out)

(define-ftype set-row-values (matrix f32 f32 f32 (integer 0 2)) matrix)
(declaim (inline set-row-values))
(defun set-row-values (m x y z index)
  (declare (optimize (speed 3)))
  (set-row-values! (copy m) x y z index))

(define-ftype set-diagonal-vector! (matrix matrix v3:vector) matrix)
(declaim (inline set-diagonal-vector!))
(defun set-diagonal-vector! (out m v)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (v3:with-values ((v v))
      (copy! out m)
      (setf o00 vx o11 vy o22 vz)))
  out)

(define-ftype set-diagonal-vector (matrix v3:vector) matrix)
(declaim (inline set-diagonal-vector))
(defun set-diagonal-vector (m v)
  (declare (optimize (speed 3)))
  (set-diagonal-vector! (copy m) m v))

(define-ftype set-diagonal-values! (matrix matrix f32 f32 f32) matrix)
(declaim (inline set-diagonal-values!))
(defun set-diagonal-values! (out m x y z)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (copy! out m)
    (setf o00 x o11 y o22 z))
  out)

(define-ftype set-diagonal-values (matrix f32 f32 f32) matrix)
(declaim (inline set-diagonal-values))
(defun set-diagonal-values (m x y z)
  (declare (optimize (speed 3)))
  (set-diagonal-values! (copy m) m x y z))

(define-ftype determinant (matrix) f32)
(declaim (inline determinant))
(defun determinant (m)
  (declare (optimize (speed 3)))
  (with-values ((m m))
    (cl:- (cl:+ (cl:* m00 m11 m22) (cl:* m10 m21 m02) (cl:* m20 m01 m12))
          (cl:* m00 m21 m12)
          (cl:* m20 m11 m02)
          (cl:* m10 m01 m22))))

(define-ftype invert! (matrix matrix) matrix)
(defun invert! (out m)
  (declare (optimize (speed 3)))
  (with-values ((o out) (m m))
    (when (id? m)
      (id! out)
      (return-from invert! out))
    (let ((det (determinant m)))
      (when (s:zero? det)
        (error "Cannot invert matrix with a determinant of zero:~%~s" m))
      (psetf o00 (/ (cl:- (cl:* m11 m22) (cl:* m12 m21)) det)
             o01 (/ (cl:- (cl:* m02 m21) (cl:* m01 m22)) det)
             o02 (/ (cl:- (cl:* m01 m12) (cl:* m02 m11)) det)
             o10 (/ (cl:- (cl:* m12 m20) (cl:* m10 m22)) det)
             o11 (/ (cl:- (cl:* m00 m22) (cl:* m02 m20)) det)
             o12 (/ (cl:- (cl:* m02 m10) (cl:* m00 m12)) det)
             o20 (/ (cl:- (cl:* m10 m21) (cl:* m11 m20)) det)
             o21 (/ (cl:- (cl:* m01 m20) (cl:* m00 m21)) det)
             o22 (/ (cl:- (cl:* m00 m11) (cl:* m01 m10)) det))
      out)))

(define-ftype invert (matrix) matrix)
(declaim (inline invert))
(defun invert (m)
  (declare (optimize (speed 3)))
  (invert! (id) m))

(define-ftype trs! (matrix f32 f32 f32 f32 f32) matrix)
(declaim (inline trs!))
(defun trs! (out tx ty angle sx sy)
  (declare (optimize (speed 3)))
  (multiple-value-bind (sin cos) (s:sincos angle)
    (with-values ((o out))
      (setf o00 (cl:* cos sx) o01 (cl:* sin sx) o02 tx
            o10 (cl:- (cl:* sin sy)) o11 (cl:* cos sy) o12 ty
            o20 0f0 o21 0f0 o22 1f0)
      out)))

(define-ftype trs (f32 f32 f32 f32 f32) matrix)
(declaim (inline trs))
(defun trs (tx ty angle sx sy)
  (declare (optimize (speed 3)))
  (trs! (id) tx ty angle sx sy))

(define-ftype get-axis! (v2:vector matrix (member :x :y)) v2:vector)
(declaim (inline get-axis!))
(defun get-axis! (out m axis)
  (declare (optimize (speed 3)))
  (v2:with-values ((o out))
    (with-values ((m m))
      (ecase axis
        (:x (setf ox m00 oy m10))
        (:y (setf ox m01 oy m11)))
      out)))

(define-ftype get-axis (matrix (member :x :y)) v2:vector)
(declaim (inline get-axis))
(defun get-axis (m axis)
  (declare (optimize (speed 3)))
  (get-axis! (v2:zero) m axis))

(define-ftype get-scale! (v2:vector matrix) v2:vector)
(declaim (inline get-scale!))
(defun get-scale! (out m)
  (declare (optimize (speed 3)))
  (v2:with-values ((o out))
    (let ((x (get-axis m :x))
          (y (get-axis m :y)))
      (declare (dynamic-extent x y))
      (setf ox (v2:magnitude x)
            oy (v2:magnitude y))
      out)))

(define-ftype get-scale (matrix) v2:vector)
(declaim (inline get-scale))
(defun get-scale (m)
  (declare (optimize (speed 3)))
  (get-scale! (v2:zero) m))

(define-ftype get-scale-values (matrix) (cl:values f32 f32))
(declaim (inline get-scale-values))
(defun get-scale-values (m)
  (let ((x (get-axis m :x))
        (y (get-axis m :y)))
    (declare (dynamic-extent x y))
    (cl:values (v2:magnitude x)
               (v2:magnitude y))))

(define-ftype get-translation! (v2:vector matrix) v2:vector)
(declaim (inline get-translation!))
(defun get-translation! (out m)
  (declare (optimize (speed 3)))
  (v2:with-values ((o out))
    (with-values ((m m))
      (setf ox m02 oy m12)
      out)))

(define-ftype get-translation (matrix) v2:vector)
(declaim (inline get-translation))
(defun get-translation (m)
  (declare (optimize (speed 3)))
  (get-translation! (v2:zero) m))

(define-ftype get-translation-values (matrix) (cl:values f32 f32))
(declaim (inline get-translation-values))
(defun get-translation-values (m)
  (with-values ((m m))
    (cl:values m02 m12)))

(define-ftype get-angle (matrix) f32)
(declaim (inline get-angle))
(defun get-angle (m)
  (declare (optimize (speed 3)))
  (with-values ((m m))
    (cl:- (atan m10 m00))))
