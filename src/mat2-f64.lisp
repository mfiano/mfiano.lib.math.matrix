(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.math.matrix.matrix2.f64
  (:local-nicknames
   (#:s #:mfiano.lib.math.scalar.f64)
   (#:v2 #:mfiano.lib.math.vector.vector2.f64))
  (:shadow #:= #:+ #:- #:* #:/ #:lerp #:values #:matrix)
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
   #:*v2
   #:*v2!
   #:copy
   #:copy!
   #:get-column-values
   #:get-column-vector
   #:get-column-vector!
   #:get-row-values
   #:get-row-vector
   #:get-row-vector!
   #:id
   #:id!
   #:id?
   #:set-column-values
   #:set-column-values!
   #:set-column-vector
   #:set-column-vector!
   #:set-row-values
   #:set-row-values!
   #:set-row-vector
   #:set-row-vector!
   #:transpose
   #:transpose!
   #:zero
   #:zero!
   #:zero?))

(in-package #:mfiano.lib.math.matrix.matrix2.f64)

(deftype matrix () '(f64-array 4))

;;; Operations

(define-constant +zero+ (make-f64-array 4) :test #'equalp)

(define-constant +id+ (to-f64-array #(1d0 0d0 0d0 1d0)) :test #'equalp)

(define-ftype new (f64 f64 f64 f64) matrix)
(declaim (inline new))
(defun new (m00 m10 m01 m11)
  (declare (optimize (speed 3)))
  (let ((m (make-f64-array 4)))
    (setf (aref m 0) m00 (aref m 1) m10 (aref m 2) m01 (aref m 3) m11)
    m))

(define-ftype values (matrix) (cl:values f64 f64 f64 f64))
(declaim (inline values))
(defun values (m)
  (cl:values (aref m 0) (aref m 1)
             (aref m 2) (aref m 3)))

(defmacro with-values (((prefix m) &rest rest) &body body)
  (once-only (m)
    `(symbol-macrolet
         ((,prefix ,m)
          (,(format-symbol #.*package* "~@:(~{~a~}~)" (list prefix "00")) (aref ,m 0))
          (,(format-symbol #.*package* "~@:(~{~a~}~)" (list prefix "10")) (aref ,m 1))
          (,(format-symbol #.*package* "~@:(~{~a~}~)" (list prefix "01")) (aref ,m 2))
          (,(format-symbol #.*package* "~@:(~{~a~}~)" (list prefix "11")) (aref ,m 3)))
       ,(if rest
            `(with-values ,rest ,@body)
            `(progn ,@body)))))

(define-ftype = (matrix matrix &key (:rel-tol f64) (:abs-tol f64)) boolean)
(declaim (inline =))
(defun = (m1 m2 &key (rel-tol 1d-6) (abs-tol 1d-6))
  (declare (optimize (speed 3)))
  (with-values ((a m1) (b m2))
    (and (s:= a00 b00 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a10 b10 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a01 b01 :rel-tol rel-tol :abs-tol abs-tol)
         (s:= a11 b11 :rel-tol rel-tol :abs-tol abs-tol))))

(define-ftype zero! (matrix) matrix)
(declaim (inline zero!))
(defun zero! (out)
  (declare (optimize (speed 3)))
  (fill out 0d0)
  out)

(define-ftype zero () matrix)
(declaim (inline zero))
(defun zero ()
  (declare (optimize (speed 3)))
  (new 0d0 0d0 0d0 0d0))

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
    (setf o00 1d0 o10 0d0 o01 0d0 o11 1d0))
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
    (setf o00 (cl:+ a00 b00)
          o10 (cl:+ a10 b10)
          o01 (cl:+ a01 b01)
          o11 (cl:+ a11 b11))
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
    (setf o00 (cl:- a00 b00)
          o10 (cl:- a10 b10)
          o01 (cl:- a01 b01)
          o11 (cl:- a11 b11))
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
    (psetf o00 (cl:+ (cl:* a00 b00) (cl:* a01 b10))
           o10 (cl:+ (cl:* a10 b00) (cl:* a11 b10))
           o01 (cl:+ (cl:* a00 b01) (cl:* a01 b11))
           o11 (cl:+ (cl:* a10 b01) (cl:* a11 b11)))
    out))

(define-ftype * (matrix matrix) matrix)
(declaim (inline *))
(defun * (m1 m2)
  (declare (optimize (speed 3)))
  (*! (zero) m1 m2))

(define-ftype *v2! (v2:vector matrix v2:vector) v2:vector)
(declaim (inline *v2!))
(defun *v2! (out m v)
  (declare (optimize (speed 3)))
  (v2:with-values ((v v) (o out))
    (with-values ((m m))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy)))))
  out)

(define-ftype *v2 (matrix v2:vector) v2:vector)
(declaim (inline *v2))
(defun *v2 (m v)
  (declare (optimize (speed 3)))
  (*v2! (v2:zero) m v))

(define-ftype transpose! (matrix matrix) matrix)
(declaim (inline transpose!))
(defun transpose! (out m)
  (declare (optimize (speed 3)))
  (with-values ((o (copy! out m)))
    (rotatef o10 o01))
  out)

(define-ftype transpose (matrix) matrix)
(declaim (inline transpose))
(defun transpose (m)
  (declare (optimize (speed 3)))
  (transpose! (id) m))

(define-ftype get-column-vector! (v2:vector matrix (integer 0 1)) v2:vector)
(declaim (inline get-column-vector!))
(defun get-column-vector! (out m index)
  (declare (optimize (speed 3)))
  (with-values ((m m))
    (v2:with-values ((o out))
      (ecase index
        (0 (setf ox m00 oy m10))
        (1 (setf ox m01 oy m11)))))
  out)

(define-ftype get-column-vector (matrix (integer 0 1)) v2:vector)
(declaim (inline get-column-vector))
(defun get-column-vector (m index)
  (declare (optimize (speed 3)))
  (get-column-vector! (v2:zero) m index))

(define-ftype get-column-values (matrix (integer 0 1)) (cl:values f64 f64))
(declaim (inline get-column-values))
(defun get-column-values (m index)
  (with-values ((m m))
    (ecase index
      (0 (cl:values m00 m10))
      (1 (cl:values m01 m11)))))

(define-ftype get-row-vector! (v2:vector matrix (integer 0 1)) v2:vector)
(declaim (inline get-row-vector!))
(defun get-row-vector! (out m index)
  (declare (optimize (speed 3)))
  (with-values ((m m))
    (v2:with-values ((o out))
      (ecase index
        (0 (setf ox m00 oy m01))
        (1 (setf ox m10 oy m11)))))
  out)

(define-ftype get-row-vector (matrix (integer 0 1)) v2:vector)
(declaim (inline get-row-vector))
(defun get-row-vector (m index)
  (declare (optimize (speed 3)))
  (get-row-vector! (v2:zero) m index))

(define-ftype get-row-values (matrix (integer 0 1)) (cl:values f64 f64))
(declaim (inline get-row-values))
(defun get-row-values (m index)
  (with-values ((m m))
    (ecase index
      (0 (cl:values m00 m01))
      (1 (cl:values m10 m11)))))

(define-ftype set-column-vector! (matrix v2:vector (integer 0 1)) matrix)
(declaim (inline set-column-vector!))
(defun set-column-vector! (out v index)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (v2:with-values ((v v))
      (ecase index
        (0 (setf o00 vx o10 vy))
        (1 (setf o01 vx o11 vy)))))
  out)

(define-ftype set-column-vector (matrix v2:vector (integer 0 1)) matrix)
(declaim (inline set-column-vector))
(defun set-column-vector (m v index)
  (declare (optimize (speed 3)))
  (set-column-vector! (copy m) v index))

(define-ftype set-column-values! (matrix f64 f64 (integer 0 1)) matrix)
(declaim (inline set-column-values!))
(defun set-column-values! (out x y index)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (ecase index
      (0 (setf o00 x o10 y))
      (1 (setf o01 x o11 y))))
  out)

(define-ftype set-column-values (matrix f64 f64 (integer 0 1)) matrix)
(declaim (inline set-column-values))
(defun set-column-values (m x y index)
  (declare (optimize (speed 3)))
  (set-column-values! (copy m) x y index))

(define-ftype set-row-vector! (matrix v2:vector (integer 0 1)) matrix)
(declaim (inline set-row-vector!))
(defun set-row-vector! (out v index)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (v2:with-values ((v v))
      (ecase index
        (0 (setf o00 vx o01 vy))
        (1 (setf o10 vx o11 vy)))))
  out)

(define-ftype set-row-vector (matrix v2:vector (integer 0 1)) matrix)
(declaim (inline set-row-vector))
(defun set-row-vector (m v index)
  (declare (optimize (speed 3)))
  (set-row-vector! (copy m) v index))

(define-ftype set-row-values! (matrix f64 f64 (integer 0 1)) matrix)
(declaim (inline set-row-values!))
(defun set-row-values! (out x y index)
  (declare (optimize (speed 3)))
  (with-values ((o out))
    (ecase index
      (0 (setf o00 x o01 y))
      (1 (setf o10 x o11 y))))
  out)

(define-ftype set-row-values (matrix f64 f64 (integer 0 1)) matrix)
(declaim (inline set-row-values))
(defun set-row-values (m x y index)
  (declare (optimize (speed 3)))
  (set-row-values! (copy m) x y index))
