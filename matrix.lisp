
(in-package :cl-tuples)

(def-tuple-type matrix22
	:tuple-element-type fast-float
	:initial-element 0.0f0
	:elements (e00 e01
		   e10 e11))

(export-tuple-operations matrix22)

(def-tuple-type matrix33
	:tuple-element-type fast-float
	:initial-element 0.0f0
	:elements (e00 e01 e02
				   e10 e11 e12
				   e20 e21 e22))

(export-tuple-operations matrix33)

(def-tuple-type matrix44
	:tuple-element-type fast-float
	:initial-element 0.0f0
	:elements (e00 e01 e02 e03
				   e10 e11 e12 e13
				   e20 e21 e22 e23
				   e30 e31 e32 e33))

(export-tuple-operations matrix44)

(def-tuple-op matrix33-equal*
    ((k matrix33 (k00 k01 k02
                  k10 k11 k12
                  k20 k21 k22))
     (m matrix33 (m00 m01 m02
                  m10 m11 m12
                  m20 m21 m22)))
  (:return boolean
           (and
            (= k00 m00) (= k01 m01) (= k02 m02)
            (= k10 m10) (= k11 m11) (= k12 m12)
            (= k20 m20) (= k21 m21) (= k22 m22))))

(def-tuple-op matrix44-equal*
    ((k matrix44 (k00 k01 k02 k03
                  k10 k11 k12 k13
                  k20 k21 k22 k23
                  k30 k31 k32 k33))
     (m matrix44 (m00 m01 m02 m03
                  m10 m11 m12 m13
                  m20 m21 m22 m23
                  m30 m31 m32 m33)))
  (:return boolean
           (and
            (= k00 m00) (= k01 m01) (= k02 m02) (= k03 m03)
            (= k10 m10) (= k11 m11) (= k12 m12) (= k13 m13)
            (= k20 m20) (= k21 m21) (= k22 m22) (= k23 m23)
            (= k30 m30) (= k31 m31) (= k32 m32) (= k33 m33))))

;; TODO: this probably needs to check the TRACE as well, since otherwise
;; it might crash in other functions (e.g. MATRIX33-ANGLE-AXIS*)
(def-tuple-op check-rotation-matrix33*
    ((m matrix33 :default))
  (:return boolean (matrix33-equal*
                    (identity-matrix33*)
                    (matrix33-product*
                     m (transpose-matrix33* m)))))

;; TODO: maybe check also whether it is purely a rotation?
(def-tuple-op check-rotation-matrix44*
    ((m matrix44 :default))
  (:return boolean (check-rotation-matrix33* (matrix44-matrix33* m))))

(def-tuple-op transpose-matrix22*
    ((mat22 matrix22
				 (e00 e01
					  e10 e11)))
	"Return the transpose of the matrix"
	(:return matrix22
			 (matrix22-values*
			  e00 e10
			  e01 e11)))

(defmacro matrix-dot (dimension row col)
  "Generate the symbols required for a dot product between the row and column of a matrix, assuming accessor symbol is e<row><col>"
  (labels
	  ((make-matrix-element-symbol (mat row col)
		 (intern (string-upcase (format nil "e~A~A~A" mat row col)) :cl-tuples)))
	(let
		((col-sym-names
		  (loop for row from 0 below dimension
			 collect (make-matrix-element-symbol 0 row col)))
		 (row-sym-names
		  (loop for col from 0 below dimension
			 collect (make-matrix-element-symbol 1 row col))))
	  `(+
		,@(loop
			 for col-sym in col-sym-names
			 for row-sym in row-sym-names
			 collect `(* ,col-sym ,row-sym))))))


(def-tuple-op transform-vertex2d*
	((mat matrix33 (e00 e01 e02 e10 e11 e12 e20 e21 e22))
	 (vert vector2d (x y w)))
  (:return vertex2d
		   (vertex2d-values*
			(+ (* x e00) (* y e01) (* w e02))
			(+ (* x e10) (* y e11) (* w e12))
			(+ (* x e20) (* y e21) (* w e22)))))


(def-tuple-op transform-vector2d*
	((mat matrix33 (e00 e01 e02 e10 e11 e12 e20 e21 e22))
	 (vec vector2d (x y)))
  (:return vector2d
		   (vector2d-values*
			(+ (* x e00) (* y e01))
			(+ (* x e10) (* y e11)))))


(def-tuple-op matrix33-product*
	((m0 matrix33 (e000 e001 e002 e010 e011 e012 e020 e021 e022))
	 (m1 matrix33 (e100 e101 e102 e110 e111 e112 e120 e121 e122)))
  (:return matrix33
		   (matrix33-values*
			(matrix-dot 3 0 0)
			(matrix-dot 3 0 1)
			(matrix-dot 3 0 2)

			(matrix-dot 3 1 0)
			(matrix-dot 3 1 1)
			(matrix-dot 3 1 2)

			(matrix-dot 3 2 0)
			(matrix-dot 3 2 1)
			(matrix-dot 3 2 2))))


(def-tuple-op transform-vertex3d*
	((mat matrix44
		  (e00 e01 e02 e03
			   e10 e11 e12 e13
			   e20 e21 e22 e23
			   e30 e31 e32 e33))
	 (vert vertex3d (x y z w)))
  (:return vertex3d
		   (vertex3d-values*
			(+ (* x e00) (* y e01) (* z e02) (* w e03))
			(+ (* x e10) (* y e11) (* z e12) (* w e13))
			(+ (* x e20) (* y e21) (* z e22) (* w e23))
			(+ (* x e30) (* y e31) (* z e32) (* w e33)))))

(def-tuple-op transform-vector3d*
	((mat matrix33
		  (e00 e01 e02
			   e10 e11 e12
			   e20 e21 e22))
	 (vect vector3d (x y z)))
  (:return vector3d
		   (vector3d-values*
			(+ (* x e00) (* y e01) (* z e02) )
			(+ (* x e10) (* y e11) (* z e12) )
			(+ (* x e20) (* y e21) (* z e12) ))))

(def-tuple-op transpose-matrix33*
    ((mat33 matrix33 (e00 e01 e02 e10 e11 e12 e20 e21 e22)))
	"Return the transpose of the matrix"
	(:return matrix33
			 (matrix33-values*
			  e00 e10 e20
			  e01 e11 e21
			  e02 e12 e22)))

(def-tuple-op print-matrix33*
	((mat matrix44 (e00 e01 e02 e03
						e10 e11 e12 e13
						e20 e21 e22 e23
						e30 e31 e32 e33)))
  "Print a 3x3 matrix in a useful format."
  (:return (values)
		   (format t "~A ~A ~A ~A ~%" e00 e01 e02)
		   (format t "~A ~A ~A ~A ~%" e10 e11 e12)
		   (format t "~A ~A ~A ~A ~%" e20 e21 e22)))

(def-tuple-op matrix44-product*
	((m0 matrix44 (e000 e001 e002 e003 e010 e011 e012 e013  e020 e021 e022 e023  e030 e031 e032 e033))
	 (m1 matrix44 (e100 e101 e102 e103 e110 e111 e112 e113  e120 e121 e122 e123  e130 e131 e132 e133)))
  (:return matrix44
		   (matrix44-values*
			(matrix-dot 4 0 0)
			(matrix-dot 4 0 1)
			(matrix-dot 4 0 2)
			(matrix-dot 4 0 3)

			(matrix-dot 4 1 0)
			(matrix-dot 4 1 1)
			(matrix-dot 4 1 2)
			(matrix-dot 4 1 3)

			(matrix-dot 4 2 0)
			(matrix-dot 4 2 1)
			(matrix-dot 4 2 2)
			(matrix-dot 4 2 3)

			(matrix-dot 4 3 0)
			(matrix-dot 4 3 1)
			(matrix-dot 4 3 2)
			(matrix-dot 4 3 3))))

(def-tuple-op identity-matrix22*
    ()
  (:return matrix22
           (matrix22-key-values
            e00 1.0
            e11 1.0)))

(def-tuple-op identity-matrix33*
    ()
  (:return matrix33
           (matrix33-key-values
            e00 1.0
            e11 1.0
            e22 1.0)))

(def-tuple-op identity-matrix44*
    ()
  (:return matrix44
           (matrix44-key-values
            e00 1.0
            e11 1.0
            e22 1.0
            e33 1.0)))

(def-tuple-op translation-matrix44*
	((tx fast-float)
	 (ty fast-float)
	 (tz fast-float))
  "Return a matrix that represents a translation transformation"
  (:return matrix44
		   (matrix44-values*
			1.0f0 0.0f0 0.0f0 tx
			0.0f0 1.0f0 0.0f0 ty
			0.0f0 0.0f0 1.0f0 tz
			0.0f0 0.0f0 0.0f0 1.0f0)))

(def-tuple-op scaling-matrix44*
    ((sx #1=fast-float)
     (sy #1#)
     (sz #1#))
  (:return matrix44
           (matrix44-key-values
            e00 sx
            e11 sy
            e22 sz
            e33 1.0)))

(def-tuple-op vertex3d-translation-matrix44*
	((vert vertex3d (tx ty tz tw)))
  "Return a matrix that represents a translation transformation"
  (:return matrix44
		   (matrix44-values*
			1.0f0 0.0f0 0.0f0 tx
			0.0f0 1.0f0 0.0f0 ty
			0.0f0 0.0f0 1.0f0 tz
			0.0f0 0.0f0 0.0f0 1.0f0)))

(def-tuple-op rotatex-matrix33*
    ((rotation fast-float))
  "Return a matrix for rotating around the x axis."
  (:return matrix33
           (let* ((sin (sin rotation))
                  (-sin (- sin))
                  (cos (cos rotation)))
             (matrix33-values*
              1.0 0.0 0.0
              0.0 cos -sin
              0.0 sin cos))))

(def-tuple-op rotatex-matrix44*
    ((rotation fast-float))
  "Return a matrix for rotating around the x axis."
  (:return matrix44
           (matrix33-matrix44*
            (rotatex-matrix33* rotation))))

(def-tuple-op rotatey-matrix33*
    ((rotation fast-float))
  "Return a matrix for rotating around the y axis."
  (:return matrix33
           (let* ((sin (sin rotation))
                  (-sin (- sin))
                  (cos (cos rotation)))
             (matrix33-values*
              cos  0.0 sin
              0.0  1.0 0.0
              -sin 0.0 cos))))

(def-tuple-op rotatey-matrix44*
    ((rotation fast-float))
  "Return a matrix for rotating around the y axis."
  (:return matrix44
           (matrix33-matrix44*
            (rotatey-matrix33* rotation))))

(def-tuple-op rotatez-matrix33*
    ((rotation fast-float))
  "Return a matrix for rotating around the z axis."
  (:return matrix33
           (let* ((sin (sin rotation))
                  (-sin (- sin))
                  (cos (cos rotation)))
             (matrix33-values*
              cos -sin 0.0
              sin cos  0.0
              0.0 0.0  1.0))))

(def-tuple-op rotatez-matrix44*
    ((rotation fast-float))
  "Return a matrix for rotating around the z axis."
  (:return matrix44
           (matrix33-matrix44*
            (rotatez-matrix33* rotation))))

(def-tuple-op transpose-matrix44*
	((mat44 matrix44
			(e00 e01 e02 e03
				 e10 e11 e12 e13
				 e20 e21 e22 e23
				 e30 e31 e32 e33)))
  "Return the transpose of the matrix"
  (:return matrix44
		   (matrix44-values*
			e00 e10 e20 e30
			e01 e11 e21 e31
			e02 e12 e22 e32
			e03 e13 e23 e33)))

(def-tuple-op make-test-matrix44*
	()
  "Return a matrix for testing purposes"
  (:return matrix44
		   (matrix44-values*
			1.0f0  2.0f0  3.0f0  4.0f0
			5.0f0  6.0f0  7.0f0  8.0f0
			9.0f0  10.0f0 11.0f0 12.0f0
			13.0f0 14.0f0 15.0f0 16.0f0)))


(def-tuple-op print-matrix44*
	((mat matrix44 (e00 e01 e02 e03
						e10 e11 e12 e13
						e20 e21 e22 e23
						e30 e31 e32 e33)))
  "Print a matrix in a useful format."
  (:return (values)
		   (format t "~A ~A ~A ~A ~%" e00 e01 e02 e03)
		   (format t "~A ~A ~A ~A ~%" e10 e11 e12 e13)
		   (format t "~A ~A ~A ~A ~%" e20 e21 e22 e23)
		   (format t "~A ~A ~A ~A ~%" e30 e31 e32 e33)))


(def-tuple-op matrix44-matrix33*
	((mat44 matrix44
			(e00 e01 e02 e03
				 e10 e11 e12 e13
				 e20 e21 e22 e23
				 e30 e31 e32 e33)))
  "Convert a 4x4 matrix to a 3x3 matrix"
  (:return matrix33
		   (matrix33-values* e00 e01 e02 e10 e11 e12 e20 e21 e22)))

(def-tuple-op matrix33-matrix44*
	((mat3 matrix33
		   (e00 e01 e02
				e10 e11 e12
				e20 e21 e22)))
  "Convert a 3x3 matrix to a 4x4 matrix"
  (:return matrix44
		   (matrix44-values* e00 e01 e02 0.0f0
					  e10 e11 e12 0.0f0
					  e20 e21 e22 0.0f0
					  0.0f0 0.0f0 0.0f0 1.0f0)))

(def-tuple-op vector3d-matrix3d*
	((zvec vector3d (zx zy zz))
	 (yvec vector3d (yx yy yz)))
  "Construct a rotation matrix from 2 vectors"
  (:return matrix33
		   (with-vector3d
			   (vector3d-cross
				zvec yvec)
			   (xx xy xz)
			 (matrix33*
			  (xx yx zx
				  xy yy zy
				  xz yz zz)))))


(def-tuple-op matrix22-determinant*
    ((mat matrix22 #.(tuple-elements 'matrix22)))
  (:return fast-float
           (- (* e00 e11)
              (* e01 e10))))

(def-tuple-op matrix33-determinant*
    ((mat matrix33 #.(tuple-elements 'matrix33)))
  (:return fast-float
           (- (+ (* e00 e11 e22)
                 (* e01 e12 e20)
                 (* e02 e10 e21))
              (* e02 e11 e20)
              (* e01 e10 e22)
              (* e00 e12 e21))))

(def-tuple-op matrix44-determinant*
    ((mat matrix44 #.(tuple-elements 'matrix44)))
  (:return fast-float
           (let ((t0 (* e00 e22))
                 (t1 (* e11 e33))
                 (t2 (* e01 e23))
                 (t3 (* e12 e30))
                 (t4 (* e03 e21))
                 (t5 (* e10 e32))
                 (t6 (* e13 e31))
                 (t7 (* e02 e20)))
             (- (+ (* t0 t1)
                   (* t2 t3)
                   (* t7 t6)
                   (* t4 t5))
                (* t3 t4)
                (* t7 t1)
                (* t2 t5)
                (* t0 t6)))))

(def-tuple-op matrix22-scale*
    ((x fast-float)
     (mat matrix22 #1=#.(tuple-elements 'matrix22)))
  (:return matrix22 (multiply-arguments matrix22-values* x #1#)))

(def-tuple-op matrix33-scale*
    ((x fast-float)
     (mat matrix33 #1=#.(tuple-elements 'matrix33)))
  (:return matrix33 (multiply-arguments matrix33-values* x #1#)))

(def-tuple-op matrix44-scale*
    ((x fast-float)
     (mat matrix44 #1=#.(tuple-elements 'matrix44)))
  (:return matrix44 (multiply-arguments matrix44-values* x #1#)))

(def-tuple-op cofactor-matrix22*
    ((mat matrix22 #.(tuple-elements 'matrix22)))
  (:return matrix22
           (matrix22-values*
            e11 e10
            e01 e00)))

(def-tuple-op cofactor-matrix33*
    ((mat matrix33 #.(tuple-elements 'matrix33)))
  (:return matrix33
           (macrolet ((cofactors ()
                        `(matrix33-values*
                          ,@(matrix-cofactors 3))))
             (cofactors))))

(def-tuple-op cofactor-matrix44*
    ((mat matrix44 #.(tuple-elements 'matrix44)))
  (:return matrix44
           (macrolet ((cofactors ()
                        `(matrix44-values*
                          ,@(matrix-cofactors 4))))
             (cofactors))))

(def-tuple-op inverted-matrix22*
    ((mat matrix22 #.(tuple-elements 'matrix22)))
  (:return matrix22
           (matrix22-scale*
            (matrix22-determinant* mat)
            (transpose-matrix22*
             (cofactor-matrix22* mat)))))

(def-tuple-op inverted-matrix33*
    ((mat matrix33 #.(tuple-elements 'matrix33)))
  (:return matrix33
           (matrix33-scale*
            (matrix33-determinant* mat)
            (transpose-matrix33*
             (cofactor-matrix33* mat)))))

(def-tuple-op inverted-matrix44*
    ((mat matrix44 #.(tuple-elements 'matrix44)))
  (:return matrix44
           (matrix44-scale*
            (matrix44-determinant* mat)
            (transpose-matrix44*
             (cofactor-matrix44* mat)))))
