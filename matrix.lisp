
(in-package :cl-tuples)

(def-tuple-type matrix33 
    :tuple-element-type single-float 
    :elements (e00 e01 e02
               e10 e11 e12
               e20 e21 e22))

(export-tuple-operations matrix33)

(def-tuple-type matrix44 
    :tuple-element-type single-float
    :elements (e00 e01 e02 e03
               e10 e11 e12 e13
               e20 e21 e22 e23
               e30 e31 e32 e33))

(export-tuple-operations matrix44)

(defmacro matrix-dot (dimension row col)
  "Generate the symbols required for a dot
   product between the row and column of a matrix"
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


(def-tuple-op transform-vertex2d 
    ((matrix33 mat (e00 e01 e02 e10 e11 e12 e20 e21 e22))
     (vertex2d vert (x y w)))
  (:return vertex2d
           (vertex2d*
            (+ (* x e00) (* y e01) (* w e02))
            (+ (* x e10) (* y e11) (* w e12))
            (+ (* x e20) (* y e21) (* w e22)))))


(def-tuple-op transform-vector2d 
    ((matrix33 mat (e00 e01 e02 e10 e11 e12 e20 e21 e22))
     (vector2d vec (x y)))
  (:return vector2d
           (vector2d*
            (+ (* x e00) (* y e01))
            (+ (* x e10) (* y e11)))))


(def-tuple-op matrix33-product 
    ((m0 matrix33 (e000 e001 e002 e010 e011 e012 e020 e021 e022))
     (m1 matrix33 (e100 e101 e102 e110 e111 e112 e120 e121 e122)))
    (:return matrix33
             (matrix33*
              (matrix-dot 3 0 0)
              (matrix-dot 3 0 1)
              (matrix-dot 3 0 2)
              
              (matrix-dot 3 1 0)
              (matrix-dot 3 1 1)
              (matrix-dot 3 1 2)
              
              (matrix-dot 3 2 0)
              (matrix-dot 3 2 1)
              (matrix-dot 3 2 2))))


(def-tuple-op transform-vertex3d 
    ((mat matrix44 
          (e00 e01 e02 e03
           e10 e11 e12 e13
           e20 e21 e22 e23
           e30 e31 e32 e33))
     (vert vertex3d (x y z w)))
  (:return vertex3d
           (vertex3d*
            (+ (* x e00) (* y e01) (* z e02) (* w e03))
            (+ (* x e10) (* y e11) (* z e02) (* w e13))
            (+ (* x e20) (* y e21) (* z e02) (* w e23))
            (+ (* x e30) (* y e31) (* z e32) (* w e33)))))

(def-tuple-op transform-vector3d 
    ((mat matrix44 
          (e00 e01 e02 e03
           e10 e11 e12 e13
           e20 e21 e22 e23
           e30 e31 e32 e33))
     (vect vector3d (x y z)))
  (:return vertex3d
           (vertex3d*
            (+ (* x e00) (* y e01) (* z e02) )
            (+ (* x e10) (* y e11) (* z e02) )
            (+ (* x e20) (* y e21) (* z e02) )
            (+ (* x e30) (* y e31) (* z e32) ))))


(def-tuple-op matrix44-product
    ((m0 matrix44 (e000 e001 e002 e003 e010 e011 e012 e013  e020 e021 e022 e023  e030 e031 e032 e033))
     (m1 matrix44 (e100 e101 e102 e103 e110 e111 e112 e113  e120 e121 e122 e123  e130 e131 e132 e133)))
  (:return matrix44
           (matrix44*
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

(def-tuple-op identity-matrix44 
    ()  
  (:return matrix44
         (matrix44*
          1.0 0.0 0.0 0.0
          0.0 1.0 0.0 0.0
          0.0 0.0 1.0 0.0
          0.0 0.0 0.0 1.0)))

(def-tuple-op translation-matrix 
    ((tx single-float)
     (ty single-float)
     (tz single-float))
  "Return a matrix that represents a translation transformation"
  (:return matrix44
           (matrix44*
            0.0 0.0 0.0 tx
            0.0 0.0 0.0 ty
            0.0 0.0 0.0 tz
            0.0 0.0 0.0 1.0)))


(def-tuple-op rotatex-matrix44 
    ((rotation single-float))
  "Return a matrix for rotating around the x axis."
  (:return matrix44
           (matrix44*
            1.0  0.0   0.0   0.0
            0.0 (cos rotation) (sin rotation)  0.0
            0.0 (sin rotation) (cos rotation)   0.0
            0.0  0.0  0.0     1.0)))



(def-tuple-op rotatey-matrix44    
    ((rotation single-float))
  "Return a matrix for rotating around the y axis."
  (:return matrix44
           (matrix44*
            (cos rotation)   0.0     (sin rotation)   0.0
            0.0     1.0     0.0     0.0
            (sin rotation)  0.0    (cos rotation)   0.0
            0.0     0.0     0.0     1.0)))

(def-tuple-op rotatez-matrix44
    ((rotation single-float))
  "Return a matrix for rotating around the z axis."
  (:return matrix44
           (matrix44*
            (cos rotation)   0.0   (sin rotation)  0.0
            (sin rotation)   0.0   (cos rotation)   0.0
            0.0     0.0   1.0     0.0
            0.0     0.0   0.0     1.0)))

(def-tuple-op make-test-matrix44
  ()
  "Return a matrix for testing purposes"
  (:return matrix44
          (matrix44*
           1.0  2.0  3.0  4.0
           5.0  6.0  7.0  8.0
           9.0  10.0 11.0 12.0
           13.0 14.0 15.0 16.0)))


(def-tuple-op print-matrix44
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