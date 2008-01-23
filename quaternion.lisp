
(in-package :cl-tuples)

(def-tuple-type quaternion
    :tuple-element-type single-float
    :elements (x y z w))

(def-tuple-type angle-axis
    :tuple-element-type single-float
    :elements (x y z a))

;; need conjugate, angle-axis conversion, slerp

(defmacro quaterion-conjugate (q)
  `(with-quaternion ,q
       (x y z a)
     (values (-x ) (- y) (- z) a)))

(defmacro quaternion-dot (q0 q1)
  `(with-quaternion ,q0
       (x0 y0 z0 a0)
     (with-quaternion ,q1
         (x1 y1 z1 a1)
       (+
        (- (* a0 a1) (* x0 x1) (* y0 y1) (* z0 z1))
        (+ (* a0 x1) (* x0 a1) (* y0 z1) (- (* z0 y1)))
        (+ (* a0 y1) (- (* x0 z1)) (* y0 a1) (* z0 x1))
        (+ (* a0 z1) (* x0 y1) (- (* y0 x1)) (* z0 a1))))))

(defmacro quaternion-mag-square (q)
  `(reduce-quaternion-values 
    #'+
    (map-quaternion-values #'* ,q ,q)))

(defmacro quaternion-mag (q)
  `(sqrt (quaternion-mag-square ,q)))

(defmacro quaternion-normal (q)
  (let ((mag-square (gensym)))
    `(with-quaternion ,q
         (x y z w) 
       (let ((,mag-square (quaternion-mag-sqare (values x y z w))))
         (map-quaternion-values #'(lambda (x)
                                    (/ x ,mag-square)) / (values x y z w))))))

(defmacro quaternion-matrix33 (q)
  "Convert a quaternion to a 3x3 rotation matrix."
  `(with-quaterion ,q
     (x y z w)
     (matrix33-tuple
      (- 1 (* 2 y y) (* 2 z z))   (- (* 2 x y) (* 2 w z))   (+ (* 2 x z) (* 2 w y))
      (+   (* 2 x y) (* 2 w z))   (- 1 (* 2 x x) (* 2 z z)) (- (* 2 y z) (* 2 w x))
      (-   (* 2 x z) (* 2 w y))   (+  (* 2 y z) (* 2 w z))  (- 1 (* 2 x x) (* 2 y y)))))
     

(defmacro angle-axis->quaternion (aa)
  "Convert an angle-axis tuple to a quaternion"
  (with-gensyms (cosa sina)
  `(with-angle-axis ,aa
       (x y z a)
     (let ((,cosa (cos (/ a 2)))
           (,sina (sin (/ a 2))))
       (quaternion-tuple (* x ,sina) (* y ,sina) (* z ,sina) (* w, cosa))))))
                
    