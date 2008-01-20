
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

