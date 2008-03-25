
(in-package :cl-tuples)

(def-tuple-type quaternion
    :tuple-element-type single-float
    :elements (x y z w))

(export-tuple-operations quaternion)

(def-tuple-type angle-axis
    :tuple-element-type single-float
    :elements (x y z a))

(export-tuple-operations angle-axis)

;; need conjugate, angle-axis conversion, slerp

(def-tuple-op quaternion-conjugate 
  ((q quaternion (x y z w)))
  (quaternion-tuple (- x) (- y) (- z) w))


(def-tuple-op quaternion-dot 
    ((quaternion-lhs quaternion (x0 y0 z0 w0)) 
     (quaternion-rhs quaternion (x1 y1 z1 w1)))
    "Dot product of two quaternions."
     (reduce-quaternion-tuple 
       #'+ (map-quaternion-tuples #'* 
                                 (quaternion-tuple x0 y0 z0 w0) 
                                 (quaternion-tuple x1 y1 z1 w1))))

(def-tuple-op quaternion-inverse 
  ((q quarternion (x y z w)))
    "Inverse of quaternion"
   (let ((dot-recip 
          (/ 1.0 (quaternion-dot 
                  (quaternion-tuple x y z w) 
                  (quaternion-tuple)))))    
     (map-quaternion-tuples
         #'(lambda (t) (* t dot-recip))
         (quaternion-conjugate (quaternion-tuple x y z w)))))

(def-tuple-op quaternion-product
    ((q-lhs quaternion (x1 y1 z1 w1))
     (q-rhs quaternion (x2 y2 z2 w2)))
  "Multiple of two quaternions"
  (quaternion-tuple (- (+ (* w1 x2) (* x1 w2) (* y1 z2)) (* z1 y2))
                    (- (+ (* w1 y2) (* y1 w2) (* z1 x2)) (* x1 z2))
                    (- (+ (* w1 z2) (* x1 y2) (* z1 w2)) (* y1 x2))
                    (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2))))

(def-tuple-op quaternion-matrix33 
  ((q quaternion (x y z w)))
   "Convert a quaternion to a 3x3 rotation matrix."
     (matrix33-tuple
      (- 1 (* 2 y y) (* 2 z z))   (- (* 2 x y) (* 2 w z))   (+ (* 2 x z) (* 2 w y))
      (+   (* 2 x y) (* 2 w z))   (- 1 (* 2 x x) (* 2 z z)) (- (* 2 y z) (* 2 w x))
      (-   (* 2 x z) (* 2 w y))   (+  (* 2 y z) (* 2 w z))  (- 1 (* 2 x x) (* 2 y y))))
     

(defmacro angle-axis-quaternion (aa)
  "Convert an angle-axis tuple to a quaternion"
  (with-gensyms (cosa sina)
  `(with-angle-axis ,aa
       (x y z a)
     (let ((,cosa (cos (/ a 2.0)))
           (,sina (sin (/ a 2.0))))
       (quaternion-tuple (* x ,sina) (* y ,sina) (* z ,sina) (* a ,cosa))))))
                
    
(def-tuple-op quaternion-transform-vector3d
    ((vector vector3d (vx vy vz vw))
     (quat quaternion (qx qy qz qw)))
  "Transform a 3d vector with a quaternion"
  (with-quaternion 
      (quaternion-product
       (quaternion-product
        (quaternion-tuple qx qy qz qw)
        (quaternion-tuple vx vy vz 0.0))
       (quaternion-conjugate (quaternion-tuple qx qy qz qw)))
      (rx ry rz rw)
    (vector3d-tuple rx ry rz)))