(in-package :cl-tuples)

(def-tuple-type quaternion
  :tuple-element-type fast-float
  :initial-element 0.0f0
  :elements (x y z w))

(export-tuple-operations quaternion)

(def-tuple-type angle-axis
  :tuple-element-type fast-float
  :initial-element 0.0f0
  :elements (x y z a))

(export-tuple-operations angle-axis)

(def-tuple-op angle-axis-matrix33*
    ((aa angle-axis (x y z a)))
  (:return matrix33
           (matrix33-map*
            (+)
            (identity-matrix33*)
            (matrix33-scale*
             (sin a)
             #1=(matrix33-values*
                0.0 (- z) y
                z 0.0 (- x)
                (- y) x 0.0))
            (matrix33-scale*
             (- 1 (cos a))
             (matrix33-product*
              #1# #1#)))))

(def-tuple-op matrix33-trace*
    ((m matrix33 #.(tuple-elements 'matrix33)))
  (:return fast-float
           (+ e00 e11 e22)))

(def-tuple-op vector3d-angle-axis*
    ((axis vector3d (x y z))
     (angle fast-float))
  (:return angle-axis
           (angle-axis-values* x y z angle)))

(def-tuple-op matrix33-angle-axis*
    ((m matrix33 #.(tuple-elements 'matrix33)))
  (:return angle-axis
           (let* ((trace (matrix33-trace* m))
                  (angle (acos (/ (1- trace) 2))))
             (cond
               ((= angle 0.0)
                (angle-axis-key-values))
               ((< 0.0 angle #1=#.(coerce pi 'fast-float))
                (vector3d-angle-axis*
                 (vector3d-normal*
                  (vector3d-values*
                   (- e21 e12)
                   (- e02 e20)
                   (- e10 e01)))
                 angle))
               ((= angle #1#)
                (vector3d-angle-axis*
                 (flet ((u0 ()
                          (let* ((u0 (/ (sqrt (1+ (- e00 e11 e22))) 2))
                                 (2u0 (* 2 u0)))
                            (vector3d-values* u0 (/ e01 2u0) (/ e02 2u0))))
                        (u1 ()
                          (let* ((u1 (/ (sqrt (1+ (- e11 e00 e22))) 2))
                                 (2u1 (* 2 u1)))
                            (vector3d-values* (/ e01 2u1) u1 (/ e12 2u1))))
                        (u2 ()
                          (let* ((u2 (/ (sqrt (1+ (- e22 e00 e11))) 2))
                                 (2u2 (* 2 u2)))
                            (vector3d-values* (/ e02 2u2) (/ e12 2u2) u2))))
                   (if (> e00 e11)
                       (if (> e00 e22)
                           (u0)
                           (if (> e22 e11)
                               (u2)
                               (u1)))
                       (if (> e22 e11)
                           (u2)
                           (u1))))
                 angle))))))

;; need conjugate, angle-axis conversion, slerp

(def-tuple-op quaternion-conjugate*
    ((q quaternion (x y z w)))
  (quaternion-values* (- x) (- y) (- z) w))

(def-tuple-op quaternion-scale*
    ((q quaternion (x y z w))
     (s fast-float))
  "Multiply a quat by a scalar"
  (:return quaternion
           (quaternion-map*
            (*) (quaternion-key-values :initial-element s) q)))

(def-tuple-op quaternion-dot*
    ((q0 quaternion)
     (q1 quaternion))
  "Dot product of two quaternions."
  (:return fast-float
           (quaternion-reduce*
            (+) (quaternion-map* (*) q0 q1))))

(def-tuple-op quaternion-mag-square*
    ((q quaternion))
  (:return fast-float
           (quaternion-dot* q q)))

(def-tuple-op quaternion-mag*
    ((q quaternion))
  (:return fast-float
           (sqrt (quaternion-mag-square* q))))

(def-tuple-op quaternion-normalize*
    ((q quaternion))
  "Ensure a quaternion is a unit"
  (:return quaternion
           (quaternion-scale*
            q (/ (quaternion-mag* q)))))

(def-tuple-op quaternion-sum*
    ((q0 quaternion)
     (q1 quaternion))
  "Sum the components of two quaternions"
  (:return quaternion
           (quaternion-map* (+) q0 q1)))

(def-tuple-op quaternion-inverse*
    ((q quaternion))
  "Inverse of quaternion"
  (:return quaternion
           (quaternion-scale*
            q (/ (quaternion-mag-square* q)))))


(def-tuple-op quaternion-product*
	((q-lhs quaternion (x1 y1 z1 w1))
	 (q-rhs quaternion (x2 y2 z2 w2)))
  "Multiple of two quaternions"
  (:return quaternion
		   (quaternion-values* (- (+ (* w1 x2) (* x1 w2) (* y1 z2)) (* z1 y2))
							   (- (+ (* w1 y2) (* y1 w2) (* z1 x2)) (* x1 z2))
							   (- (+ (* w1 z2) (* x1 y2) (* z1 w2)) (* y1 x2))
							   (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2)))))

(def-tuple-op quaternion-matrix33*
	((q quaternion (x y z w)))
  "Convert a quaternion to a 3x3 rotation matrix."
  (:return matrix33
		   (matrix33-values*
			(- 1 (* 2 y y) (* 2 z z))   (- (* 2 x y) (* 2 w z))   (+ (* 2 x z) (* 2 w y))
			(+   (* 2 x y) (* 2 w z))   (- 1 (* 2 x x) (* 2 z z)) (- (* 2 y z) (* 2 w z))
			(-   (* 2 x z) (* 2 w y))   (+  (* 2 y z) (* 2 w z))  (- 1 (* 2 x x) (* 2 y y)))))

(def-tuple-op angle-axis-quaternion*
    ((aa angle-axis (x y z a)))
  "Convert an angle-axis tuple to a quaternion tuple"
  (:return quaternion
           (let* ((a/2 (* 0.5 a))
                  (sin-angle (sin a/2)))
             (quaternion-values*
              (* x sin-angle)
              (* y sin-angle)
              (* z sin-angle)
              (cos a/2)))))

(def-tuple-op quaternion-angle-axis*
    ((q quaternion (x y z w)))
  "Convert an quaternion tuple to an angle-axis tuple.  If the angle is
zero, the axis isn't defined, so the unit x vector is returned instead."
  (:return angle-axis
           ;; either test for one, disable traps, or catch the exception
           (if (or (= w 1.0) (= w -1.0))
               (angle-axis-key-values x 1.0)
               (let ((angle (* 2 (acos w))))
                 (vector3d-angle-axis*
                  (vector3d-scale*
                   (vector3d-values* x y z)
                   (/ (sqrt (- 1 (* w w)))))
                  angle)))))

(def-tuple-op quaternion-transform-vector3d*
    ((vector vector3d (vx vy vz))
     (quat quaternion (qx qy qz qw)))
  "Transform a 3d vector with a quaternion"
  (:return vector3d
           (with-quaternion*
               (quaternion-product*
                (quaternion-product*
                 quat
                 (vector3d-quaternion* vector))
                (quaternion-conjugate* quat))
               (rx ry rz rw)
             (vector3d-values* rx ry rz))))

(def-tuple-op vector3d-quaternion*
    ((vector vector3d (vx vy vz)))
  "Convert a 3d vector into q auqt for angular velocity purposes"
  (:return quaternion
           (quaternion-values* vx vy vz 0.0)))
