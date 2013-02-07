

(in-package :cl-tuples)

(defun make-translation (x y z)
  (let ((result (make-matrix-44)))
    (with-matrix44 result
		   (e00 e01 e02 e03
    		    e10 e11 e12 e13
		    e20 e21 e22 e23
		    e30 e31 e32 e33)
		   (setf e03 x)
		   (setf e13 y)
		   (setf e23 z))))

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

(defun scaling-matrix44 (sx sy sz)
  (make-matrix44* (scaling-matrix44* sx sy sz)))

(defun make-scaling (x y z)
  (scaling-matrix44 x y z))

(defun make-rotation (theta :key axis)
  (let
      ((s (sin theta))
       (c (cos theta))
       (result (make-matrix-44)))
    (with-matrix44 
     result
     (e00 e01 e02 e03
      e10 e11 e12 e13
      e20 e21 e22 e23
      e30 e31 e32 e33)
     (ecase axis
	    (:x ((setf e11 c)
		 (setf e22 c)
		 (setf e21 (- s))
		 (setf e12 s)))
	    (:y ((setf e00 c)
		 (setf e22 c)
		 (setf e02 (- s))
		 (setf e20 s))
	    (:z ((setf e00 c)
		 (setf e11 c)
		 (setf e01 s)
		 (setf e10 (-s )))))))))

(defun make-perspective (fov near far)
  (let
      ((s (sin (* 0.5 fov)))
       (c (cos (* 0.5 fov)))
       (q (/ s (- 1 (/ near far)))))
(with-matrix44 
     result
     (e00 e01 e02 e03
      e10 e11 e12 e13
      e20 e21 e22 e23
      e30 e31 e32 e33)
     (setf e00 c)
     (setf e11 c)
     (setf e22 q)
     (setf e32 s)
     (setf e23 (- (* q near))))))
	  
		
		  