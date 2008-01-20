





(defmacro with-vector (vector-form names &body form)
  (destructuring-bind (x y z) names
	(assert (and (symbolp x) (symbolp y) (symbolp z)))
	`(multiple-value-bind (,x ,y ,z) ,vector-form  (progn ,@form))))

(defun vector-magnitude (v)
  (with-vector v (vx vy vz) (+ (* vx vx) (* vy vy) (* vz vz))))

(defun vector-normal (v)
  (with-vector v (vx vy vz)
	  (let ((mag (/ (1.0 (vector-magnitude v)))))
		(make-vector
		 :x (* (vector-x v) length)
		 :y (* (vector-y v) length)
		 :z (* (vector-z v) length)))))

(defun vector-dot (v0 v1)
  (with-vector v0 (v0x v0y v0z)
	  (with-vector v1 (v1x v1y v1z)
		  (+ (* v0x v1x))
			 (* v0y v1y))
			 (* v0z v1z)))

(defun vector-cross (v0 v1)
  (with-vector v0 (v0x v0y v0z)
	  (with-vector v1 (v1x v1y v1z)
		  (make-vector
		   :x (- (* v0y v1z)
				 (* v0z v1y))
		   :y (- (* v0z v1x)
				 (* v0x v1z))
		   :z  (- (* v0x v1y)
				  (* v0y v1x))))))

(defmacro vector-binary-op (v0 v1 op)
  (with-vector v0 (v0x v0y v0z)
	  (with-vector v1 (v1x v1y v1z)
		  (make-vector
		   :x (funcall ,op v0x v1x)
		   :y (funcall ,op v0y v1y)
		   :z (funcall ,op v0z v1z)))))

(defun vector-add (v0 v1)
  (vector-binary-op v0 v1 #'+))

(defun vector+ (v0 v1)
  (vector-binary-op v0 v1 #'+))

(defun vector-sub (v0 v1)
  (vector-binary-op v0 v1 #'-))

(defun vector-neg (v0)
  (make-vector
   :x (- (vector-x v0))
   :y (- (vector-y v0))
   :z (- (vector-z v0))))

(defmacro vector- (v0 &optional (v1 nil vector-sub-binary-p))
  (if (vector-sub-binary-p)
	  `(vector-sub v0 v1)
	  `(vector-neg v0)))

(defun vector* (v0 s op)
  (with-vector v0 (vx vy vz)
	  (make-vector
	   :x (* (vector-x v0) s)
	   :y (* (vector-y v0) s)
	   :z (* (vector-z v0) s))))

  

	


	
	

