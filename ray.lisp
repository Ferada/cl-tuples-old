  
(defmacro make-ray (&key origin direction)
  (let ((origin-x (gensym "ORX"))
		(origin-y (gensym "ORY"))
		(origin-z (gensym "ORX"))
		(direction-x (gensym "DIRX"))
		(direction-y (gensym "DIRY"))
		(direction-z (gensym "DIRZ")))
	`(multiple-value-bind (,origin-x ,origin-y ,origin-z) ,origin
	  (multiple-value-bind (,direction-x ,direction-y ,direction-z) ,direction
		(values ,origin-x ,origin-y ,origin-z ,direction-x ,direction-y ,direction-z)))))
	
(defmacro ray-origin (&key ray)
	`(multiple-value-bind (origin-x origin-y origin-z) ,ray
	  (values origin-x origin-y origin-z)))

(defmacro ray-direction (&key ray)
	`(multiple-value-bind (origin-x origin-y origin-z direction-x direction-y direction-z) 
	   ,ray
	  (declare (ignore origin-x origin-y origin-z))
	  (values direction-x direction-y direction-z)))
