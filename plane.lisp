
(in-package :cl-tuples)

(def-tuple-type plane
    :tuple-element-type single-float
    :elements (x y z d))

(def-tuple-op plane-normal 
	((p plane (px py px pd)))
  (:return vector3d
		   (vector3d-values* px py pz)))

(def-tuple-op plane-displacement 
	((p plane (px py px pd)))
  (:return fast-float
		   pd))

(def-tuple-op normal-displacement-plane 
	((vec vector3d (x y z))
	 (d fast-float))
  (:return plane
		   (plane-values x y z d)))

