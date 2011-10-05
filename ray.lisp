  
(in-package :cl-tuples)

(def-tuple-type ray
	:tuple-element-type fast-float
	:intial-element 0.0f0
	:elements (ox oy oz dirx diry dirz))

(export-tuple-operations ray)

(def-tuple-op ray-origin*
	((ray ray (ox oy oz dirx diry dirz)))
  (:return vector3d
		   (vector3d-values ox oy oz)))

(def-tuple-op ray-direcion*
	((ray ray (ox oy oz dirx diry dirz)))
  (:return vector3d
		   (vector3d-values dirx diry dirz)))
