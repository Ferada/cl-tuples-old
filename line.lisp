
(in-package :cl-tuples)

(def-tuple-type line2d 
    :tuple-element-type single-float
	:initial-element 0.0f0
    :elements (sx sy ex ey))

(def-tuple-type line3d 
    :tuple-element-type single-float
	:initial-element 0.0f0
    :elements (sx sy sz ex ey ez))

(def-tuple-op line->vector2d 
	((line line2d (sx sy ex ey)))
  (:return vector2d
		   (vector2d* (- ex sx) (- ey sy))))

(def-tuple-op line-start->vertex2d
	((line line2d (sx sy ex ey)))
  (:return vertex2d
		   (vector2d* sx sy)))

(def-tuple-op line-end->vertex2d
	((line line2d (sx sy ex ey)))
  (:return vertex2d
		   (vector2d* sx sy)))
		   

(def-tuple-type line3d 
    :tuple-element-type single-float
    :elements (sx sy sz ex ey ez))

(def-tuple-op line->vector3d 
	((line line3d (sx sy ex ey)))
  (:return vector3d
		   (vector3d* (- ex sx) (- ey sy) (- ez sz))))

(def-tuple-op line-start->vertex3d
	((line line3d (sx sy ex ey)))
  (:return vertex3d
		   (vector3d* sx sy sz)))

(def-tuple-op line-end->vertex3d
	((line line3d (sx sy ex ey)))
  (:return vertex3d
		   (vector3d* sx sy sz)))
