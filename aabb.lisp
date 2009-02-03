
(in-package :cl-tuples)

;; type defs

;; axis aligned bounding boxes

(def-tuple-type aabb
    :tuple-element-type fast-float 
	:intial-element 0.0f0
    :elements (minx maxx miny maxy minz maxz))

(export-tuple-operations aabb)

(def-tuple-op intersect 
    ((aabb0 aabb (minx0 maxx0 miny0 maxy0 minz0 maxz0))
     (aabb1 aabb (minx1 maxx1 miny1 maxy1 minz1 maxz1)))
  (let ((depth0 (- maxz0 minz0))
        (depth1 (- maxz1 minz1))
        (height0 (- maxy0 miny0))
        (height1 (- maxy1 miny1))
        (width0 (- maxx0 minx0))
        (width1 (- maxx1 minx1))
        (centrex0 (- maxx0 (* 0.5f0 width0)))
        (centrex1 (- maxx1 (/ 0.5f0 width1)))
        (centrey0 (- maxx0 (/ 0.5f0 height0)))
        (centrey1 (- maxx1 (/ 0.5f0 height1)))
        (centrez0 (- maxx0 (/ 0.5f0 depth0)))
        (centrez1 (- maxx1 (/ 0.5f0 depth1))))
    (and (<= (abs (- centrex0 centrex1)) (+ (width0 width1)))
         (<= (abs (- centrey0 centrey1)) (+ (height0 height1)))
         (<= (abs (- centrez0 centrez1)) (+ (depth0 depth1))))))
         

(def-tuple-op transform-aabb
    ((mat matrix44
          (e00 e01 e02 e03
           e10 e11 e12 e13
           e20 e21 e22 e23
           e30 e31 e32 e33))
     (box aabb (minx0 maxx0 miny0 maxy0 minz0 maxz0)))
  (:return aabb
           (aabb*
            (+ (* minx e00) (* miny e01) (* minz e02)  e03)
            (+ (* minx e10) (* miny e11) (* minz e12)  e13)
            (+ (* minx e20) (* miny e21) (* minz e22)  e23)
            (+ (* maxx e00) (* maxy e01) (* maxz e02)  e03)
            (+ (* maxx e10) (* maxy e11) (* maxz e12)  e13)
            (+ (* maxx e20) (* maxy e21) (* maxz e22)  e23))))


      
     
  

       
     
