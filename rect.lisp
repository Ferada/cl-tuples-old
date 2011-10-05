(in-package :cl-tuples)

(def-tuple-type rect
    :tuple-element-type single-float
    :elements (left right top bottom))

(export-tuple-operations rect)

(def-tuple-op rect-width*
    ((r rect (left right top bottom)))
  (:return single-float 
           (- right left)))

(def-tuple-op rect-height*
    ((r rect (left right top bottom)))
  (:return single-float 
           (- bottom top)))