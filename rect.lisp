(in-package :cl-tuples)

(def-tuple-type rect
    :tuple-element-type single-float
    :elements (left right top bottom))

(export-tuple-operations rect)

(def-tuple-op width
    ((r rect (left right top bottom)))
  (:return single-float 
           (- right left)))

(def-tuple-op height
    ((r rect (left right top bottom)))
  (:return single-float 
           (- bottom top)))