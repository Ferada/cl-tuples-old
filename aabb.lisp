
(in-package :cl-tuples)

;; type defs

;; axis aligned bounding boxes

(def-tuple-type aabb
    :tuple-element-type single-float 
    :elements (minx maxx miny maxy minz maxz))

(export-tuple-operations aabb)
