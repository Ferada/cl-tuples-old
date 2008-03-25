
(in-package :cl-tuples)

(def-tuple-type colour
    :tuple-element-type single-float
    :elements (r g b a))

(export-tuple-operations colour)
