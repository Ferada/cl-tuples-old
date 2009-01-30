
(in-package :cl-tuples)

(def-tuple-type colour
    :tuple-element-type fast-float
    :elements (r g b a))

(export-tuple-operations colour)
