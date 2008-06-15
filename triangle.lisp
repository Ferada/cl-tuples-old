
(in-package :cl-tuples)

(def-tuple-type triangle 
    :tuple-element-type (unsigned-byte 16) 
    :elements (a b c))

(export-tuple-operations triangle)
