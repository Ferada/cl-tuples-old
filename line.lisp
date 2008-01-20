
(in-package :cl-tuples)

(def-tuple-type line2d 
    :tuple-element-type single-float
    :elements (sx sy ex ey))

(def-tuple-type line3d 
    :tuple-element-type single-float
    :elements (sx sy sz ex ey ez))

(defmacro line->vector2d (line2d)
  (with-line2d ,line2d
    (sx sy ex ey)
    (values (- ex sx) (- ey sy))))

(defmacro line-start->vertex2d (line2d)
  (with-line2d ,line2d
      (sx sy ex ey)
    (values sx sy 1)))

(defmacro line-end->vertex2d (line2d)
  (with-line2d ,line2d
      (sx sy ex ey)
    (values ex ey 1)))

(def-tuple-type line3d 
    :tuple-element-type single-float
    :elements (sx sy sz ex ey ez))

(defmacro line-start->vertex3d (line3d)
  (with-line3d ,line3d
      (sx sy sz ex ey ez)
    (values sx sy sz 1)))

(defmacro line-end->vertex3d (line3d)
  (with-line3d ,line3d
      (sx sy sz ex ey ez)
    (values ex ey ez 1)))

(defmacro line->vector3d (line3d)
  (with-line2d ,line2d
    (sx sy sz ex ey ez)
    (values (- ex sx) (- ey sy) (- sz ez))))
