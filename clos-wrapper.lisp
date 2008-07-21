

;; (def-tuples-class thing
;;     ((pos :vector3d)
;;      (verts :vector3d 78)))


;; .. expands to

;; (def-class thing  ()
;;     ((pos :type (values ..) :accessor %pos-of)
;;      (verts :type (simple-array )) :accessor %verts-of))

;; (defmethod pos-of (thing)
;;   (vector3d (%pos-of thing)))

;; (defmethod (setf pos-of (thing) values)
;;     (setf)
;;     )
