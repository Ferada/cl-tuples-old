
A tuple type declaration auto-generates a number of useful functions, macros, and types. 

It is best to give an example.

(def-tuple-type vector2d
				:tuple-element-type short-float
				:initial-element 0.0f0
				:elements (x y))

Will declare a tuple of short-floats, arrays of which are initialised
with the element 0.0f0 and which has two elements, named x and y.

There will be a struct to represent this type, declared as follows:

(defstruct vector2d
		   :type vector
		   :constructor nil
		   (x 0.0f0 :type short-float)
		   (y 0.0f0 :type short-float))

i.e. a struct, stored as a vector with elements representing the
elements of the tuple, initialised to the initial-element value of the
tuple.

The following routines will be generated to be able to manipulate them.

(vector2d-values* x y) => (values x y)          ;; convert from args to values
(vector2d* v) => (values (aref v 0) (aref v 1)) ;; covert from array to values
(new-vector2d)                                  ;; returns an empty tuple vector- i.e. #( 0 0 )
(make-vector2d x y)                             ;; returns a vector (struct) as #( x y )
(make-vector2d* (values x y))                   ;; same as the above only with multiple value arguments
(setf (vector2d* v) (values x y) )              ;; generalised set that takes multiple values
(with-vector2d v (i j) ...)                     ;; binds x and y of tuple vector v to i and j in the body
(with-vector2d* (values x y) (i j) ..)          ;; same as the above, only it expects a values form

;; arrays -- this can create an array of n vector2ds (eg 4 vector2ds == 8 element array)
(make-vector2d-array dimensons &key adjustable fill-pointer)

(vector2d-aref v  n)                            ;; treats v as an array of n vector2d's and returns the nth vector2d as a vector (ie struct)
(vector2d-aref* v n)                            ;; treats v as an array of n vector2d's and returns the nth vector2d as multiple values
(setf (vector2d-aref v n) #( x y ))             ;; sets the n'tn vector2d in the array v
(setf (vector2d-aref v n) (values x y ))        ;; sets the n'tn vector2d in the array v, expects multiple value argument
 
(vector2d-push #( x y ) v)                      ;; push an vector2d into an array of vector2d
(vector2d-push*  (values x y) v)                ;; same as above but with multiple values
(vector2d-push-extend #( x y ) v)               ;; as vector2d-push but admits the possiblity of extension
(vector2d-push-extend* (values x y) v)          ;; same as above but takes multiple value arguments

(vector2d-fill-pointer v)                       ;; returns fill pointer 
(setf (vector2d-fill-pointer v) x)              ;; sets fill pointer
(vector2d-array-dimensions v)                   ;; returns number of vector2d's array can hold

In addition a small convienince reader syntax is implemented - #{ x y z } is equivalent to (values x y z) as client
code of this library is likely to manipulate many multiple values.
