This is CL-TUPLES, an experimental "type facade" which appears to give you configurable homogenous
vectors based on defstruct, and allow for free conversion between multiple values and vectors. As
such it is to be hoped that it will be of most use when writing code for raytracers, games, and
the like, which use simple linear algebra primitives intensely.

The rule is that if a symbol ends in an asterisk the function or macro bound to it will operate
on or return multiple values, and the unadorned symbols will work with vectors.

# HOWTO

A two-dimensional vector value is created by `MAKE-VECTOR2D`:

    > (make-vector2d 1f0 1f0)
    #(1.0 1.0)

The type `FAST-FLOAT`, which is used for all float values, is actually a
subtype of `SINGLE-FLOAT`, so make sure to only use values that fit into
that type.

To calculate the length of this vector `VECTOR2D-LENGTH*` can now be
used like this:

    > (let ((v (make-vector2d 1f0 1f0)))
        (vector2d-length* (vector2d* v)))
    1.4142135

By converting the object into a bunch of variables, the macro pipeline
keeps transient objects and function calls away.  The above form thus
expands to something like the following (type declarations and some
other code omitted for clarity):

    (LET ((V (MAKE-VECTOR2D 1.0 1.0)))
      (MULTIPLE-VALUE-BIND (#:G1764 #:G1765)
          (VALUES (AREF V 0) (AREF V 1))
        (SYMBOL-MACROLET ((X #:G1764) (Y #:G1765))
          (SQRT (+ (* X X) (* Y Y))))))

The coordinates of the vector are bound and made available to the length
calculation code.  If we skip the object creation and go straight the
`VALUES` route, the following is approximately the same as above,
without ever creating a vector object.

    > (vector2d-length* (vector2d-values 1.0 1.0))
    1.4142135

The reader syntax may be used to the same effect:

    > #{1.0 1.0}
    1.0
    1.0
    > (vector2d-length* #{1.0 1.0})
    1.4142135

Based on this design more operations are implemented.  See the API and
the tests for details on vectors, vertexes, matrixes and quaternions.

Defining new operators is done via `DEF-TUPLE-OP`, e.g.:

    (def-tuple-op scaling-matrix44*
        ((sx cl-tuples::fast-float)
         (sy cl-tuples::fast-float)
         (sz cl-tuples::fast-float))
      (:return matrix44
               (matrix44-values*
                sx    0.0f0 0.0f0 0.0f0
                0.0f0 sy    0.0f0 0.0f0
                0.0f0 0.0f0 sz    0.0f0
                0.0f0 0.0f0 0.0f0 1.0f0)))

This operator accepts three arguments and creates the obvious matrix
from them.  So lets say, a function has as a conventional argument a
vector of three elements.  Binding each element to a name and applying
the above operator to them gives us the following:

    > (let ((v (make-vector3d* #{1f0 1f0 1f0))))
        (with-vector3d v (sx sy sz)
          (make-matrix44* (scaling-matrix44* sx sy sz)))
    #(1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0)

The calculated matrix is converted to an actual object to be returned.

# ASSORTED EXAMPLES

    > (let ((v (make-vector2d 1f0 1f0))
            (s 2f0))
        (vector2d-length* (vector2d-scale* (vector2d* v) s)))
    2.828427
