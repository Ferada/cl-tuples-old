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

    > (enable-tuples-syntax)
    > #{1.0 1.0}
    1.0
    1.0
    > (vector2d-length* #{1.0 1.0})
    1.4142135

(Since the reader syntax and `VECTOR2D-VALUES` expand directly into a
`VALUES` call, nothing prevents you from using that as well.)

Based on this design more operations are implemented.  See the API and
the tests for details on vectors, vertexes, matrixes and quaternions.

Defining new operators is done via `DEF-TUPLE-OP`, e.g.:

    (def-tuple-op scaling-matrix44*
        ((sx fast-float)
         (sy fast-float)
         (sz fast-float))
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

# QUATERNIONS

(Adapted from the documentation of cl-quaternion to this API.)

Creating a quaternion from real and imaginary components.  The first
argument is the real part, and the rest are the imaginary components.

    > (make-quaternion* (quaternion-values* 10f0 3f0 0f0 0f0))
    #(10.0 3.0 0.0 0.0)

Quaternions can be normalized and magnitudes may be computed.

    > (make-quaternion* (quaternion-normalize* (quaternion* *)))
    #(0.9578263 0.28734788 0.0 0.0)
    > (quaternion-mag* (quaternion* *))
    1.0

Quaternion addition and multiplication are supported.

    > (make-quaternion*
       (quaternion-sum* (quaternion-values* 3f0 0f0 0f0 0f0)
                        (quaternion-values* 1f0 1f0 0f0 1f0)))
    #(4.0 1.0 0.0 1.0)
    > (make-quaternion*
       (quaternion-product* (quaternion-values* 3f0 0f0 0f0 0f0)
                            (quaternion-values* 1f0 1f0 0f0 1f0)))
    #(3.0 0.0 3.0 -3.0)

Unit quaternions may be used to represent rotations.  Functions are
provided for working with quaternions for this purpose.

    > (values fast-pi (type-of fast-pi))
    3.1415927
    SINGLE-FLOAT
    > (make-quaternion*
       (angle-axis-quaternion*
        (angle-axis-values* 0f0 0f0 1f0 (/ single-pi 2f0))))
    #(0.0 0.0 0.70710677 0.70710677)

Vectors can then be transformed using these quaternions.

    > (quaternion-transform-vector3d*
       (vector3d-values* 0.0 1.0 0.0)
       (angle-axis-quaternion*
        (angle-axis-values* 0.0 0.0 1.0 (/ fast-pi 2))))
    -0.99999994
    0.0
    0.0

At the moment you have still to convert an angle-axis representation to
either a matrix or a quaternion by yourself to rotate a vector by it.

    > (quaternion-transform-vector3d*
       (vector3d-values* 0.0 1.0 0.0)
       (angle-axis-quaternion*
        (angle-axis-values* 0.0 0.0 1.0 fast-pi)))
    8.742278e-8
    -1.0
    0.0
    > (transform-vector3d*
       (angle-axis-matrix33*
        (angle-axis-values* 0.0 0.0 1.0 fast-pi))
       (vector3d-values* 0.0 1.0 0.0))
    8.742278e-8
    -1.0
    0.0
