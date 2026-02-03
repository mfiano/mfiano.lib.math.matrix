# mfiano.lib.math.matrix : A fast 2D/3D matrix math library

This project provides four sibling packages:

* `mfiano.lib.math.matrix.matrix2.f32`: single-float 2x2 matrix math
* `mfiano.lib.math.matrix.matrix2.f64`: double-float 2x2 matrix math
* `mfiano.lib.math.matrix.matrix3.f32`: single-float 3x3 matrix math
* `mfiano.lib.math.matrix.matrix3.f64`: double-float 3x3 matrix math

## Using it

Pick the precision you want and use the package directly.

Example:

```lisp
(mfiano.lib.math.matrix.matrix2.f32:new 0f0 1f0 2f0 3f0)
;; => #(0.0 1.0 2.0 3.0)

(mfiano.lib.math.matrix.matrix2.f64:new 0d0 1d0 2d0 3d0)
;; => #(0.0d0 1.0d0 2.0d0 3.0d0)

(mfiano.lib.math.matrix.matrix3.f32:new 0f0 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)
;; => #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0)

(mfiano.lib.math.matrix.matrix3.f64:new 0d0 1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0)
;; => #(0.0d0 1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0 7.0d0 8.0d0)
```

Note: This project is a work in progress. It is only used by a few in-house projects and does not include, or test, everything I would like yet.

## Design philosophy

This code is optimized for:

* small, composable math routines with both mutating and consing variants (where applicable)
* predictable names and signatures
* game/graphics ergonomics over math library completeness

## License

Licensed under the MIT License.
