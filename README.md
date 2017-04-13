# trivial-gamekit

Simplified interface to cl-bodge facilities.

## Installation and loading

```lisp
;; add cl-bodge distribution into quicklisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")

;; load precompiled native libraries and the gamekit
(ql:quickload '(:bodge-blobs :trivial-gamekit))
```


## Example
```lisp
(cl:defpackage :trivial-gamekit-example
  (:use :cl :ge :trivial-gamekit))
(cl:in-package :trivial-gamekit-example)


(defclass example (gamekit-system) ())

(defmethod draw ((this example))
  (print-text "Hello, Gamedev!" 10.0 10.0)
  (draw-rect (vec2 0 0) 205 42
             :stroke-paint (vec4 0.8 0.6 0.6 1.0)
             :rounding 5.0))

(start 'example)
```