# trivial-gamekit

Simplified interface to [cl-bodge](https://github.com/borodust/cl-bodge) facilities.


## Requirements

* OpenGL 4.1+
* 64-bit (x86_64) Windows, GNU/Linux or macOS
* x86_64 SBCL or CCL


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
  (print-text "Hello, Gamedev!" 240.0 240.0))

(start 'example)
```


## Documentation

See `trivial-gamekit` [wiki](https://github.com/borodust/trivial-gamekit/wiki).


## Help

`#lispgames` at `irc://chat.freenode.net`