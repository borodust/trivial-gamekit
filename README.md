# trivial-gamekit

Library for getting into gamedev with Common Lisp! Very simple interface to graphics, audio and input.


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

(defclass example (gamekit:gamekit-system) ())

(defmethod gamekit:draw ((this example))
  (gamekit:print-text "Hello, Gamedev!" 240.0 240.0))

(gamekit:start 'example)
```


## Documentation

See `trivial-gamekit` external [documentation](https://github.com/borodust/trivial-gamekit/wiki).


## Help

`#lispgames` at `irc://chat.freenode.net`
