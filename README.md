# trivial-gamekit

Library for getting into gamedev with Common Lisp! Very simple interface to graphics, audio and input.


## Requirements

* OpenGL 2.1 or 3.3+
* 64-bit (x86_64) Windows, GNU/Linux or macOS
* x86_64 SBCL or CCL


## Installation and loading

By default, `trivial-gamekit` works in OpenGL 3.3 mode. To enable OpenGL 2.1 you need to
```lisp
(pushnew :bodge-gl2 *features*)
```

### Via Quicklisp distribution
```lisp
;; add cl-bodge distribution into quicklisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")

;; load the gamekit
(ql:quickload :trivial-gamekit)
```

### Via bodge-projects collection and Quicklisp's local projects

```sh
export BODGE_PROJECTS_DIR=<desired-path-to-bodge-projects>
# Quicklisp might be installed in a different place
export QUICKLISP_LOCAL_PROJECTS_DIR=~/quicklisp/local-projects

git clone https://github.com/borodust/bodge-projects "$BODGE_PROJECTS_DIR"
cd "$BODGE_PROJECTS_DIR" && git submodule init && git submodule update

ln -s "$BODGE_PROJECTS_DIR" "$QUICKLISP_LOCAL_PROJECTS_DIR/bodge-projects"
```


## Example

Copy-paste these into your Common Lisp REPL after loading `trivial-gamekit`:

```lisp
(gamekit:defgame example () ())

(defmethod gamekit:draw ((this example))
  (gamekit:draw-text "Hello, Gamedev!" (gamekit:vec2 240.0 240.0)))

(gamekit:start 'example)
```


## Documentation

See `trivial-gamekit` external [documentation](https://borodust.org/projects/trivial-gamekit/).


## Help

`#lispgames` or [`#cl-bodge`](https://web.libera.chat/gamja/?channel=#cl-bodge) at `irc.libera.chat:6697`
