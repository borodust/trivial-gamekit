(cl:defpackage :trivial-gamekit.distribution
  (:nicknames :gamekit.distribution)
  (:use :cl)
  (:export deliver))
(cl:in-package :trivial-gamekit.distribution)


(defun deliver (system-name game-class &key build-directory (zip ge.dist:*zip*) (lisp ge.dist:*lisp*))
  "Builds an executable, serializes resources and packs required foreign libraries into a .zip
archive for distribution. `system-name` is a name of `asdf` system of your application and
`game-class` is a game class defined with [`defgame`](#gamekit-defgame) (the one that could be
passed to [`#'start`](#gamekit-start) to start your game). By default, it builds all artifacts
into `build/` directory relative to `system-name` system path, but you can pass any other path
to `:build-directory` key argument to put target files into it instead.

This routine uses `zip` and `lisp` ('sbcl' [Steel Bank Common Lisp](http://sbcl.org) is the
default) to build a distributable package on various platforms. If those executables are not on
your system's `PATH`, you would need to provide absolute paths to them via `:zip` and `:lisp`
key arguments accordingly.

You can load this function into an image via `:trivial-gamekit/distribution` system.

Example:
```common_lisp
\(ql:quickload :trivial-gamekit/distribution)
\(gamekit.distribution:deliver :example-asdf-system 'example-package::example
                              :build-directory \"/tmp/example-game/\"
                              :zip \"/usr/bin/zip\"
                              :lisp \"/usr/bin/sbcl\")
```"
  (apply #'trivial-gamekit::%mount-resources (trivial-gamekit::list-all-resources))
  (let ((game-class-package (make-symbol (package-name (symbol-package game-class))))
        (game-class-name (make-symbol (symbol-name game-class)))
        (ge.dist:*zip* zip)
        (ge.dist:*lisp* lisp))
    (ge.dist:register-distribution system-name "trivial-gamekit::main"
                                   :asset-containers '(("/_gamekit/" "gamekit.brf")
                                                       ("/_asset/" "assets.brf"))
                                   :epilogue (asdf:system-relative-pathname :trivial-gamekit
                                                                            "epilogue.lisp")
                                   :bindings (list
                                              (cons '*gamekit-game-class*
                                                    ``(,',game-class-package ,',game-class-name)))))
  (ge.dist:make-distribution system-name :build-directory build-directory))
