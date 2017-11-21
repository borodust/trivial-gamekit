(cl:defpackage :trivial-gamekit.distribution
  (:nicknames :gamekit.distribution)
  (:use :cl)
  (:export deliver))
(cl:in-package :trivial-gamekit.distribution)


(defun deliver (system-name game-class &key build-directory)
  (apply #'trivial-gamekit::%mount-resources (trivial-gamekit::list-all-resources))
  (let ((game-class-package (make-symbol (package-name (symbol-package game-class))))
        (game-class-name (make-symbol (symbol-name game-class))))
    (ge.dist:register-distribution system-name "trivial-gamekit::main"
                                   :asset-containers '(("/_gamekit/" "gamekit.brf")
                                                       ("/_asset/" "assets.brf"))
                                   :epilogue (asdf:system-relative-pathname :trivial-gamekit
                                                                            "epilogue.lisp")
                                   :bindings (list
                                              (cons '*gamekit-game-class*
                                                    ``(,',game-class-package ,',game-class-name)))))
  (ge.dist:make-distribution system-name :build-directory build-directory))
