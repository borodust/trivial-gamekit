(cl:defpackage :trivial-gamekit.distribution
  (:nicknames :gamekit.distribution)
  (:use :cl)
  (:export descriptor))
(cl:in-package :trivial-gamekit.distribution)


(defmacro descriptor (system game-class)
  `(progn
     (ge.dist:descriptor ,system
       :entry-function (:trivial-gamekit main)
       :asset-containers (("/_gamekit/" "gamekit.brf")
                          ("/_asset/" "assets.brf"))
       :epilogue ,(asdf:system-relative-pathname :trivial-gamekit "epilogue.lisp")
       :bind ((*gamekit-game-class* ''(,(make-symbol (package-name (symbol-package game-class)))
                                       ,(make-symbol (symbol-name game-class))))))
     (defmethod ge.dist:configure-system ((system (eql ,system)))
       (trivial-gamekit::mount-all-resources ',game-class))))
