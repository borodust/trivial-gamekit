(cl:defpackage :trivial-gamekit.distribution
  (:nicknames :gamekit.distribution)
  (:use :cl)
  (:export deliver))
(cl:in-package :trivial-gamekit.distribution)


(defun deliver (system-name game-class &key build-directory (zip ge.dist:*zip*) (lisp ge.dist:*lisp*))

  (let ((game-class-instance (find-class game-class nil)))
    (unless game-class-instance
      (error "Class with name ~A not found" game-class))
    (unless (subtypep game-class-instance 'gamekit::gamekit-system)
      (error "~A is not a gamekit instance class" game-class)))
  (apply #'trivial-gamekit::%mount-resources (trivial-gamekit::list-all-resources))
  (let ((game-class-package (make-symbol (package-name (symbol-package game-class))))
        (game-class-name (make-symbol (symbol-name game-class)))
        (ge.dist:*zip* zip)
        (ge.dist:*lisp* lisp))
    (ge.dist:register-distribution system-name "trivial-gamekit::main"
                                   :asset-containers '(("/_gamekit/" "gamekit.brf")
                                                       ("/_asset/" "assets.brf"))
                                   :epilogue (asdf:system-relative-pathname :trivial-gamekit
                                                                            "distrib/epilogue.lisp")
                                   :bindings (list
                                              (cons '*gamekit-game-class*
                                                    ``(,',game-class-package ,',game-class-name))))
    (ge.dist:make-distribution system-name :build-directory build-directory)))
