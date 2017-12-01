(cl:in-package :cl-user)

(defpackage :trivial-gamekit.def
  (:use :cl :asdf))

(in-package :trivial-gamekit.def)


(defsystem trivial-gamekit
  :description "Simple facade for cl-bodge functionality"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (log4cl cl-bodge/graphics cl-bodge/audio cl-bodge/host
                      cl-bodge/resources cl-bodge/canvas cl-bodge/assets
                      cl-bodge/text uiop cl-muth cl-fad cl-muth)
  :serial t
  :components ((:file "packages")
               (:file "resources")
               (:file "gamekit")))


(defsystem trivial-gamekit/distribution
  :description "Distribution facilities for trivial-gamekit"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (trivial-gamekit cl-bodge/distribution)
  :serial t
  :components ((:file "distribution")))


(defsystem trivial-gamekit/documentation
  :description "Documentation for trivial-gamekit"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (doxy alexandria cl-mustache trivial-gamekit trivial-gamekit/distribution)
  :serial t
  :components ((:file "documentation")))
