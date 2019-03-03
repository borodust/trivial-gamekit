(asdf:defsystem trivial-gamekit
  :description "Simple facade for cl-bodge functionality"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (log4cl bodge-utilities
                      cl-bodge/graphics
                      cl-bodge/audio
                      cl-bodge/host
                      cl-bodge/resources
                      cl-bodge/canvas
                      cl-bodge/appkit
                      uiop cl-muth cl-fad cl-muth)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "resources")
               (:file "gamekit")))


(asdf:defsystem trivial-gamekit/distribution
  :description "Distribution facilities for trivial-gamekit"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (trivial-gamekit cl-bodge/distribution)
  :pathname "distrib/"
  :serial t
  :components ((:file "distribution")))


(asdf:defsystem trivial-gamekit/documentation
  :description "Documentation for trivial-gamekit"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (doxy alexandria cl-mustache trivial-gamekit trivial-gamekit/distribution trivial-docstring)
  :pathname "docs/"
  :serial t
  :components ((:file "packages")
               (:file "bodge")
               (:file "gamekit")
               (:file "renderer")))
