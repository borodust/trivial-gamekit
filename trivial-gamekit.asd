(cl:in-package :cl-user)

(defpackage :trivial-gamekit.def
  (:use :cl :asdf))

(in-package :trivial-gamekit.def)


(defsystem trivial-gamekit
  :description "Simple facade for cl-bodge functionality"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "The Unlicense"
  :depends-on (log4cl cl-bodge/graphics cl-bodge/audio cl-bodge/host
                      cl-bodge/resources cl-bodge/canvas cl-bodge/assets
                      cl-bodge/text uiop)
  :serial t
  :components ((:file "packages")
               (:file "resources")
               (:file "gamekit")))
