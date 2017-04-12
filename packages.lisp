(in-package :trivial-gamekit.def)


(defpackage :trivial-gamekit
  (:use :cl :ge :ge.util)
  (:export start
           stop

           gamekit-system
           print-text
           simulate
           draw
           blare
           bind-key))
