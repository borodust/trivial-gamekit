(in-package :trivial-gamekit.def)


(defpackage :trivial-gamekit
  (:use :cl :ge :ge.util)
  (:export start
           stop

           gamekit-system
           gamekit
           print-text
           draw
           bind-button
           bind-cursor))
