(in-package :trivial-gamekit.def)


(defpackage :trivial-gamekit
  (:nicknames :gamekit)
  (:use :cl :ge :ge.util)
  (:export start
           stop

           gamekit-system
           gamekit
           draw

           bind-button
           bind-cursor

           print-text
           draw-line
           draw-curve
           draw-rect
           draw-circle
           draw-ellipse
           draw-arc
           draw-polygon
           draw-polyline))
