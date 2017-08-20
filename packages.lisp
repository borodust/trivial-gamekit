(in-package :trivial-gamekit.def)


(defpackage :trivial-gamekit
  (:nicknames :gamekit)
  (:use :cl :ge :ge.util)
  (:export start
           stop

           vec2
           vec3
           vec4
           mult
           add
           subt
           div
           x
           y
           z
           w

           gamekit-system
           gamekit
           draw

           initialize-resources
           import-image
           import-sound

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
           draw-polyline
           draw-image

           play))
