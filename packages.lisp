(in-package :trivial-gamekit.def)


(defpackage :trivial-gamekit
  (:nicknames :gamekit)
  (:use :cl :cl-bodge.graphics :cl-bodge.audio :cl-bodge.engine
        :cl-bodge.utils :cl-bodge.resources :cl-bodge.canvas
        :cl-bodge.host :cl-bodge.assets :cl-bodge.text)
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
           defgame
           gamekit
           act
           draw

           define-image
           define-sound
           initialize-resources
           import-image
           import-sound
           prepare-resources
           notice-resources

           initialize-host
           initialize-graphics
           initialize-audio
           post-initialize

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

           translate-canvas
           rotate-canvas
           scale-canvas
           with-pushed-canvas

           play
           play-sound
           stop-sound))
