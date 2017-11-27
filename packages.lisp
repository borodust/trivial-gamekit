(in-package :trivial-gamekit.def)


(defpackage :trivial-gamekit
  (:nicknames :gamekit)
  (:use :cl :cl-bodge.graphics :cl-bodge.audio :cl-bodge.engine
        :cl-bodge.utils :cl-bodge.resources
        :cl-bodge.host :cl-bodge.assets :cl-bodge.text)
  (:import-from :cl-bodge.canvas
                draw-line
                draw-curve
                draw-rect
                draw-circle
                draw-ellipse
                draw-arc
                draw-polygon
                draw-polyline
                translate-canvas
                rotate-canvas
                scale-canvas
                with-pushed-canvas)
  (:export start
           stop

           vec2
           vec3
           vec4
           mult
           add
           subt
           div
           normalize
           cross-product
           dot-product
           lerp
           x
           y
           z
           w

           defgame
           gamekit
           act
           draw

           register-resource-package
           define-image
           define-sound
           define-font
           prepare-resources
           notice-resources
           make-font

           initialize-host
           initialize-graphics
           initialize-audio
           post-initialize

           bind-button
           bind-cursor

           draw-text
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

           play-sound
           stop-sound

           ;; deprecated
           initialize-resources
           import-image
           import-sound
           gamekit-system
           print-text
           play))
