(bodge-util:define-package :trivial-gamekit
  (:nicknames :gamekit)
  (:use :cl :cl-bodge.graphics :cl-bodge.audio :cl-bodge.engine
        :bodge-util :cl-bodge.resources :cl-bodge.host)
  (:reexport-from :cl-bodge.canvas
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
             scale-canvas)
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
           push-action

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
           pre-destroy

           bind-button
           bind-cursor

           draw-text
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
