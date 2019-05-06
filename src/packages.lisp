(bodge-util:define-package :trivial-gamekit
  (:nicknames :gamekit)
  (:use :cl :bodge-util :cl-bodge.engine)
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
  (:reexport-from :cl-bodge.appkit
                  draw
                  stop)
  (:export vec2
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
           start
           gamekit
           act
           push-action

           register-resource-package
           define-image
           define-sound
           define-font
           prepare-resources
           notice-resources
           make-font
           calc-text-bounds

           initialize-host
           initialize-graphics
           initialize-audio
           post-initialize
           pre-destroy

           viewport-width
           viewport-height

           bind-button
           bind-any-button
           bind-cursor

           draw-text
           draw-image

           image-width
           image-height

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
