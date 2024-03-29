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
           cross
           dot
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
           define-text
           define-binary
           prepare-resources
           dispose-resources
           notice-resources
           make-font
           calc-text-bounds

           post-initialize
           pre-destroy

           viewport-width
           viewport-height
           canvas-width
           canvas-height

           bind-button
           bind-any-button
           bind-cursor
           bind-scroll
           bind-any-gamepad
           bind-gamepad-button
           bind-gamepad-any-button
           bind-gamepad-dpad
           bind-gamepad-stick
           bind-gamepad-trigger

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

           get-text
           get-binary

           ;; deprecated
           initialize-host
           initialize-graphics
           initialize-audio
           initialize-resources

           import-image
           import-sound
           gamekit-system
           print-text
           play))
