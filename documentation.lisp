(cl:defpackage :trivial-gamekit.documentation
  (:nicknames :gamekit.documentation)
  (:use :cl :gamekit :gamekit.distribution)
  (:export render-documentation))
(cl:in-package :trivial-gamekit.documentation)


(defmacro docstring (object &body docstring)
  `(setf (documentation ,object t)
         ,@docstring))


#|
(docstring #'
  "

Example:
```common_lisp
```")
|#

(docstring #'vec2
  "Makes a two-dimensional vector.

Example:
```common_lisp
(gamekit:vec2 0 0)
```")

(docstring #'vec3
  "Makes a three-dimensional vector.

Example:
```common_lisp
(gamekit:vec3 1 1 2)
```")

(docstring #'vec4
  "Makes a four-dimensional vector.

Example:
```common_lisp
(gamekit:vec4 1 1 2 3)
```")

(docstring #'x
  "Reads first element of a vector.

Example:
```common_lisp
(gamekit:x (gamekit:vec2 1 1))
```")

(docstring #'(setf x)
  "Stores first element of a vector.

Example:
```common_lisp
(setf (gamekit:x (gamekit:vec2 1 1)) 0)
```")

(docstring #'y
  "Reads second element of a vector.

Example:
```common_lisp
(gamekit:y (gamekit:vec2 1 1))
```")

(docstring #'(setf y)
  "Stores second element of a vector.

Example:
```common_lisp
(setf (gamekit:y (gamekit:vec2 1 1)) 0)
```")

(docstring #'z
  "Reads third element of a vector.

Example:
```common_lisp
(gamekit:z (gamekit:vec4 1 1 2 3))
```")

(docstring #'(setf z)
  "Stores third element of a vector.

Example:
```common_lisp
(setf (gamekit:z (gamekit:vec4 1 1 2 3)) 0)
```")

(docstring #'w
  "Reads fourth element of a vector.

Example:
```common_lisp
(gamekit:w (gamekit:vec4 1 1 2 3))
```")

(docstring #'(setf w)
  "Stores fourth element of a vector.

Example:
```common_lisp
(setf (gamekit:w (gamekit:vec4 1 1 2 3)) 0)
```")

(docstring #'mult
  "Element-wise multiplication. Accepts both vectors and scalars.

Example:
```common_lisp
(gamekit:mult 2 (gamekit:vec2 1 1) 0.5)
```")

(docstring #'add
  "Element-wise addition. Accepts both vectors and scalars.

Example:
```common_lisp
(gamekit:add 1 (gamekit:vec2 1 1) -1)
```")

(docstring #'subt
  "Element-wise subtraction. Accepts both vectors and scalars.

Example:
```common_lisp
(gamekit:subt 1 (gamekit:vec2 1 1) (gamekit:vec2 -1 -1))
```")

(docstring #'div
  "Element-wise division. Accepts both vectors and scalars.

Example:
```common_lisp
(gamekit:div (gamekit:vec2 1 1) 2 (gamekit:vec2 0.5 0.5))
```")

(docstring #'draw-text
  "Draws text on the canvas starting at coordinates passed as second argument.
Use `:fill-color` key parameter to change text's color. To change a font, pass object created
with [`#'make-font`](#gamekit-make-font) via `:font` parameter.

Example:
```common_lisp
(gamekit:draw-text \"Hello, Gamekit!\" (gamekit:vec2 11 23)
                   :fill-color (gamekit:vec4 0 0 0 1)
                   :font (gamekit:make-font 'example-package::noto-sans 32))
```")

(docstring #'draw-line
  "Draws a line starting from coordinates passed as first argument to coordinates in second
parameter. Third parameter is a color to draw a line with. `:thickness` is a scalar floating
point value controlling pixel-width of a line.

Example:
```common_lisp
(gamekit:draw-line (gamekit:vec2 8 5) (gamekit:vec2 32 11)
                   (gamekit:vec4 1 0.5 0 1)
                   :thickness 1.5)
```")

(docstring #'draw-curve
  "Draws a bezier curve from coordinates passed as first argument to coordinates in second
parameter with two control points in third and fourth parameters accordingly. Fifth argument is
a curve's color. `:thickness` is a scalar floating point value controlling pixel-width of a
curve.

Example:
```common_lisp
(gamekit:draw-line (gamekit:vec2 8 5) (gamekit:vec2 32 11)
                   (gamekit:vec2 0 5) (gamekit:vec2 32 0)
                   (gamekit:vec4 1 0.5 0 1)
                   :thickness 1.5)
```")

(docstring #'draw-rect
  "Draws a rectangle with origin passed in first argument, width and height - second and third
arguments accordingly. `:fill-paint` key is a color to fill insides of a rectangle with. If you
pass color to `:stroke-paint`, edges of the rectangle are going to be struck with
it. `:thickness` controls pixel width of struck edges. Use `:rounding` in pixels to round
rectangle corners.

Example:
```common_lisp
(gamekit:draw-rect (gamekit:vec2 0 0) 314 271
                   :fill-paint (gamekit:vec4 1 0.75 0.5 0.5)
                   :stroke-paint (gamekit:vec4 0 0 0 1)
                   :rounding 5.0)
```")

(docstring #'draw-circle
  "Draws a circle with center in first argument and radius in second argument.
Provide color with `:fill-paint` paramater to fill the inner area of the circle with. If
`:stroke-paint` color is provided, circle's border is going to be struck with it. `:thickness`
controls pixel width of struck border.

Example:
```common_lisp
(gamekit:draw-circle (gamekit:vec2 100 500) 3/4
                     :fill-paint (gamekit:vec4 1 1 1 1)
                     :stroke-paint (gamekit:vec4 0 0 0 1)
                     :thickness 3)
```")

(docstring #'draw-ellipse
  "Draws an ellipse with center provided in first argument, x and y radii as second and thrid
arguments accordingly. Pass a color as `:fill-paint` paramater to fill the inner area of the
ellipse with. If `:stroke-paint` color is provided, ellipse's border will be struck with
it. `:thickness` controls pixel width of struck border.

Example:
```common_lisp
(gamekit:draw-ellipse (gamekit:vec2 128 128) 16 32
                      :fill-paint (gamekit:vec4 0 0 0 1)
                      :stroke-paint (gamekit:vec4 1 1 1 1)
                      :thickness 1.1)
```")

(docstring #'draw-arc
  "Draws an arc from `a0` to `a1` angles (in radians) with center passed in first argument and
radius in second. If provided, color in `:fill-paint` will be used to fill the area under an arc
confined between a circle's curve and a line connecting angle points. `:fill-paint` and
`:stroke-paint` colors are, if provided, used to fill insides and stroke arc's border
correspondingly.

Example:
```common_lisp
(gamekit:draw-arc (gamekit:vec2 256 256) 128
                  (/ pi 4) (* (/ pi 2) 1.5)
                  :fill-paint (gamekit:vec4 0.25 0.5 0.75 1)
                  :stroke-paint (gamekit:vec4 0.75 0.5 0.25 1)
                  :thickness 2.0)
```")

(docstring #'draw-polygon
  "Draws a polygon connecting list of vertices provided in first argument. `:fill-paint` is
a color to fill insides of a polygon and `:stroke-paint` color is used to stroke polygon
edges. `:thickness` controls pixel-width of a stroke.

Example:
```common_lisp
(gamekit:draw-polygon (list (gamekit:vec2 10 10) (gamekit:vec2 20 20)
                            (gamekit:vec2 30 20) (gamekit:vec2 20 10))
                      :fill-paint (gamekit:vec4 0.25 0.5 0.75 1)
                      :stroke-paint (gamekit:vec4 0.75 0.5 0.25 1)
                      :thickness 3.0)
```")

(docstring #'draw-polyline
  "Draws a polyline connecting list of vertices provided in first argument. Second argument is a
color to stroke a line with. `:thickness` controls pixel width of a line.

Example:
```common_lisp
(gamekit:draw-polyline (list (gamekit:vec2 10 10) (gamekit:vec2 20 20)
                             (gamekit:vec2 30 20) (gamekit:vec2 20 10))
                       (gamekit:vec4 0.75 0.5 0.25 1)
                       :thickness 3.0)
```")

(docstring #'draw-image
  "Draws an image at coordinates specified in first argument. Second argument is `image-id` used
in [`#'define-image`](#gamekit-define-image) earlier. Optional `:origin` key is a point within
image to start drawing from, if you want to render only a part of image. `:width` and `:height`
keys tell width and height of a subimage to draw. They are optional and could be skipped to draw
a subimage with full height and width available.

Example:
```common_lisp
(gamekit:draw-image (gamekit:vec2 314 271) 'example-package::logo
                    :origin (gamekit:vec2 0 0)
                    :width 320
                    :height 240)
```")

(docstring #'translate-canvas
  "Moves drawing origin to the specified position making the latter a new origin. All following
draw operations will be affected by this change unless wrapped with
[`with-pushed-canvas`](#gamekit-with-pushed-canvas) macro.

Example:
```common_lisp
(gamekit:translate-canvas 100 500)
```")

(docstring #'rotate-canvas
  "Rotates current canvas for specified number of radians. All following drawing operations will
be affected by this change unless wrapped with
[`with-pushed-canvas`](#gamekit-with-pushed-canvas) macro.

Example:
```common_lisp
(gamekit:rotate-canvas (/ pi 2))
```")

(docstring #'scale-canvas
  "Scales current canvas by x and y axes accordingly. All following drawing operations will be
affected by this change unless wrapped with [`with-pushed-canvas`](#gamekit-with-pushed-canvas)
macro.

Example:
```common_lisp
(gamekit:scale-canvas 0.5 1.5)
```")

(docstring (macro-function 'with-pushed-canvas)
  "Saves current canvas transformations (translations, rotations, scales) before entering its
body and restores upon exist from the body. All transformation operations inside this macro
don't affect outer canvas outside of a body of this macro.

Example:
```common_lisp
(gamekit:translate-canvas 400 300)
(gamekit:with-pushed-canvas ()
  (gamekit:rotate-canvas (/ pi 4)))
```")


(defclass kramdown-renderer () ())


(defparameter *template* (alexandria:read-file-into-string
                          (asdf:system-relative-pathname :trivial-gamekit/documentation
                                                         "doc-entry.template")))

(defun format-name (name)
  (if (symbolp name)
      (string-downcase (symbol-name name))
      (format nil "~(~A~)"
              (mapcar #'symbol-name name))))


(defun format-link (name)
  (format nil "#gamekit-~(~{~A~^-~}~)" (alexandria:ensure-list name)))


(defun format-documentation-entry (type name documentation &optional lambda-list)
  (let ((mustache:*escape-tokens* nil))
    (mustache:render* *template* (alexandria:plist-alist
                                  (append (list :type type
                                                :name (format-name name)
                                                :link (format-link name))
                                          (when documentation
                                            (list :documentation documentation))
                                          (list :lambda-list
                                                (format nil "(~{~(~A~)~^ ~})" lambda-list)))))))


(defmethod doxy:document-class ((this kramdown-renderer) name docstring)
  nil)

(defmethod doxy:document-function ((this kramdown-renderer) name lambda-list docstring)
  (format-documentation-entry "function" name docstring lambda-list))

(defmethod doxy:document-macro ((this kramdown-renderer) name lambda-list docstring)
  (format-documentation-entry "macro" name docstring lambda-list))

(defmethod doxy:document-generic ((this kramdown-renderer) name lambda-list docstring)
  (format-documentation-entry "generic" name docstring lambda-list))

(defmethod doxy:document-variable ((this kramdown-renderer) name docstring)
  (format-documentation-entry "variable" name docstring))


(defun render-documentation-and-collect-index (renderer output-directory exists-action)
  (flet ((%render-documentation (file &rest symbols)
           (alexandria:with-output-to-file (output (merge-pathnames file output-directory)
                                                   :if-exists exists-action)
             (loop for (symbol . doc) in (apply #'doxy:collect-documentation renderer symbols)
                when doc
                do (format output "~A~&~%" doc)
                and
                collect (format nil "* [~(~A~)](~A)"
                                (format-name symbol)
                                (format-link symbol))))))
    (append
     (%render-documentation "defining-a-game.md"
                            'defgame
                            'start
                            'stop
                            'gamekit
                            'post-initialize
                            'pre-destroy
                            'act
                            'draw)
     (%render-documentation "math.md"
                            'vec2
                            'vec3
                            'vec4
                            'mult
                            'add
                            'subt
                            'div
                            'x
                            'y
                            'z
                            'w)
     (%render-documentation "locating-resources.md"
                            'register-resource-package
                            'define-image
                            'define-sound
                            'define-font
                            'make-font
                            'prepare-resources
                            'notice-resources)
     (%render-documentation "drawing.md"
                            'draw-line
                            'draw-curve
                            'draw-rect
                            'draw-circle
                            'draw-ellipse
                            'draw-arc
                            'draw-polygon
                            'draw-polyline
                            'draw-image
                            'draw-text
                            'translate-canvas
                            'rotate-canvas
                            'scale-canvas
                            'with-pushed-canvas)
     (%render-documentation "playing-an-audio.md"
                            'play-sound
                            'stop-sound)
     (%render-documentation "listening-to-input.md"
                            'bind-button
                            'bind-cursor)
     (%render-documentation "building-a-distributable.md"
                            'deliver))))


(defun render-documentation (&key (overwrite t)
                               (output-directory (asdf:system-relative-pathname
                                                  :trivial-gamekit/documentation "docs/")))
  (let ((renderer (make-instance 'kramdown-renderer))
        (exists-action (if overwrite :supersede :error)))
    (log:info "Rendering documentation into '~A'" output-directory)
    (ensure-directories-exist output-directory)
    (let ((index (render-documentation-and-collect-index renderer output-directory exists-action)))
      (alexandria:with-output-to-file (output (merge-pathnames output-directory "symbol-index.md")
                                              :if-exists exists-action)
        (loop for entry in index
           do (format output "~A~%" entry))))))
