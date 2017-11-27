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
  "

Example:
```common_lisp
```")

(docstring #'draw-line
  "

Example:
```common_lisp
```")

(docstring #'draw-curve
  "

Example:
```common_lisp
```")

(docstring #'draw-rect
  "

Example:
```common_lisp
```")

(docstring #'draw-circle
  "

Example:
```common_lisp
```")

(docstring #'draw-ellipse
  "

Example:
```common_lisp
```")

(docstring #'draw-arc
  "

Example:
```common_lisp
```")

(docstring #'draw-polygon
  "

Example:
```common_lisp
```")

(docstring #'draw-polyline
  "

Example:
```common_lisp
```")

(docstring #'draw-image
  "

Example:
```common_lisp
```")

(docstring #'translate-canvas
  "

Example:
```common_lisp
```")

(docstring #'rotate-canvas
  "

Example:
```common_lisp
```")

(docstring #'scale-canvas
  "

Example:
```common_lisp
```")

(docstring #'with-pushed-canvas
  "

Example:
```common_lisp
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


(defun render-documentation (&key (overwrite t))
  (let ((renderer (make-instance 'kramdown-renderer))
        (output-directory (asdf:system-relative-pathname :trivial-gamekit/documentation "docs/"))
        (exists-action (if overwrite :supersede :error)))
    (log:info "Rendering documentation into '~A'" output-directory)
    (ensure-directories-exist output-directory)
    (let ((index (render-documentation-and-collect-index renderer output-directory exists-action)))
      (alexandria:with-output-to-file (output (merge-pathnames output-directory "symbol-index.md")
                                              :if-exists exists-action)
        (loop for entry in index
           do (format output "~A~%" entry))))))
