(cl:in-package :trivial-gamekit.documentation)


(defclass kramdown-renderer () ())


(defparameter *template* (alexandria:read-file-into-string
                          (asdf:system-relative-pathname :trivial-gamekit/documentation
                                                         "docs/doc-entry.template")))

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
                                                  :trivial-gamekit/documentation "build/docs/")))
  (let ((renderer (make-instance 'kramdown-renderer))
        (exists-action (if overwrite :supersede :error)))
    (log:info "Rendering documentation into '~A'" output-directory)
    (ensure-directories-exist output-directory)
    (let ((index (render-documentation-and-collect-index renderer output-directory exists-action)))
      (alexandria:with-output-to-file (output (merge-pathnames output-directory "symbol-index.md")
                                              :if-exists exists-action)
        (loop for entry in index
           do (format output "~A~%" entry))))))
