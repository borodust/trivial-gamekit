(cl:in-package :trivial-gamekit)


(declaim (special *font*))


(defvar +origin+ (vec2 0.0 0.0))


(defvar *black* (vec4 0 0 0 1))


(defclass gamekit-system ()
  ((keymap :initform nil)
   (cursor-action :initform nil)
   (cursor-position :initform (vec2 0 0))
   (cursor-changed-p :initform nil)
   (resource-path :initarg :resource-path :initform nil)
   (resource-registry)
   (prepare-resources :initform t)
   (button-action :initform nil)))


(defmethod initialize-instance ((this gamekit-system) &rest args &key depends-on)
  (apply #'call-next-method this
         :depends-on (append (list 'ge.snd:audio-system) depends-on)
         args))


(defgeneric configure-game (game)
  (:method (this) (declare (ignore this))))


(defun split-opts (opts)
  (loop for opt in opts
        if (member (first opt) '(:prepare-resources))
          collect opt into extended
        else
          collect opt into std
        finally (return (values std extended))))


(defmacro defgame (name (&rest classes) &body ((&rest slots) &rest opts))
  (multiple-value-bind (std-opts extended) (split-opts opts)
    `(progn
       (ge.app:defapp ,name (gamekit-system ,@classes)
         ,slots
         ,@std-opts)
       ,(with-hash-entries ((prepare-resources :prepare-resources))
                           (alist-hash-table extended)
          `(progn
             (defmethod configure-game ((this ,name))
               (log/debug "Reconfiguring ~A" ',name)
               ,@(multiple-value-bind (value exist-p) prepare-resources
                   `((setf (slot-value this 'prepare-resources) ,(if exist-p
                                                                     (first value)
                                                                     t))))))))))


(defun gamekit ()
  (ge.app:app))


(defgeneric act (system)
  (:method (system) (declare (ignore system))))


(defgeneric initialize-resources (system)
  (:method (system) (declare (ignore system))))


(defgeneric initialize-audio (system)
  (:method (system) (declare (ignore system))))


(defgeneric initialize-graphics (system)
  (:method (system) (declare (ignore system))))


(defgeneric initialize-host (system)
  (:method (system) (declare (ignore system))))


(defgeneric post-initialize (system)
  (:method (system)
    (declare (ignore system))))


(defgeneric pre-destroy (system)
  (:method (system) (declare (ignore system))))


(ge.vg:defcanvas gamekit-canvas (next-method)
  (funcall next-method))


(defmethod initialize-resources :around ((system gamekit-system))
  (with-slots (resource-registry) system
    (let ((*resource-registry* resource-registry))
      (call-next-method))))


(defmacro when-gamekit ((gamekit-var) &body body)
  `(when-let ((,gamekit-var (gamekit)))
     ,@body))


(defmethod ge.app:draw :around ((this gamekit-system))
  (let ((*font* (cl-bodge.canvas:make-default-font)))
    (call-next-method)))


(define-event-handler on-keyboard-event ((ev ge.host:keyboard-event) key state)
  (when-gamekit (gamekit)
    (with-slots (keymap button-action) gamekit
      (when-let ((action (getf (gethash key keymap) state)))
        (push-action action))
      (when button-action
        (flet ((call-action ()
                 (funcall button-action key state)))
          (push-action #'call-action))))))


(defun bodge-mouse-button->gamekit (bodge-button)
  (case bodge-button
    (:left :mouse-left)
    (:right :mouse-right)
    (:middle :mouse-middle)
    (t bodge-button)))


(define-event-handler on-mouse-event ((ev ge.host:mouse-event) button state)
  (when-gamekit (gamekit)
    (with-slots (keymap button-action) gamekit
      (when-let ((action (getf (gethash (bodge-mouse-button->gamekit button) keymap) state)))
        (push-action action))
      (when button-action
        (flet ((call-action ()
                 (funcall button-action (bodge-mouse-button->gamekit button)
                          state)))
          (push-action #'call-action))))))


(define-event-handler on-cursor-event ((ev ge.host:cursor-event) x y)
  (when-gamekit (gamekit)
    (with-slots (cursor-position cursor-changed-p) gamekit
      (unless cursor-changed-p
        (setf cursor-changed-p t))
      (setf (x cursor-position) x
            (y cursor-position) y))))


(defun make-package-resource-table (resource-paths)
  (flet ((to-package-pair (pair)
           (cons (find-package (car pair)) (cdr pair))))
    (alist-hash-table (mapcar #'to-package-pair resource-paths))))


(defun push-action (action)
  (ge.app:inject-flow
   (instantly ()
     (funcall action))))


(defgeneric notice-resources (game &rest resource-names)
  (:method ((this gamekit-system) &rest resource-names)
    (declare (ignore this))
    (log:info "Resources loaded: ~A" resource-names)))


(defun prepare-resources (&rest resource-names)
  (log:trace "Preparing resources: ~A" resource-names)
  (let ((game (gamekit)))
    (with-slots (resource-registry) game
      (flet ((get-canvas ()
               (ge.app:app-canvas game))
             (notify-game ()
               (apply #'notice-resources game resource-names)))
        (run (>> (loading-flow resource-registry #'get-canvas resource-names)
                 (instantly ()
                   (push-action #'notify-game))))))))


(defun %mount-for-executable (this)
  (with-slots (resource-path prepare-resources) this
    (unless (executablep)
      (when resource-path
        (register-resource-package :keyword resource-path)
        (%mount-packages :keyword))
      (when prepare-resources
        (apply #'%mount-resources (list-all-resources))))))


(defun %prepare-resources (this)
  (with-slots (resource-registry prepare-resources) this
    (flet ((%get-canvas ()
             (ge.app:app-canvas this)))
      (when prepare-resources
        (loading-flow resource-registry #'%get-canvas (list-all-resources))))))


(defmethod ge.app:acting-flow ((this gamekit-system))
  (with-slots (cursor-position cursor-changed-p cursor-action) this
    (labels ((%process-cursor ()
               (when (and cursor-action cursor-changed-p)
                 (funcall cursor-action (x cursor-position) (y cursor-position))
                 (setf cursor-changed-p nil)))
             (%act ()
               (%process-cursor)
               (act this)))
      (instantly () (%act)))))


(defmethod ge.app:configuration-flow ((this gamekit-system))
  (with-slots (keymap resource-registry) this
    (>> (instantly ()
          (configure-game this)
          (setf keymap (make-hash-table)
                resource-registry (make-instance 'gamekit-resource-registry))
          (initialize-resources this)
          (%mount-for-executable this))
        (ge.host:for-host ()
          (log/debug "Initializing host")
          (initialize-host this))
        (ge.gx:for-graphics ()
          (log/debug "Initializing graphics")
          (initialize-graphics this))
        (->> ()
          (log/debug "Preparing resources")
          (%prepare-resources this))
        (instantly ()
          (log/debug "Initializing audio")
          (initialize-audio this)
          (log/debug "Invoking post-initialization hook")
          (post-initialize this)
          (log/debug "Initialization completed")))))


(defmethod ge.app:sweeping-flow ((this gamekit-system))
  (with-slots (resource-registry) this
    (instantly ()
      (log/debug "Invoking pre-destroy hook")
      (pre-destroy this)
      (log/debug "Disposing resources")
      (dispose-resources resource-registry)
      (log/debug "Sweeping complete"))))


(defun resource-by-id (id)
  (with-slots (resource-registry) (gamekit)
    (%get-resource resource-registry id)))


(defun bind-button (key state action)
  (when-gamekit (gamekit)
    (with-slots (keymap) gamekit
      (with-system-lock-held (gamekit)
        (setf (getf (gethash key keymap) state) action)))))


(defun bind-any-button (action)
  (when-gamekit (gamekit)
    (with-slots (button-action) gamekit
      (with-system-lock-held (gamekit)
        (setf button-action action)))))


(defun bind-cursor (action)
  (let ((gamekit (gamekit)))
    (with-slots (cursor-action) gamekit
      (with-system-lock-held (gamekit)
        (setf cursor-action action)))))


(defun play-sound (sound-id &key looped-p)
  (let ((source (resource-by-id sound-id)))
    (when looped-p
      (setf (ge.snd:audio-looped-p source) t))
    (ge.snd:play-audio source)))


(defun stop-sound (sound-id)
  (ge.snd:stop-audio (resource-by-id sound-id)))


(defun play (sound-id &key looped-p)
  (play-sound sound-id :looped-p looped-p))


(defmacro with-pushed-canvas (() &body body)
  `(ge.vg:with-retained-canvas
     ,@body))


(defun draw-image (position image-id &key origin width height)
  (when-let ((image (resource-by-id image-id)))
    (let* ((image-origin (or origin +origin+))
           (image-width (if width
                            width
                            (ge.vg:image-paint-width image)))
           (image-height (if height
                             height
                             (ge.vg:image-paint-height image))))
      (ge.vg:with-retained-canvas
        (ge.vg:translate-canvas (- (x position) (x image-origin)) (- (y position) (y image-origin)))
        (draw-rect image-origin image-width image-height :fill-paint image)))))


(defun image-width (image-id)
  (if-let ((image (resource-by-id image-id)))
    (ge.vg:image-paint-width image)
    (error "Image ~A not found" image-id)))


(defun image-height (image-id)
  (if-let ((image (resource-by-id image-id)))
    (ge.vg:image-paint-height image)
    (error "Image ~A not found" image-id)))


(defun make-font (font-id size)
  (ge.vg:make-font (resource-by-id font-id) :size size))


(defun calc-text-bounds (text &optional (font *font*))
  (ge.vg:with-font (font)
    (ge.vg:canvas-text-bounds text)))


(defun print-text (string x y &optional (color *black*))
  (draw-text string (vec2 x y) :fill-color color))


(defun draw-text (string origin &key (fill-color *black*) (font *font*))
  (ge.vg:with-font (font)
    (ge.vg:draw-text origin string fill-color)))


(defun start (classname &key (log-level :info)
                          (opengl-version '(3 3))
                          samples
                          blocking
                          viewport-resizable
                          (viewport-decorated t)
                          (autoscaled t)
                          (swap-interval 1)
                          properties)
  (log/level log-level)
  (ge.app:start classname :log-level log-level
                          :opengl-version opengl-version
                          :samples samples
                          :blocking blocking
                          :viewport-resizable viewport-resizable
                          :viewport-decorated viewport-decorated
                          :autoscaled autoscaled
                          :swap-interval swap-interval
                          :properties properties))
