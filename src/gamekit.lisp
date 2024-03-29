(cl:in-package :trivial-gamekit)


(declaim (special *font*))


(defvar +origin+ (vec2 0.0 0.0))
(defvar *black* (vec4 0 0 0 1))

(defvar *gamekit-instance* nil)

(defclass gamekit-system ()
  ((keymap :initform nil)
   (gamepad-map :initform nil)
   (gamepads :initform nil)
   (gamepad-action :initform nil)
   (cursor-action :initform nil)
   (cursor-position :initform (vec2 0 0))
   (cursor-changed-p :initform nil)
   (resource-path :initarg :resource-path :initform nil)
   (resource-registry)
   (prepare-resources :initform t)
   (button-action :initform nil)
   (scroll-action :initform nil)
   (viewport-width :initform 0)
   (viewport-height :initform 0)
   (canvas-width :initform 0)
   (canvas-height :initform 0)))


(defmethod initialize-instance ((this gamekit-system) &rest args &key depends-on)
  (apply #'call-next-method this
         :depends-on (union (list 'ge.snd:audio-system) depends-on)
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
  *gamekit-instance*)


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


(defmethod initialize-resources :around ((system gamekit-system))
  (with-slots (resource-registry) system
    (let ((*resource-registry* resource-registry))
      (call-next-method))))


(defmacro when-gamekit ((gamekit-var) &body body)
  `(when-let ((,gamekit-var (gamekit)))
     ,@body))


(defmacro if-gamekit ((gamekit-var) &body body)
  `(if-let ((,gamekit-var (gamekit)))
     ,@body))


(defun viewport-width ()
  (when-gamekit (gamekit)
    (with-slots (viewport-width) gamekit
      viewport-width)))


(defun viewport-height ()
  (when-gamekit (gamekit)
    (with-slots (viewport-height) gamekit
      viewport-height)))


(defun canvas-width ()
  (when-gamekit (gamekit)
    (with-slots (canvas-width) gamekit
      canvas-width)))


(defun canvas-height ()
  (when-gamekit (gamekit)
    (with-slots (canvas-height) gamekit
      canvas-height)))


(defmethod ge.app:draw :around ((this gamekit-system))
  (let ((*font* (cl-bodge.canvas:make-default-font)))
    (call-next-method)))


(defun register-gamepad (gamekit gamepad)
  (with-slots (gamepad-action gamepads) gamekit
    (push gamepad gamepads)
    (when gamepad-action
      (funcall gamepad-action gamepad :connected))))


(defun remove-gamepad (gamekit gamepad)
  (with-slots (gamepad-action gamepads) gamekit
    (deletef gamepads gamepad)
    (unwind-protect
         (when gamepad-action
           (funcall gamepad-action gamepad :disconnected)))))


(define-event-handler on-keyboard ((ev ge.host:keyboard-event) key state)
  (when-gamekit (gamekit)
    (with-slots (keymap button-action) gamekit
      (when-let ((action (getf (gethash key keymap) state)))
        (push-action action))
      (when button-action
        (flet ((call-action ()
                 (funcall button-action key state)))
          (push-action #'call-action))))))


(define-event-handler on-gamepad-connect ((ev ge.host:gamepad-connected-event))
  (when-gamekit (gamekit)
    (with-slots (gamepad-map gamepad-action) gamekit
      (let ((gamepad (ge.host:gamepad-from ev)))
        (flet ((%connect-gamepad ()
                 (register-gamepad gamekit gamepad)))
          (push-action #'%connect-gamepad))))))


(define-event-handler on-gamepad-disconnect ((ev ge.host:gamepad-disconnected-event))
  (when-gamekit (gamekit)
    (with-slots (gamepad-map gamepad-action) gamekit
      (let ((gamepad (ge.host:gamepad-from ev)))
        (flet ((%disconnect-gamepad ()
                 (remove-gamepad gamekit gamepad)))
          (push-action #'%disconnect-gamepad))))))


(define-event-handler on-gamepad-button ((ev ge.host:gamepad-button-event))
  (when-gamekit (gamekit)
    (with-slots (gamepad-map) gamekit
      (let ((gamepad (ge.host:gamepad-from ev))
            (button (ge.host:button-from ev))
            (state (ge.host:state-from ev)))
        (flet ((process-button ()
                 (when-let ((action (gethash :any gamepad-map)))
                   (funcall action gamepad button state))
                 (when-let ((action (getf (gethash button gamepad-map) state)))
                   (funcall action gamepad))))
          (push-action #'process-button))))))


(define-event-handler on-gamepad-dpad ((ev ge.host:gamepad-dpad-event))
  (when-gamekit (gamekit)
    (with-slots (gamepad-map) gamekit
      (let ((gamepad (ge.host:gamepad-from ev))
            (state (ge.host:state-from ev)))
        (flet ((process-dpad ()
                 (when-let ((action (getf (gethash :dpad gamepad-map) state)))
                   (funcall action gamepad))))
          (push-action #'process-dpad))))))


(defun invoke-stick-action (gamekit event stick)
  (with-slots (gamepad-map) gamekit
    (let ((gamepad (ge.host:gamepad-from event))
          (x (ge.host:x-from event))
          (y (ge.host:y-from event)))
      (flet ((process-stick ()
               (when-let ((action (gethash stick gamepad-map)))
                 (funcall action gamepad x y))))
        (push-action #'process-stick)))))


(define-event-handler on-gamepad-left-stick ((ev ge.host:gamepad-left-stick-event))
  (when-gamekit (gamekit)
    (invoke-stick-action gamekit ev :left-stick)))


(define-event-handler on-gamepad-right-stick ((ev ge.host:gamepad-right-stick-event))
  (when-gamekit (gamekit)
    (invoke-stick-action gamekit ev :right-stick)))


(defun invoke-trigger-action (gamekit event trigger)
  (with-slots (gamepad-map) gamekit
    (let ((gamepad (ge.host:gamepad-from event))
          (value (ge.host:value-from event)))
      (flet ((process-trigger ()
               (when-let ((action (gethash trigger gamepad-map)))
                 (funcall action gamepad value))))
        (push-action #'process-trigger)))))


(define-event-handler on-gamepad-left-trigger ((ev ge.host:gamepad-left-trigger-event))
  (when-gamekit (gamekit)
    (invoke-trigger-action gamekit ev :left-trigger)))


(define-event-handler on-gamepad-right-trigger ((ev ge.host:gamepad-right-trigger-event))
  (when-gamekit (gamekit)
    (invoke-trigger-action gamekit ev :right-trigger)))


(define-event-handler on-viewport-size-change ((ev ge.host:viewport-size-change-event)
                                               width height)
  (when-gamekit (gamekit)
    (with-slots (viewport-width viewport-height) gamekit
      (setf viewport-width width
            viewport-height height))))


(defun bodge-mouse-button->gamekit (bodge-button)
  (case bodge-button
    (:left :mouse-left)
    (:right :mouse-right)
    (:middle :mouse-middle)
    (t bodge-button)))


(define-event-handler on-mouse ((ev ge.host:mouse-event) button state)
  (when-gamekit (gamekit)
    (with-slots (keymap button-action) gamekit
      (when-let ((action (getf (gethash (bodge-mouse-button->gamekit button) keymap)
                               state)))
        (push-action action))
      (when button-action
        (flet ((call-action ()
                 (funcall button-action (bodge-mouse-button->gamekit button)
                          state)))
          (push-action #'call-action))))))

(define-event-handler on-scroll-event ((ev ge.host:scroll-event) x-offset y-offset)
  (when-gamekit (gamekit)
    (with-slots (keymap scroll-action) gamekit
      (when scroll-action
        (flet ((call-action ()
                 (funcall scroll-action x-offset y-offset)))
          (push-action #'call-action))))))


(define-event-handler on-cursor ((ev ge.host:cursor-event) x y)
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
     (funcall action)))
  (values))


(defgeneric notice-resources (game &rest resource-names)
  (:method (this &rest resource-names)
    (declare (ignore this resource-names))))


(defmethod notice-resources ((this gamekit-system) &rest resource-names)
  (declare (ignore this))
  (log:info "Resources loaded: ~A" resource-names))


(defun prepare-resources (&rest resource-names)
  (log:trace "Preparing resources: ~A" resource-names)
  (let ((game (gamekit)))
    (with-slots (resource-registry) game
      (flet ((notify-game ()
               (apply #'notice-resources game resource-names)))
        (run (>> (loading-flow resource-registry #'ge.app:app-canvas resource-names)
                 (instantly ()
                   (push-action #'notify-game))))))))


(defun dispose-resources (&rest resource-names)
  (let ((game (gamekit)))
    (with-slots (resource-registry) game
      (flet ((%%dispose ()
               (loop for name in resource-names
                     do (%dispose-resource resource-registry name))))
        (push-action #'%%dispose)))))


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
    (when prepare-resources
      (loading-flow resource-registry #'ge.app:app-canvas (list-all-resources)))))


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


(defun %update-canvas-dimensions (gamekit)
  (with-slots (canvas-width canvas-height) gamekit
    (let ((canvas (ge.app:app-canvas)))
      (setf canvas-width (ge.vg:canvas-width canvas)
            canvas-height (ge.vg:canvas-height canvas)))))


(defmethod ge.app:configuration-flow ((this gamekit-system))
  (with-slots (keymap gamepad-map resource-registry
               viewport-width viewport-height
               canvas-width canvas-height)
      this
    (>> (instantly ()
          (configure-game this)
          (setf keymap (make-hash-table)
                gamepad-map (make-hash-table)
                resource-registry (make-instance 'gamekit-resource-registry)
                *gamekit-instance* this))
        (ge.host:for-host ()
          (ge.host:with-viewport-dimensions (width height)
            (setf viewport-width width
                  viewport-height height)))
        (ge.gx:for-graphics ()
          (%update-canvas-dimensions this))
        (instantly ()
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
        (ge.host:for-host ()
          (loop for gamepad in (ge.host:list-gamepads)
                do (register-gamepad this gamepad)))
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
      (%dispose-resources resource-registry)
      (log/debug "Sweeping complete")
      (setf *gamekit-instance* nil))))


(ge.ng:define-event-handler on-viewport-update ((ev ge.host:viewport-size-change-event))
  (when-gamekit (gamekit)
    (ge.app:inject-flow (ge.gx:for-graphics ()
                          (%update-canvas-dimensions gamekit)))))


(defun resource-by-id (id)
  (with-slots (resource-registry) (gamekit)
    (%get-resource resource-registry id)))


(defun raise-binding-error ()
  (error "Input can only be bound when gamekit is started."))


(defun bind-button (key state action)
  (if-gamekit (gamekit)
    (with-slots (keymap) gamekit
      (with-system-lock-held (gamekit)
        (setf (getf (gethash key keymap) state) action)))
    (raise-binding-error)))


(defun bind-any-gamepad (action)
  (if-gamekit (gamekit)
    (with-slots (gamepad-action gamepads) gamekit
      (setf gamepad-action action)
      (when action
        (flet ((register-gamepads ()
                 (loop for gamepad in gamepads
                       do (funcall action gamepad :connected))))
          (push-action #'register-gamepads))))
    (raise-binding-error)))


(defun bind-gamepad-button (button state action)
  (if-gamekit (gamekit)
    (with-slots (gamepad-map) gamekit
      (setf (getf (gethash button gamepad-map) state) action))
    (raise-binding-error)))


(defun bind-gamepad-dpad (state action)
  (if-gamekit (gamekit)
    (with-slots (gamepad-map) gamekit
      (setf (getf (gethash :dpad gamepad-map) state) action))
    (raise-binding-error)))


(defun bind-gamepad-any-button (action)
  (if-gamekit (gamekit)
    (with-slots (gamepad-map) gamekit
      (setf (gethash :any gamepad-map) action))
    (raise-binding-error)))


(defun bind-gamepad-stick (stick action)
  (if-gamekit (gamekit)
    (with-slots (gamepad-map) gamekit
      (let ((stick (ecase stick
                     (:right :right-stick)
                     (:left :left-stick))))
        (setf (gethash stick gamepad-map) action)))
    (raise-binding-error)))


(defun bind-gamepad-trigger (trigger action)
  (if-gamekit (gamekit)
    (with-slots (gamepad-map) gamekit
      (let ((trigger (ecase trigger
                       (:right :right-trigger)
                       (:left :left-trigger))))
        (setf (gethash trigger gamepad-map) action)))
    (raise-binding-error)))


(defun bind-any-button (action)
  (if-gamekit (gamekit)
    (with-slots (button-action) gamekit
      (with-system-lock-held (gamekit)
        (setf button-action action)))
    (raise-binding-error)))


(defun bind-cursor (action)
  (if-gamekit (gamekit)
    (with-slots (cursor-action) gamekit
      (with-system-lock-held (gamekit)
        (setf cursor-action action)))
    (raise-binding-error)))

(defun bind-scroll (action)
  (if-gamekit (gamekit)
    (with-slots (scroll-action) gamekit
      (with-system-lock-held (gamekit)
        (setf scroll-action action)))
    (raise-binding-error)))


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


(defun draw-image (position image-id &key origin width height
                                       mirror-y mirror-x)
  (when-let ((image (resource-by-id image-id)))
    (let* ((image-origin (or origin +origin+))
           (image-width (if width
                            width
                            (ge.vg:image-paint-width image)))
           (image-height (if height
                             height
                             (ge.vg:image-paint-height image))))
      (ge.vg:with-retained-canvas
        (when mirror-y
          (ge.vg:translate-canvas 0 image-height)
          (ge.vg:scale-canvas 1 -1))
        (when mirror-x
          (ge.vg:translate-canvas image-width 0)
          (ge.vg:scale-canvas -1 1))

        (ge.vg:translate-canvas (x position) (y position))
        (ge.vg:translate-canvas (- (x image-origin)) (- (y image-origin)))
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


(defun draw-text (string origin &key fill-color font)
  (ge.vg:with-font ((or font *font*))
    (ge.vg:draw-text origin string (or fill-color *black*))))


(defun get-text (resource-id)
  (resource-by-id resource-id))


(defun get-binary (resource-id)
  (resource-by-id resource-id))


(defun start (classname &key (log-level :info)
                          (opengl-version '(3 3))
                          samples
                          blocking
                          viewport-resizable
                          (viewport-decorated t)
                          (autoscaled t)
                          swap-interval
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
