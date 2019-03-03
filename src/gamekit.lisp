(cl:in-package :trivial-gamekit)


(declaim (special *font*))


(defvar +origin+ (vec2 0.0 0.0))


(defvar *black* (vec4 0 0 0 1))


(defclass gamekit-system (enableable generic-system)
  ((keymap :initform nil)
   (cursor-action :initform nil)
   (cursor-position :initform (vec2 0 0))
   (cursor-changed-p :initform nil)
   (resource-path :initarg :resource-path :initform nil)
   (resource-registry)
   (viewport-width :initarg :viewport-width :initform 640)
   (viewport-height :initarg :viewport-height :initform 480)
   (viewport-scale :initform 1f0)
   (framebuffer-size :initform (vec2 640 480) :accessor %framebuffer-size-of)
   (fullscreen-p :initarg :fullscreen-p :initform nil)
   (autoscaled :initform (property '(:host :autoscaled) t))
   (prepare-resources :initform t)
   (viewport-title :initarg :viewport-title :initform "Trivial Gamekit")
   (canvas :initform nil :reader canvas-of)
   (font :initform nil :reader font-of)
   (antialiased :initform (property '(:gamekit :antialiased) t))
   (button-action :initform nil)
   (action-queue :initform (make-task-queue)))
  (:default-initargs :depends-on '(graphics-system audio-system)))


(defgeneric configure-game (game)
  (:method ((this gamekit-system)) (declare (ignore this))))


(defun split-opts (opts)
  (loop for opt in opts
     if (member (first opt) '(:viewport-width
                              :viewport-height
                              :viewport-title
                              :fullscreen-p
                              :prepare-resources))
     collect opt into extended
     else
     collect opt into std
     finally (return (values std extended))))


(defmacro defgame (name (&rest classes) &body ((&rest slots) &rest opts))
  (multiple-value-bind (std-opts extended) (split-opts opts)
    `(progn
       (defclass ,name (gamekit-system ,@classes)
         ,slots
         ,@std-opts)
       ,(with-hash-entries ((viewport-width :viewport-width)
                            (viewport-height :viewport-height)
                            (viewport-title :viewport-title)
                            (fullscreen-p :fullscreen-p)
                            (prepare-resources :prepare-resources))
                           (alist-hash-table extended)
          `(progn
             (defmethod configure-game ((this ,name))
               ,@(when viewport-width
                   `((setf (slot-value this 'viewport-width) ,@viewport-width)))
               ,@(when viewport-height
                   `((setf (slot-value this 'viewport-height) ,@viewport-height)))
               ,@(when viewport-title
                   `((setf (slot-value this 'viewport-title) ,@viewport-title)))
               ,@(when fullscreen-p
                   `((setf (slot-value this 'fullscreen-p) ,@fullscreen-p)))
               ,@(multiple-value-bind (value exist-p) prepare-resources
                   `((setf (slot-value this 'prepare-resources) ,(if exist-p
                                                                     (first value)
                                                                     t))))))))))


(defmethod initialize-instance :around ((this gamekit-system) &key)
  (when (null *gamekit-instance-class*)
    (error "Manual gamekit instance creation forbidden. Use #'gamekit:start"))
  (call-next-method))


(defun gamekit ()
  (when *gamekit-instance-class*
    (ge.ng:engine-system *gamekit-instance-class*)))


(defgeneric act (system)
  (:method (system) (declare (ignore system))))


(defgeneric draw (system)
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


(defmethod draw :around ((system gamekit-system))
  (with-slots (canvas font framebuffer-size) system
    (gl:viewport 0 0 (x framebuffer-size) (y framebuffer-size))
    (gl:clear :color-buffer :depth-buffer :stencil-buffer)
    (let ((*font* font))
      (render t canvas :next-method #'call-next-method))))


(defmethod initialize-resources :around ((system gamekit-system))
  (with-slots (resource-registry) system
    (let ((*resource-registry* resource-registry))
      (call-next-method))))


(defmacro when-gamekit ((gamekit-var) &body body)
  `(when-let ((,gamekit-var (gamekit)))
     ,@body))


(define-event-handler on-keyboard-event ((ev keyboard-event) key state)
  (when-gamekit (gamekit)
    (with-slots (keymap action-queue button-action) gamekit
      (when-let ((action (getf (gethash key keymap) state)))
        (push-task action action-queue))
      (when button-action
        (flet ((call-action ()
                 (funcall button-action key state)))
          (push-task #'call-action action-queue))))))


(defun bodge-mouse-button->gamekit (bodge-button)
  (case bodge-button
    (:left :mouse-left)
    (:right :mouse-right)
    (:middle :mouse-middle)
    (t bodge-button)))


(define-event-handler on-mouse-event ((ev mouse-event) button state)
  (when-gamekit (gamekit)
    (with-slots (keymap action-queue button-action) gamekit
      (when-let ((action (getf (gethash (bodge-mouse-button->gamekit button) keymap) state)))
        (push-task action action-queue))
      (when button-action
        (flet ((call-action ()
                 (funcall button-action (bodge-mouse-button->gamekit button)
                          state)))
          (push-task #'call-action action-queue))))))


(define-event-handler on-cursor-event ((ev cursor-event) x y)
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
  (when-let ((gamekit (gamekit)))
    (with-slots (action-queue) gamekit
      (push-task action action-queue))))


(defgeneric notice-resources (game &rest resource-names)
  (:method ((this gamekit-system) &rest resource-names)
    (declare (ignore this))
    (log:info "Resources loaded: ~A" resource-names)))


(defun prepare-resources (&rest resource-names)
  (log:trace "Preparing resources: ~A" resource-names)
  (let ((game (gamekit)))
    (with-slots (canvas resource-registry) game
      (flet ((get-canvas ()
               canvas)
             (notify-game ()
               (apply #'notice-resources game resource-names)))
        (run (flow:>> (loading-flow resource-registry #'get-canvas resource-names)
                 (instantly ()
                   (push-action #'notify-game))))))))


(define-event-handler on-framebuffer-change ((ev framebuffer-size-change-event) width height)
  (when-let ((gamekit (gamekit)))
    (flet ((update-framebuffer ()
             (setf (%framebuffer-size-of gamekit) (vec2 width height))))
      (push-action #'update-framebuffer))))


(defun update-viewport (gamekit w h scale)
  (with-slots (viewport-width viewport-height viewport-scale
               canvas pixel-ratio autoscaled)
      gamekit
    (let ((scale (if autoscaled scale 1f0)))
      (setf viewport-scale scale
            viewport-width (/ w scale)
            viewport-height (/ h scale)))
    (ge.vg:update-canvas-size canvas viewport-width viewport-height)
    (ge.vg:update-canvas-pixel-ratio canvas (/ (x (%framebuffer-size-of gamekit))
                                               viewport-width ))))


(define-event-handler on-window-size-change ((ev viewport-size-change-event) width height)
  (when-let ((gamekit (gamekit)))
    (run
     (for-host ()
       (let* ((scale (viewport-scale)))
         (flet ((%update-viewport ()
                  (update-viewport gamekit width height scale)))
           (push-action #'%update-viewport)))))))


(defun %mount-for-executable (this)
  (with-slots (resource-path prepare-resources) this
    (unless (executablep)
      (when resource-path
        (register-resource-package :keyword resource-path)
        (%mount-packages :keyword))
      (when prepare-resources
        (apply #'%mount-resources (list-all-resources))))))

(defun %initialize-host (this)
  (with-slots (viewport-title viewport-width viewport-height fullscreen-p framebuffer-size) this
    (let* ((vp-size (viewport-size))
           (fb-size (framebuffer-size))
           (pixel-ratio (/ (x fb-size) (x vp-size))))
      (setf (viewport-title) viewport-title
            (fullscreen-viewport-p) fullscreen-p
            (viewport-size) (vec2 viewport-width viewport-height)
            framebuffer-size (vec2 (* viewport-width pixel-ratio)
                                   (* viewport-height pixel-ratio)))
      (initialize-host this)
      pixel-ratio)))

(defun %initialize-graphics (this pixel-ratio)
  (with-slots (viewport-width viewport-height canvas font antialiased) this
    (setf canvas (ge.vg:make-canvas 'gamekit-canvas viewport-width
                                    viewport-height
                                    :pixel-ratio pixel-ratio
                                    :antialiased antialiased))
    (let ((font-face (ge.vg:register-font-face canvas
                                               +font-name+
                                               (load-resource +font-name+))))
      (setf font (ge.vg:make-font font-face :size 32)))
    (setf (swap-interval) 1)
    (initialize-graphics this)))

(defun %prepare-resources (this)
  (with-slots (canvas resource-registry prepare-resources) this
    (flet ((%get-canvas () canvas))
      (when prepare-resources
        (loading-flow resource-registry #'%get-canvas (list-all-resources))))))

(defun %game-loop (this)
  (with-slots (action-queue cursor-position cursor-changed-p cursor-action
               viewport-scale)
      this
    (labels ((%process-cursor ()
               (when (and cursor-action cursor-changed-p)
                 (funcall cursor-action
                          (/ (x cursor-position) viewport-scale)
                          (/ (y cursor-position) viewport-scale))
                 (setf cursor-changed-p nil)))
             (%act ()
               (%process-cursor)
               (drain action-queue)
               (act this))
             (%draw ()
               (draw this)
               (swap-buffers)))
      (loop-flow (flow:>> (instantly () (%act))
                     (for-graphics () (%draw)))
         (lambda () (enabledp this))))))

(defmethod initialize-system :after ((this gamekit-system))
  (with-slots (keymap resource-registry) this
    (configure-game this)
    (setf keymap (make-hash-table)
          resource-registry (make-instance 'gamekit-resource-registry))
    (initialize-resources this)
    (%mount-for-executable this)))

(defmethod enabling-flow list ((this gamekit-system))
  (flow:>>
   (for-host ()
     (%initialize-host this))
   (for-graphics (pixel-ratio)
     (%initialize-graphics this pixel-ratio))
   (flow:->> ()
     (%prepare-resources this))
   (instantly ()
     (initialize-audio this)
     (post-initialize this)
     (run (flow:>> (%game-loop this)
                   (instantly ()
                     (pre-destroy this)))))))


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


;;;
;;; Startup routines
;;;
(defun start (classname &key (log-level :info)
                          (opengl-version '(3 3))
                          samples
                          blocking
                          viewport-resizable
                          (viewport-decorated t)
                          (autoscaled t))
  (when *gamekit-instance-class*
    (error "Only one active system of type 'gamekit-system is allowed"))
  (setf *gamekit-instance-class* classname)
  (startup `(:engine (:systems (,classname) :log-level ,log-level)
             :host (:opengl-version ,opengl-version
                    :samples ,samples
                    :viewport-resizable ,viewport-resizable
                    :viewport-decorated ,viewport-decorated
                    :autoscaled ,autoscaled)
             :gamekit (:antialiased ,(not samples)))
           :blocking blocking))


(defun %stop ()
  (when *gamekit-instance-class*
    (unwind-protect
         (shutdown)
      (setf *gamekit-instance-class* nil))))


(defun stop (&key blocking)
  (if blocking
      (%stop)
      (in-new-thread ("exit-thread")
        (%stop))))


(define-event-handler on-exit ((ev viewport-hiding-event))
  (stop))
