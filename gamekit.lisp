(in-package :trivial-gamekit)


(declaim (special *text-renderer*))


(define-constant +assets-directory+
    (merge-pathnames "assets/" (asdf:component-pathname (asdf:find-system :trivial-gamekit)))
  :test #'equal)


(defun asset-path (file)
  (merge-pathnames file +assets-directory+))


(defvar *gamekit-instance* nil)


(defclass gamekit-system (enableable generic-system)
  ((keymap :initform nil)
   (cursor-action :initform nil)
   (viewport-width :initarg :viewport-width :initform 640)
   (viewport-height :initarg :viewport-height :initform 480)
   (viewport-title :initarg :viewport-title :initform "Trivial Gamekit")
   (canvas :initform nil)
   (text-renderer :initform nil))
  (:default-initargs :depends-on '(graphics-system)))


(definline gamekit ()
  *gamekit-instance*)


(defgeneric draw (system)
  (:method ((system gamekit-system)) (declare (ignore system))))


(defmethod draw :around ((system gamekit-system))
  (with-slots (canvas text-renderer) system
    (with-canvas (canvas)
      (let ((*text-renderer* text-renderer))
        (gl:clear :color-buffer :depth-buffer :stencil-buffer)
        (call-next-method)))))


(define-event-handler on-keyboard-event ((ev keyboard-event) key state)
  (with-slots (keymap) (gamekit)
    (when-let ((action (getf (gethash key keymap) state)))
      (funcall action))))


(define-event-handler on-mouse-event ((ev mouse-event) button state)
  (with-slots (keymap) (gamekit)
    (when-let ((action (getf (gethash button keymap) state)))
      (funcall action))))


(define-event-handler on-cursor-event ((ev cursor-event) x y)
  (with-slots (cursor-action) (gamekit)
    (when cursor-action
      (funcall cursor-action x y))))


(defmethod initialize-system :after ((this gamekit-system))
  (with-slots (keymap viewport-title viewport-width viewport-height
                              text-renderer canvas)
      this
    (when *gamekit-instance*
      (error "Only one active system of type 'gamekit-system is allowed"))
    (register-resource-loader (make-resource-loader (asset-path "font.brf")))
    (setf keymap (make-hash-table)
          *gamekit-instance* this)

    (run (>> (-> ((host)) ()
               (setf (viewport-title) viewport-title)
               (setf (viewport-size) (vec2 viewport-width viewport-height)))
             (resource-flow (font-resource-name "NotoSansUI-Regular.ttf"))
             (-> ((graphics)) (font)
               (gl:viewport 0 0 viewport-width viewport-height)
               (setf text-renderer (make-text-renderer viewport-width
                                                       viewport-height
                                                       font 32.0)
                     canvas (make-canvas viewport-width
                                         viewport-height
                                         :antialiased t)))
             (concurrently ()
               (let (looped-flow)
                 (setf looped-flow (>> (-> ((graphics)) ()
                                         (draw this)
                                         (swap-buffers (host)))
                                       (instantly ()
                                         (when (enabledp this)
                                           (run looped-flow)))))
                 (run looped-flow)))))))


(defmethod discard-system :before ((this gamekit-system))
  (setf *gamekit-instance* nil))


(defun bind-button (key state action)
  (let ((gamekit (gamekit)))
    (with-slots (keymap) gamekit
      (with-system-lock-held (gamekit)
        (setf (getf (gethash key keymap) state) action)))))


(defun bind-cursor (action)
  (let ((gamekit (gamekit)))
    (with-slots (cursor-action) gamekit
      (with-system-lock-held (gamekit)
        (setf cursor-action action)))))


(defun print-text (string x y &optional color)
  (draw-text *text-renderer* string :position (vec2 x y) :color color))


(defun start (classname)
  (startup `(:engine (:systems (,classname)))))


(defun stop ()
  (shutdown))


(define-event-handler on-exit ((ev viewport-hiding-event))
  (in-new-thread "exit-thread"
    (stop)))
