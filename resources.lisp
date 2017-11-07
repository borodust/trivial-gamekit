(in-package :trivial-gamekit)


(declaim (special *resource-registry*))


(define-constant +game-resource-root+ "/_asset/"
  :test #'equal)

(define-constant +kit-resource-root+ "/_gamekit/"
  :test #'equal)

(define-constant +font-name+ (format nil "~A~A/"
                                     +kit-resource-root+
                                     "/font/sdf/NotoSans-Regular.ttf")
  :test #'equal)

(defvar *resources* nil)

(defvar *gamekit-assets-root*
  (merge-pathnames "assets/" (asdf:component-pathname (asdf:find-system :trivial-gamekit))))


(defun game-resource-path (symbol)
  (reduce #'merge-pathnames (list (symbol-name symbol)
                                  (fad:pathname-as-directory
                                   (package-name (symbol-package symbol)))
                                  +game-resource-root+)))


(defun kit-resource-path (resource)
  (reduce #'merge-pathnames (list resource +kit-resource-root+)))


(defun gamekit-asset-path (file)
  (merge-pathnames file *gamekit-assets-root*))


(mount-container "/_gamekit/font/" (gamekit-asset-path "font.brf"))


(define-sdf-font +font-name+)


(defclass gamekit-resource-registry ()
  ((resources :initform (make-hash-table :test 'equal))))


(defun %load-sound (resource-name)
  (>> (resource-flow resource-name)
      (-> ((audio)) ((sound))
        (let ((source (ge.snd:make-audio-source)))
          (with-disposable ((buffer (ge.snd:make-audio-buffer sound)))
            (ge.snd:attach-audio-buffer buffer source))
          source))))


(defun %load-image (resource-name canvas-provider &key)
  (>> (resource-flow resource-name)
      (-> ((graphics)) ((image))
        (ge.vg:make-image-paint image :canvas (funcall canvas-provider)))))


(defun %load-resource (resource-name type canvas-provider)
  (switch (type :test #'eq)
    (:image (%load-image resource-name canvas-provider))
    (:audio (%load-sound resource-name))))


(defun preloading-flow (loader canvas-provider base-paths)
  (with-slots (resources) loader
    (>> (~> (loop for (id type path) in *resources*
               as resource-name = (game-resource-path id)
               as resource-package = (find-package (symbol-package id))
               as base-path = (gethash resource-package base-paths)
               do (mount-filesystem resource-name (merge-pathnames path base-path))
               collect (let ((id id)
                             (resource-name resource-name)
                             (type type))
                         (>> (%load-resource resource-name type canvas-provider)
                             (instantly (resource)
                               (cons id resource))))))
        (concurrently ((results))
          (loop for (id . resource) in results
             do (setf (gethash id resources) resource))))))


(defun %register-resource (loader type id path &rest options &key &allow-other-keys)
  (with-slots (resources) loader
    (setf (gethash id resources) (nconc (list (cons type path)) options))))


(defun %get-resource (loader id)
  (with-slots (resources) loader
    (gethash id resources)))


(defun register-game-resource (id path &rest handler-args)
  (let ((resource-path (game-resource-path id)))
    (register-resource resource-path
                       (apply #'make-resource-handler handler-args))
    (pushnew (list id (first handler-args) path) *resources* :test #'eq :key #'first)))


(defun import-image (resource-id path)
  (register-game-resource resource-id path :image :type :png))


(defun import-sound (resource-id path)
  (register-game-resource resource-id path :audio))


(defmacro define-image (name path)
  `(register-game-resource ,name ,path :image :type :png))
