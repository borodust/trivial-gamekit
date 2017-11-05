(in-package :trivial-gamekit)


(declaim (special *resource-registry*))


(defvar *gamekit-assets-root*
  (merge-pathnames "assets/" (asdf:component-pathname (asdf:find-system :trivial-gamekit))))


(define-constant +font-name+ "/_gamekit/font/sdf/NotoSans-Regular.ttf"
  :test #'equal)


(defun asset-path (file)
  (merge-pathnames file *gamekit-assets-root*))


(mount-container "/_gamekit/font/" (asset-path "font.brf"))


(define-sdf-font +font-name+)


(defclass gamekit-resource-registry ()
  ((resources :initform (make-hash-table :test 'equal))))


(defun %load-sound (path)
  (>> (concurrently ()
        (load-ogg-vorbis-audio path))
      (-> ((audio)) (sound)
        (let ((source (ge.snd:make-audio-source)))
          (with-disposable ((buffer (ge.snd:make-audio-buffer sound)))
            (ge.snd:attach-audio-buffer buffer source))
          source))))


(defun %load-image (path canvas-provider &key)
  (>> (concurrently ()
        (load-png-image path))
      (-> ((graphics)) (image)
        (ge.vg:make-image-paint image :canvas (funcall canvas-provider)))))


(defun %load-resource (resource canvas-provider base-path)
  (destructuring-bind ((type . resource-path) &rest options &key &allow-other-keys) resource
    (let ((path (merge-pathnames resource-path base-path)))
      (switch (type :test #'eq)
        (:image (apply #'%load-image path canvas-provider options))
        (:sound (%load-sound path))))))


(defun preloading-flow (loader canvas-provider base-path)
  (with-slots (resources) loader
    (let ((ids (loop for resource-id being the hash-key in resources collect resource-id)))
      (>> (~> (loop for id in ids collect (%load-resource (gethash id resources)
                                                          canvas-provider
                                                          base-path)))
          (concurrently (results)
            (loop for id in ids
               for result in (first results)
               do (setf (gethash id resources) result)))))))


(defun %register-resource (loader type id path &rest options &key &allow-other-keys)
  (with-slots (resources) loader
    (setf (gethash id resources) (nconc (list (cons type path)) options))))


(defun %get-resource (loader id)
  (with-slots (resources) loader
    (gethash id resources)))


(defun import-image (resource-id path)
  (%register-resource *resource-registry* :image resource-id path))


(defun import-sound (resource-id path)
  (%register-resource *resource-registry* :sound resource-id path))
