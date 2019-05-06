(cl:in-package :trivial-gamekit)


(declaim (special *resource-registry*))


(define-constant +game-resource-root+ "/_asset/"
  :test #'equal)

(define-constant +kit-resource-root+ "/_gamekit/"
  :test #'equal)


(defvar *resources* nil)
(defvar *resouce-packages* nil)


(defvar *gamekit-assets-root*
  (merge-pathnames "assets/" (asdf:component-pathname (asdf:find-system :trivial-gamekit))))


(defun game-resource-path (symbol)
  (reduce #'merge-pathnames (list (symbol-name symbol)
                                  (fad:pathname-as-directory
                                   (package-name (symbol-package symbol)))
                                  +game-resource-root+)))


(defun kit-resource-path (resource)
  (reduce #'merge-pathnames (list resource +kit-resource-root+)))


(defun kit-asset-path (file)
  (merge-pathnames file *gamekit-assets-root*))


(defclass gamekit-resource-registry ()
  ((resources :initform (make-hash-table :test 'equal))))


(defun %load-sound (resource-name)
  (concurrently ()
    (let* ((sound (ge.rsc:load-resource resource-name))
           (source (ge.snd:make-audio-source)))
      (with-disposable ((buffer (ge.snd:make-audio-buffer sound)))
        (ge.snd:attach-audio-buffer buffer source))
      (cons source t))))


(defun %load-image (resource-name canvas-provider &key use-nearest-interpolation)
  (>> (concurrently ()
        (ge.rsc:load-resource resource-name))
      (ge.gx:for-graphics (image)
        (cons (ge.vg:make-image-paint (funcall canvas-provider) image
                                      :use-nearest-interpolation use-nearest-interpolation)
              t))))


(defun %load-font (resource-name canvas-provider &key)
  (>> (concurrently ()
        (ge.rsc:load-resource resource-name))
      (ge.gx:for-graphics (font-face)
        (cons (ge.vg:register-font-face (funcall canvas-provider) resource-name font-face) nil))))


(defun %load-resource (resource-name type canvas-provider parameters)
  (eswitch (type :test #'eq)
    (:image (apply #'%load-image resource-name canvas-provider parameters))
    (:audio (%load-sound resource-name))
    (:font (%load-font resource-name canvas-provider))))


(defun loading-flow (loader canvas-provider resource-names)
  (with-slots (resources) loader
    (unless (ge.ng:executablep)
      (apply #'%mount-resources resource-names))
    (>>
     (~> (loop for (id type nil . parameters) in *resources*
               when (member id resource-names :test #'eq)
                 collect (when-let ((id id)
                                    (type type)
                                    (resource-path (game-resource-path id)))
                           (>> (%load-resource resource-path
                                               type
                                               canvas-provider
                                               parameters)
                               (instantly ((resource . disposable-p))
                                 (list id resource disposable-p))))))
     (concurrently ((results))
       (loop for (id resource disposable-p) in results
             do (setf (gethash id resources) (cons resource disposable-p)))))))


(defun dispose-resources (registry)
  (with-slots (resources) registry
    (loop for (resource . disposable-p) being the hash-value of resources
          when disposable-p
          do (dispose resource))))


(defun list-all-resources ()
  (mapcar #'car *resources*))


(defun %get-resource (loader id)
  (with-slots (resources) loader
    (if-let ((resource (gethash id resources)))
      (car resource)
      (error "Resource with id ~A not found" id))))


(defun register-resource-package (package-name path)
  (setf (assoc-value *resouce-packages* (find-package package-name)) path))


(defun %mount-resources (&rest resource-names)
  (let ((package-table (alist-hash-table *resouce-packages*)))
    (loop for id in resource-names
       as (nil path) = (assoc-value *resources* id)
       as base-path = (or (gethash (symbol-package id) package-table) *default-pathname-defaults*)
       do (ge.rsc:mount-filesystem (game-resource-path id) (merge-pathnames path base-path))
       collect id)))


(defun %mount-packages (&rest package-names)
  (let ((package-table (alist-hash-table *resouce-packages*)))
    (loop for package-name in package-names
          as package = (find-package package-name)
          as base-path = (or (gethash package package-table) *default-pathname-defaults*)
          append (loop for (id nil path) in *resources*
                       when (eq package (symbol-package id))
                         do (ge.rsc:mount-filesystem (game-resource-path id) (merge-pathnames path base-path))
                         and
                           collect id))))


(defun autoprepare (resource-id)
  (when (ge.app:app)
    (%mount-resources resource-id)
    (prepare-resources resource-id)))


(defun register-game-resource (id path parameters &rest handler-args)
  (check-type id symbol)
  (let ((resource-path (game-resource-path id)))
    (ge.rsc:register-resource resource-path
                              (apply #'ge.rsc:make-resource-handler handler-args))
    (setf (assoc-value *resources* id) (append (list (first handler-args) path)
                                               parameters))))


(defun import-image (resource-id path)
  (register-game-resource resource-id path :image :type :png))


(defun import-sound (resource-id path)
  (register-game-resource resource-id path :audio))


(defmacro define-image (name path &key use-nearest-interpolation)
  (once-only (name)
    `(progn
       (register-game-resource ,name ,path
                               `(:use-nearest-interpolation ,,use-nearest-interpolation)
                               :image :type :png)
       (autoprepare ,name))))


(defmacro define-sound (name path)
  (once-only (name)
    `(progn
       (register-game-resource ,name ,path () :audio)
       (autoprepare ,name))))


(defmacro define-font (name path)
  (once-only (name)
    `(progn
       (register-game-resource ,name ,path () :font :type :ttf)
       (autoprepare ,name))))
