(cl:in-package :trivial-gamekit)


(declaim (special *resource-registry*))


(defvar *gamekit-instance-class* nil)


(define-constant +game-resource-root+ "/_asset/"
  :test #'equal)

(define-constant +kit-resource-root+ "/_gamekit/"
  :test #'equal)

(define-constant +font-name+ (format nil "~A~A" +kit-resource-root+ "NotoMono-Regular")
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


(defresource :font +font-name+
  :type :ttf
  :path (kit-asset-path "NotoSans-Regular.ttf"))


(defclass gamekit-resource-registry ()
  ((resources :initform (make-hash-table :test 'equal))))


(defun %load-sound (resource-name)
  (flow:>>
   (resource-flow resource-name)
   (concurrently ((sound))
     (let* ((source (ge.snd:make-audio-source)))
       (with-disposable ((buffer (ge.snd:make-audio-buffer sound)))
         (ge.snd:attach-audio-buffer buffer source))
       source))))


(defun %load-image (resource-name canvas-provider &key)
  (flow:>>
   (resource-flow resource-name)
   (for-graphics ((image))
     (ge.vg:make-image-paint (funcall canvas-provider) image))))


(defun %load-font (resource-name canvas-provider &key)
  (flow:>>
   (resource-flow resource-name)
   (for-graphics ((font-face))
     (ge.vg:register-font-face (funcall canvas-provider) resource-name font-face))))


(defun %load-resource (resource-name type canvas-provider)
  (eswitch (type :test #'eq)
    (:image (%load-image resource-name canvas-provider))
    (:audio (%load-sound resource-name))
    (:font (%load-font resource-name canvas-provider))))


(defun loading-flow (loader canvas-provider resource-names)
  (with-slots (resources) loader
    (unless (ge.ng:executablep)
      (apply #'%mount-resources resource-names))
    (flow:>>
     (flow:~> (loop for (id type) in *resources*
                    when (member id resource-names :test #'eq)
                      collect (when-let ((id id)
                                         (type type)
                                         (resource-path (game-resource-path id)))
                                (flow:>> (%load-resource resource-path type canvas-provider)
                                         (instantly (resource)
                                           (cons id resource))))))
     (concurrently ((results))
       (loop for (id . resource) in results
             do (setf (gethash id resources) resource))))))


(defun list-all-resources ()
  (mapcar #'car *resources*))

(defun %register-resource (loader type id path &rest options &key &allow-other-keys)
  (with-slots (resources) loader
    (setf (gethash id resources) (nconc (list (cons type path)) options))))


(defun %get-resource (loader id)
  (with-slots (resources) loader
    (if-let ((resource (gethash id resources)))
      resource
      (error "Resource with id ~A not found" id))))


(defun register-resource-package (package-name path)
  "Associates resource package with filesystem path. For proper resource handling it is
recommended to put it as a top-level form, so resources could be located at load-time.

First argument, a package name, must be a valid Common Lisp package name that could be used to
locate package via #'find-package. Second argument is a filesystem path to a directory where
resources can be found.

Example:
```common_lisp
(gamekit:register-resource-package :example-package
                                   \"/home/gamdev/example-game/assets/\")
```"
  (setf (assoc-value *resouce-packages* (find-package package-name)) path))


(defun %mount-resources (&rest resource-names)
  (let ((package-table (alist-hash-table *resouce-packages*)))
    (loop for id in resource-names
       as (nil path) = (assoc-value *resources* id)
       as base-path = (or (gethash (symbol-package id) package-table) *default-pathname-defaults*)
       do (mount-filesystem (game-resource-path id) (merge-pathnames path base-path))
       collect id)))


(defun %mount-packages (&rest package-names)
  (let ((package-table (alist-hash-table *resouce-packages*)))
    (loop for package-name in package-names
       as package = (find-package package-name)
       as base-path = (or (gethash package package-table) *default-pathname-defaults*)
       append (loop for (id nil path) in *resources*
                 when (eq package (symbol-package id))
                 do (mount-filesystem (game-resource-path id) (merge-pathnames path base-path))
                 and
                 collect id))))


(defun autoprepare (resource-id)
  (when *gamekit-instance-class*
    (%mount-resources resource-id)
    (prepare-resources resource-id)))


(defun register-game-resource (id path &rest handler-args)
  (check-type id symbol)
  (let ((resource-path (game-resource-path id)))
    (register-resource resource-path
                       (apply #'make-resource-handler handler-args))
    (setf (assoc-value *resources* id) (list (first handler-args) path))))


(defun import-image (resource-id path)
  (register-game-resource resource-id path :image :type :png))


(defun import-sound (resource-id path)
  (register-game-resource resource-id path :audio))


(defmacro define-image (name path)
  "Registers image resource by name that can be used by [`#'draw-image`](#gamekit-draw-image)
later. Second argument is a valid path to the resource.  Only .png images are supported at this
moment.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be used to locate the
resource, if relative path is given as an argument to this macro.

Example:
```common_lisp
(gamekit:define-image 'example-package::logo \"images/logo.png\")
```"
  (once-only (name)
    `(progn
       (register-game-resource ,name ,path :image :type :png)
       (autoprepare ,name))))


(defmacro define-sound (name path)
  "Registers sound resource by name that can be used by [`#'play-sound`](#gamekit-play-sound) later.
Second argument is a valid path to the resource.  Formats supported: .wav, .ogg (Vorbis), .flac,
.aiff.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be used to locate the
resource, if relative path is given as an argument to this macro.

Example:
```common_lisp
(gamekit:define-sound 'example-package::blop \"sounds/blop.ogg\")
```"
  (once-only (name)
    `(progn
       (register-game-resource ,name ,path :audio)
       (autoprepare ,name))))


(defmacro define-font (name path)
  "Registers font resource by name that can be passed to [`#'make-font`](#gamekit-make-font) later.
Second argument is a valid path to the resource. Only .ttf format is supported at this moment.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be used to locate the
resource, if relative path is given as an argument to this macro.

Example:
```common_lisp
(gamekit:define-font 'example-package::noto-sans \"fonts/NotoSans-Regular.ttf\")
```"  (once-only (name)
    `(progn
       (register-game-resource ,name ,path :font :type :ttf)
       (autoprepare ,name))))
