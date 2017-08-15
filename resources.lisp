(in-package :trivial-gamekit)


(declaim (special *resource-loader*))


(defclass gamekit-resource-loader ()
  ((resources :initform (make-hash-table :test 'equal))))


(defun %load-sound (path)
  (>> (concurrently ()
        (load-ogg-vorbis-audio path))
      (-> ((audio)) (sound)
        (let ((source (ge.snd:make-audio-source)))
          (with-disposable ((buffer (ge.snd:make-audio-buffer sound)))
            (ge.snd:attach-audio-buffer buffer source))
          source))))


(defun %load-resource (resource)
  (let ((type (car resource))
        (path (cdr resource)))
    (switch (type :test #'eq)
      (:image (concurrently ()
                (load-png-image path)))
      (:sound (%load-sound path)))))


(defun preloading-flow (loader)
  (with-slots (resources) loader
    (let ((ids (loop for resource-id being the hash-key in resources collect resource-id)))
      (>> (~> (loop for id in ids collect (%load-resource (gethash id resources))))
          (concurrently (results)
            (loop for id in ids
               for result in (first results)
               do (setf (gethash id resources) result)))))))


(defun %register-resource (loader type id path)
  (with-slots (resources) loader
    (setf (gethash id resources) (cons type path))))


(defun %get-resource (loader id)
  (with-slots (resources) loader
    (gethash id resources)))


(defun import-image (resource-id path)
  (%register-resource *resource-loader* :image resource-id path))


(defun import-sound (resource-id path)
  (%register-resource *resource-loader* :sound resource-id path))
