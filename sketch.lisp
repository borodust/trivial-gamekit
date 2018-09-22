(cl:in-package :trivial-gamekit)


(defvar *sketch-list* nil)


(defgeneric %render-sketch (sketch))


(defmacro defsketch (name-and-opts &body body)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (declare (ignore opts))
    (with-gensyms (this)
      `(progn
         (defmethod %render-sketch ((,this (eql ',name)))
           (declare (ignore ,this))
           (with-pushed-canvas ()
             ,@body))
         (pushnew ',name *sketch-list*)))))


(defun remove-sketch (sketch-name)
  (deletef *sketch-list* sketch-name))


(defun remove-all-sketches ()
  (setf *sketch-list* nil))
