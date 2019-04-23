(cl:in-package :trivial-gamekit)


(defun start-polling-controllers (frequency)
  (let ((executor (acquire-executor :single-threaded-p t :exclusive-p t)))
    (flet (())
     (execute executor ()))))
