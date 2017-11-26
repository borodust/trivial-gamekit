(cl:defpackage :trivial-gamekit.documentation
  (:nicknames :gamekit.documentation)
  (:use :cl :gamekit)
  (:export render-documentation))
(cl:in-package :trivial-gamekit.documentation)


(defclass kramdown-renderer () ())


(defparameter *template* (alexandria:read-file-into-string
                          (asdf:system-relative-pathname :trivial-gamekit/documentation
                                                         "documentation-entry.template")))

(defun format-documentation-entry (type symbol documentation &optional lambda-list)
  (let ((mustache:*escape-tokens* nil))
    (mustache:render* *template* (alexandria:plist-alist
                                  (append (list :type type
                                                :name (string-downcase (symbol-name symbol)))
                                          (when documentation
                                            (list :documentation documentation))
                                          (list :lambda-list
                                                (format nil "(~{~(~A~)~^ ~})" lambda-list)))))))


(defmethod doxy:document-class ((this kramdown-renderer) name docstring)
  nil)

(defmethod doxy:document-function ((this kramdown-renderer) name lambda-list docstring)
  (format-documentation-entry "function" name docstring lambda-list))

(defmethod doxy:document-macro ((this kramdown-renderer) name lambda-list docstring)
  (format-documentation-entry "macro" name docstring lambda-list))

(defmethod doxy:document-generic ((this kramdown-renderer) name lambda-list docstring)
  (format-documentation-entry "generic" name docstring lambda-list))

(defmethod doxy:document-variable ((this kramdown-renderer) name docstring)
  (format-documentation-entry "variable" name docstring))


(defun render-documentation (&key (overwrite t))
  (let ((renderer (make-instance 'kramdown-renderer))
        (output-directory (asdf:system-relative-pathname :trivial-gamekit/documentation "docs/")))
    (ensure-directories-exist output-directory)
    (flet ((%render-documentation (file &rest symbols)
             (alexandria:with-output-to-file (output (merge-pathnames file output-directory)
                                                     :if-exists (if overwrite
                                                                    :supersede
                                                                    :error))
               (loop for (nil . doc) in (apply #'doxy:collect-documentation renderer symbols)
                  when doc
                  do (format output "~A~&~%" doc)))))
      (%render-documentation "defining-a-game.md" 'defgame 'start 'stop 'gamekit))))
