(defclass material-manager ()
  ((materials :initform (make-array 5 :adjustable t :fill-pointer 0))
   (marker :initform 0)))

(defun get-next-material (manager)
  (with-slots (marker materials) manager
    (let ((material (aref materials marker)))
      (if (>= (incf marker) (length materials))
          (setf marker 0))
      material)))

(defun add-material (manager material)
  (with-slots (materials) manager
    (vector-push-extend material materials)))
