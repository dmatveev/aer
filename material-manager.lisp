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
    (vector-push material materials)))

(defun pixel-transform (pixel)
  (cdr (assoc pixel '((_ . -0.5) (* . 0.5)))))

(defun encode-image (image)
  (let* ((image-size (array-dimensions image))
         (image-rows (first image-size))
         (image-cols (second image-size))
         (material (make-instance 'matrix :cols (* image-cols image-rows))))
    (matrix-do (i image-rows j image-cols)
      (setf (matrix-ref material 0 (+ (* i image-cols) j))
            (pixel-transform (aref image i j))))
    material)) 

(defun encode-recognized (recognized)
  (make-array (length recognized)
              :initial-contents (loop for pixel across recognized collecting
                                     (pixel-transform pixel))))

(defun add-image (manager source recognized)
  (add-material manager (make-material :input (encode-image source)
                                       :output (encode-recognized recognized))))