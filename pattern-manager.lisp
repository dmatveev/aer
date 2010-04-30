(defclass pattern-manager ()
  ((patterns :initform (make-array 5 :adjustable t :fill-pointer 0))
   (marker :initform 0)))

(defun get-next-pattern (manager)
  (with-slots (marker patterns) manager
	(let ((pattern (aref patterns marker)))
	  (if (>= (incf marker) (length patterns))
		  (setf marker 0))
	  pattern)))

(defun add-pattern (manager pattern)
  (with-slots (patterns) manager
	(vector-push pattern patterns)))

(defun pixel-transform (pixel)
  (cdr (assoc pixel '((_ . 0) (* . 1)))))

(defun encode-image (image)
  (let* ((image-size (array-dimensions image))
		 (image-rows (first image-size))
		 (image-cols (second image-size))
		 (pattern (make-array (* image-cols image-rows))))
	(matrix-do (i image-rows j image-cols)
	  (setf (aref pattern (+ (* i image-cols) j)) (pixel-transform (aref image i j))))
	pattern)) 

(defun add-image (manager image)
  (add-pattern manager (encode-image image)))