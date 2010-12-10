(defstruct material input output)


(defstruct activation function derivative)

(defun sigmoid-fcn (x) (+ -0.5 (/ 1.0 (+ 1 (exp (- x))))))
(defun sigmoid-der (x) (* (+ x 0.5) (- 1.5 x))) 
(defun sigmoid () (make-activation :function #'sigmoid-fcn
                                   :derivative #'sigmoid-der))

(defun hypertan-fcn (x) (* 0.5 (tanh x)))
(defun hypertan-der (x) (- 1 (* x x)))
(defun hypertan () (make-activation :function #'hypertan-fcn
                                    :derivative #'hypertan-der))

(defun linear-fcn (x) x)
(defun linear-der (x) (declare (ignore x)) 1)
(defun linear () (make-activation :function #'linear-fcn
                                  :derivative #'linear-der))


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
