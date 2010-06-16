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