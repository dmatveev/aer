(defun make-activation (function differencial)
  (cons function differencial))

(defun activation-function (activation)
  (car activation))

(defun activation-differencial (activation)
  (cdr activation))

;;prefefined activation functions
(defun sigmoid-fcn (x) (+ -0.5 (/ 1.0 (+ 1 (exp (- x))))))
(defun sigmoid-der (x) (* (+ x 0.5) (- 1.5 x))) 
(defun sigmoid () (make-activation #'sigmoid-fcn #'sigmoid-der))

(defun hypertan-fcn (x) (* 0.5 (tanh x)))
(defun hypertan-der (x) (- 1 (* x x)))
(defun hypertan () (make-activation #'hypertan-fcn #'hypertan-der))

(defun linear-fcn (x) x)
(defun linear-der (x) (declare (ignore x)) 1)
(defun linear () (make-activation #'linear-fcn #'linear-der))

