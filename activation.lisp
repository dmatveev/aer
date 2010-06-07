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
