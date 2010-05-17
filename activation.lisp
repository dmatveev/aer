(defun make-activation (function differencial)
  (cons function differencial))

(defun activation-function (activation)
  (car activation))

(defun activation-differencial (activation)
  (cdr activation))

;; prefefined activation functions
(defun sigmoid ()
  (flet ((sigmoid-f (x) (+ -0.5 (/ 1.0 (+ 1 (exp (- x)))))))
    (make-activation #'sigmoid-f
                     #'(lambda (x) (* (+ x 0.5) (- 1.5 x))))))