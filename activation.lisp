(defun make-activation (function differencial)
  (cons function differencial))

(defun activation-function (activation)
  (car activation))

(defun activation-differencial (activation)
  (cdr activation))


;; prefefined activation functions
(defun sigmoid ()
  (make-activation #'(lambda (x) (/ 1 (+ 1 (exp (- x)))))
                   #'(lambda (x) (* x (- 1 x)))))