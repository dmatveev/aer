(defun make-material (&key (input #()) (output #()))
  (cons input output))

(defun material-input (material)
  (car material))

(defun material-output (material)
  (cdr material))