(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defgeneric matrix* (left right)
  (:documentation "Multiply two objects"))

(defgeneric matrix-ref (matrix i j)
  (:documentation "Reference a matrix cell"))

(defclass matrix ()
  ((data :reader data)))

(defmethod initialize-instance :after ((self matrix) &key
                                       (rows 1) (cols 1) (initial-element 0)
                                       (from #2A((0)) from-supplied-p))
  (setf (slot-value self 'data)
        (if from-supplied-p
            from
            (make-array `(,rows ,cols) :initial-element initial-element))))

(defmacro matrix-rows (matrix)
  `(first (array-dimensions (data ,matrix))))

(defmacro matrix-cols (matrix)
  `(second (array-dimensions (data ,matrix))))

(defmacro dimensions (matrix)
  `(list (matrix-rows ,matrix) (matrix-cols ,matrix)))

(defmacro matrix-reference (matrix row col)
  `(aref (data ,matrix) ,row ,col))

(defmethod matrix-ref ((self matrix) row col)
  (matrix-reference self row col))

(defun (setf matrix-ref) (value matrix row col)
  (setf (matrix-reference matrix row col) value))

(defmacro matrix-do ((row-index rows col-index cols) &body body)
  `(dotimes (,row-index ,rows)
     (dotimes (,col-index ,cols)
       ,@body)))

(defmacro matrix-create-tabulated ((row-arg rows-form col-arg cols-form) &body body)
  (with-gensyms (result)
    `(let ((,result (make-instance 'matrix :rows ,rows-form :cols ,cols-form)))
       (matrix-tabulate (,result ,row-arg ,col-arg) ,@body))))

(defmacro matrix-tabulate ((matrix row-arg col-arg) &body body)
  `(progn
	 (matrix-do (,row-arg (matrix-rows ,matrix) ,col-arg (matrix-cols ,matrix))
	   (setf (matrix-ref ,matrix ,row-arg ,col-arg) ,@body))
	 ,matrix))

(defmacro matrix-collect (matrix closure)
  `(matrix-create-tabulated (i (matrix-rows ,matrix) j (matrix-cols ,matrix))
	 (funcall ,closure (matrix-ref ,matrix i j))))

(defmethod matrix* ((left matrix) (right matrix))
  (if (equal (matrix-cols left) (matrix-rows right))
      (matrix-create-tabulated (result-i (matrix-rows left) result-j (matrix-cols right))
        (do ((i 0 (1+ i))
             (s 0 (+ s (* (matrix-ref left result-i i)
						  (matrix-ref right i result-j))))) 
            ((= i (matrix-cols left)) s)))
      (error "Operation can not be performed")))

(defmethod matrix* ((left matrix) (right rational))
  (matrix-create-tabulated (result-i (matrix-rows left) result-j (matrix-cols left))
    (* right (matrix-ref left result-i result-j))))

(defmethod print-object ((object matrix) stream)
  (format stream "~a" (data object)))
