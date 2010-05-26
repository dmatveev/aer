(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defgeneric matrix* (left right)
  (:documentation "Multiply two objects"))

(defgeneric matrix+ (left right)
  (:documentation "Matrix sum"))

(defgeneric matrix- (left right)
  (:documentation "Matrix sub"))

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

(defmacro matrix-dimensions (matrix)
  `(array-dimensions (data ,matrix)))

(defmacro matrix-reference (matrix row col)
  `(aref (data ,matrix) ,row ,col))

(defun matrix-ref (self row col)
  (matrix-reference self row col))

(defun (setf matrix-ref) (value matrix row col)
  (setf (matrix-reference matrix row col) value))

(defmacro matrix-do ((row-index rows col-index cols) &body body)
  `(dotimes (,row-index ,rows)
     (dotimes (,col-index ,cols)
       ,@body)))

(defmacro matrix-tabulate ((matrix row-arg col-arg) &body body)
  `(progn
     (matrix-do (,row-arg (matrix-rows ,matrix) ,col-arg (matrix-cols ,matrix))
       (setf (matrix-ref ,matrix ,row-arg ,col-arg) ,@body))
     ,matrix))

(defmacro matrix-create-tabulated ((row-arg rows-form col-arg cols-form) &body body)
  (with-gensyms (result)
    `(let ((,result (make-instance 'matrix :rows ,rows-form :cols ,cols-form)))
       (matrix-tabulate (,result ,row-arg ,col-arg) ,@body))))

(defun matrix-transpose (matrix)
  (matrix-create-tabulated (i (matrix-cols matrix) j (matrix-rows matrix))
    (matrix-ref matrix j i)))

(defun matrix-ebye (top left operator)
  (matrix-create-tabulated (i (matrix-rows left) j (matrix-cols top))
    (funcall operator  (matrix-ref top 0 j) (matrix-ref left i 0))))

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

(defmethod matrix+ ((left matrix) (right matrix))
  (if (equal (matrix-dimensions left) (matrix-dimensions right))
      (matrix-create-tabulated (result-i (matrix-rows left) result-j (matrix-cols right))
        (+ (matrix-ref left result-i result-j)
           (matrix-ref right result-i result-j)))
      (error "Operation can not be performed")))

(defmethod matrix- ((left matrix) (right matrix))
  (if (equal (matrix-dimensions left) (matrix-dimensions right))
      (matrix-create-tabulated (result-i (matrix-rows left) result-j (matrix-cols right))
        (- (matrix-ref left result-i result-j) (matrix-ref right result-i result-j)))
      (error "Operation can not be performed")))

(defmethod matrix* ((left matrix) (right real))
  (matrix-create-tabulated (result-i (matrix-rows left) result-j (matrix-cols left))
    (* right (matrix-ref left result-i result-j))))

(defun matrix+= (a b)
  (matrix-tabulate (a i j) (+ (matrix-ref a i j) (matrix-ref b i j))))

(defun matrix-inject (matrix closure)
  (let ((result 0))
    (destructuring-bind (rows cols) (matrix-dimensions matrix)
      (matrix-do (i rows j cols)
        (setf result (funcall closure result (matrix-ref matrix i j)))))
    result))

(defmethod print-object ((object matrix) stream)
  (format stream "~a" (data object)))
