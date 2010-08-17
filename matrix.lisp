;; This file is a part of the aer forecasting system
;;
;; Copyright (c) 2010 Dmitry Matveev <dmatveev@inbox.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

(defclass matrix ()
  ((data :reader data)))

(defmethod initialize-instance :after ((self matrix) &key
                                       (rows 1) (cols 1) (initial-element 0)
                                       (from #2A((0)) from-supplied-p))
  (setf (slot-value self 'data)
        (if from-supplied-p from
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

(defun matrix-collect-into (matrix closure)
  (matrix-tabulate (matrix i j)
    (funcall closure (matrix-ref matrix i j))))

(defun matrix-*-into (target left right)
  (if (and (equal (matrix-cols target) (matrix-cols right))
           (equal (matrix-rows target) (matrix-rows left)))
      (matrix-tabulate (target t-i t-j)
        (do ((i 0 (1+ i))
             (s 0 (+ s (* (matrix-ref left t-i i)
                          (matrix-ref right i t-j))))) 
            ((= i (matrix-cols left)) s)))        
      (error "Operation can not be performed")))

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
