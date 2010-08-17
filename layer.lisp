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


(defclass backprop ()
  ())

(defgeneric process (neural-object input)
  (:documentation "Process the input vector"))

(defgeneric calculate-deltas (layer context)
  (:documentation "Calculate the deltas"))

(defgeneric apply-corrections (neural-object scheme)
  (:documentation "Apply the calculated weight corrections"))

(defgeneric calculate-corrections (network layer next-outputs scheme)
  (:documentation "Calculate the corrections for the layer synapse weights"))

(defstruct bpcontext target layer errors)

(defclass layer ()
  ((outputs :reader outputs)))

(defclass educable-layer (layer)
  ((activation :reader activation :initarg :activation :initform (sigmoid))
   (weights :reader weights)
   (corrections)
   (deltas :reader deltas)))

(defclass input-layer  (layer) ())
(defclass hidden-layer (educable-layer) ())
(defclass output-layer (educable-layer) ())

(defmethod initialize-instance :after ((instance educable-layer) &key (inputs 1) (neurons 1))
  (with-slots (outputs weights corrections deltas) instance
    (setf weights (matrix-create-tabulated (row inputs col neurons) (- 0.5 (random 1.0)))
          outputs (make-instance 'matrix :cols neurons)
          deltas (make-instance 'matrix :cols neurons)
          corrections (make-instance 'matrix :rows inputs :cols neurons)))) 

(defmethod process ((instance educable-layer) (input matrix))
  (with-slots (activation outputs weights) instance
    (matrix-*-into outputs input weights)
    (matrix-collect-into outputs (activation-function activation))))

(defmethod process ((instance input-layer) (input matrix))
  (setf (slot-value instance 'outputs) input))

(defmethod calculate-deltas ((layer hidden-layer) context)
  (with-slots (outputs deltas activation) layer
    (with-slots ((prev-layer layer)) context
      (let ((derivative   (activation-derivative activation))
            (prev-deltas  (deltas prev-layer))
            (prev-weights (weights prev-layer)))
        (matrix-tabulate (deltas i j)
          (* (funcall derivative (matrix-ref outputs 0 j))
             (loop :for c :from 0 :to (1- (matrix-cols prev-weights)) :summing
                  (* (matrix-ref prev-deltas  0 c)
                     (matrix-ref prev-weights j c)))))))))

(defmethod calculate-deltas :before ((layer output-layer) context)
  (with-slots (outputs) layer
    (with-slots (target errors) context
      (matrix-tabulate (errors i j)
        (- (aref target j) (matrix-ref outputs 0 j))))))

(defmethod calculate-deltas ((layer output-layer) context)
  (with-slots (outputs deltas activation) layer
    (with-slots (errors) context
      (let ((derivative (activation-derivative activation)))
        (matrix-tabulate (deltas i j)
          (* (funcall derivative (matrix-ref outputs 0 j))
             (matrix-ref errors 0 j)))))))

(defmethod calculate-corrections (network layer next-outputs (scheme backprop))
  (with-slots ((nju-param speed)) network
    (with-slots (deltas corrections) layer
      (matrix-tabulate (corrections i j)
        (* nju-param (matrix-ref next-outputs 0 i) (matrix-ref deltas 0 j))))))

(defmethod apply-corrections ((instance educable-layer) scheme)
  (with-slots (weights corrections) instance
    (matrix+= weights corrections)))

(defun reset-corrections (layer)
  (with-slots (corrections) layer
    (matrix-tabulate (corrections i j) 0)))

(defmethod print-object ((object layer) stream)
  (with-slots (weights activation) object
      (format stream "<~a inputs: ~a neurons: ~a>"
              (class-of object)
              (matrix-rows weights)
              (matrix-cols weights))))
