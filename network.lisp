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


(defgeneric backprop-learn (network material scheme)
  (:documentation "Perform a learning iteration"))

(defgeneric process-epoch (network store precision scheme)
  (:documentation "Process a learning epoch"))

(defclass network ()
  ((layers :initform (make-array 5 :adjustable t :fill-pointer 0) :reader layers)
   (speed  :initform 0.1 :initarg :speed :reader learning-speed)))

(defmacro add-layer (container type &optional (initargs '()))
  `(vector-push-extend (apply #'make-instance (cons ,type ,initargs)) ,container))

(defmacro lastp (cell)
  `(eql (second ,cell) nil))

(defmethod initialize-instance :after ((instance network)
                                       &key (inputs 1) (config '((:neurons 1))))
  (with-slots (layers) instance
    (add-layer layers 'input-layer)
    (let ((prev-layer-outputs inputs))
      (flet ((build (args)
               (add-layer layers
                          (if (lastp args) 'output-layer 'hidden-layer)
                          (append (car args) `(:inputs ,prev-layer-outputs)))
               (setf prev-layer-outputs (getf (car args) :neurons))))
        (maplist #'build config)))))

(defmethod process ((instance network) (input matrix))
  (let ((prev-layer-output input))
    (loop :for layer :across (layers instance) :do
      (setf prev-layer-output (process layer prev-layer-output)))
    prev-layer-output))

(defmethod apply-corrections ((instance network) scheme)
  (loop
     :with layers := (subseq (layers instance) 1)
     :for layer :across layers :do (apply-corrections layer scheme)))
 
(defun collect-corrections (network context scheme)
  (loop
     :with layers := (reverse (layers network))
     :for current-layer :across layers 
     :for next-layer :across (subseq layers 1) :do
       (with-slots ((prev-layer layer)) context
         (calculate-deltas current-layer context)
         (setf prev-layer current-layer)
         (calculate-corrections network current-layer (outputs next-layer) scheme))))

(defmethod backprop-learn (network material scheme)
  (process network (material-input material))
  (let* ((target (material-output material))
         (context (make-bpcontext
                   :target target
                   :errors (make-instance 'matrix :cols (length target)))))
    (collect-corrections network context scheme)
    (bpcontext-errors context)))

(defmethod backprop-learn :after (network material (scheme backprop))
  (apply-corrections network scheme))

(defun rms-helper (a x) (+ a (* x x)))

(defmethod process-epoch (network store precision scheme)
  (declare (ignore precision))
  (with-slots (materials) store
    (loop :for material :across materials :summing
           (matrix-inject (backprop-learn network material scheme)
                          #'rms-helper)
         :into s
         :finally (return (/ s (length materials))))))

(defun train (network store precision scheme)
  (do ((counter 0 (1+ counter)))
      ((process-epoch network store precision scheme) counter)))

(defun train-times (network store times scheme
                    &key (verbose-period 100 vp-s))
  (do ((counter 0 (1+ counter)))
      ((= counter times) times)
    (let ((err (process-epoch network store 0.01 scheme)))
      (if (and vp-s (= (mod counter verbose-period) 0))
          (format t "Epoch: ~a; error: ~a~%" counter err)))))
