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


(defun numeric-encode (value min-limit max-limit &optional (compress-factor 1))
  (let ((range (- max-limit min-limit)))
    (* compress-factor (/ (- value (+ (/ range 2) min-limit)) range))))  

(defun numeric-decode (value min-limit max-limit &optional (compress-factor 1))
  (let ((range (- max-limit min-limit)))
    (+ min-limit (/ range 2) (* value (/ range compress-factor)))))

(defgeneric time-series-form (from-sequence policy)
  (:documentation "Form a time series from the sequence"))

(defmethod time-series-form (from-sequence (policy value-policy))
  (coerce from-sequence 'list))

(defgeneric fore-series-form (prev-sequence fore-sequence policy)
  (:documentation "TBD"))

(defmethod fore-series-form (prev-sequence fore-sequence (policy value-policy))
  (declare (ignore prev-sequence policy))
  (coerce fore-sequence 'list))

(defun encode-input-series (past-data depth pos extractor policy)
  (let* ((result (make-instance 'matrix :cols (1+ depth))))
    (encode-time-series (time-series-form past-data policy) result extractor policy)
    (setf (matrix-ref result 0 depth) (encode-position pos))
    result))

(defun make-series-material (data depth pos extractor policy)
  (let* ((past-data (subseq data 0 depth))
         (fore-data (subseq data depth))
         (encoded-past (make-instance 'matrix :cols (1+ depth)))
         (encoded-fore (make-array (length fore-data))))
    (encode-time-series (time-series-form past-data policy) encoded-past extractor policy)
    (setf (matrix-ref encoded-past 0 depth) (encode-position pos))
    (encode-forecast (fore-series-form past-data fore-data policy)
                     encoded-fore extractor policy)
    (make-material :input encoded-past :output encoded-fore)))
