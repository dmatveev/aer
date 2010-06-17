(defmacro conc (sequence-a sequence-b)
  `(concatenate 'vector ,sequence-a ,sequence-b))

(defmacro is-first (x)
  `(= , x 1))

(defmacro is-extremum (e)
  `(or (eql ,e 'min) (eql ,e 'max)))

(defun sequence-last (sequence n)
  (subseq sequence (- (length sequence) n)))

(defun last-2 (sequence)
  (sequence-last sequence 2))

(defun collect-diffs (sequence-a sequence-b)
  (loop :for a :across sequence-a :for b :across sequence-b :collect
     (abs (- a b))))

(defun collect-slopes (sequence)
  (loop
     :with prev := (elt sequence 0) :for each :across (subseq sequence 1)
     :collect (let ((slope (- each prev)))
                (setq prev each)
                slope)))

(defun collect-extremums (sequence)
  (loop
     :with prev := (elt sequence 0) :for this :in (subseq sequence 1)
     :collect (let ((product (* prev this)))
                (setq prev this)
                (if (< product 0.0) (if (< this 0.0) 'max 'min) 'nop))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct date year month day)

  (defun tokenize (string token)
    (loop :for start := 0 :then (1+ finish)
       :for finish := (position token string :start start)
       :collecting (subseq string start finish)
       :until (null finish))))

(defun date-from (string)
  (let ((tokens (tokenize string #\-)))
    (make-date :year (read-from-string (elt tokens 0))
               :month (read-from-string (elt tokens 1))
               :day (read-from-string (elt tokens 2)))))

(defun season-of (date)
  (let ((month (date-month date)))
    (cond ((or  (<= month 2) (= month 12))  'winter)
		  ((and (>= month 3) (<= month 5))  'spring)
		  ((and (>= month 6) (<= month 8))  'summer)
		  ((and (>= month 9) (<= month 11)) 'autumn))))

(defun position-in-season (date)
  (let ((season (season-of date))
        (month (date-month date)))
    (cond ((eql season 'winter) (mod month 12))
          ((eql season 'spring) (- month 3))
          ((eql season 'summer) (- month 6))
          ((eql season 'autumn) (- month 9)))))

(defun encode-position (pos)
  (elt #(-0.3 0.0 0.3) pos))