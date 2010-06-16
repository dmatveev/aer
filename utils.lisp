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
