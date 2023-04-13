;;;
;;; karo
;;;
;;; Copyright (c) 2014, Ola Rinta-Koski & Daniel Schell
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;;     Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;; 
;;;     Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; See 
;;; https://ola.rinta-koski.net/karo/
;;; for more info

;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP")

#+cormanlisp
(defun cd (new-directory)
  (setf (ccl:current-directory) (pathname new-directory)))

(defpackage :karo
  (:use :common-lisp)
  (:export #:about))

(in-package "KARO")

(defparameter *karo-version* "1.109")
(defparameter *karo-version-date* '("2023-04-13"
				    "08:00:00"))

(defvar *notes-in-group* 12)
(defvar *n-ad-size* 3)

(defparameter *group-sizes* '(6 9 12 19 24))
(defparameter *default-group-size* 12)

(defun z12-p () (= *notes-in-group* 12))
(defun z24-p () (= *notes-in-group* 24))

(defun factorial (x)
  (unless (and (integerp x) (>= x 0))
    (error "X has to be a positive integer or 0, not ~A -- FACTORIAL" x))
  (cond ((>= x 1) (* x (factorial (1- x))))
	(t 1)))

(defun number-of-karos (&optional z n)
  (let ((z (or z *notes-in-group*))
	(n (or n *n-ad-size*)))
    (/ (factorial z) (* (factorial (/ z n)) (expt (factorial n) (/ z n))))))


(defconstant +max-z6-karo+ (number-of-karos 6)
  "Number of possible 6-note karos")
(defconstant +max-z9-karo+ (number-of-karos 9)
  "Number of possible 9-note karos")
(defconstant +max-z12-karo+ (number-of-karos 12)
  ;; 12! / (4! * (3!)^4) = 15400
  "Number of possible 12-note karos with chords of size 3")

(defmacro default (var value)
  `(unless ,var (setf ,var ,value)))

(defun max-karo (&optional group-size)
  (default group-size *notes-in-group*)
  (ecase group-size
    (6  +max-z6-karo+)
    (9  +max-z9-karo+)
    (12 +max-z12-karo+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug* nil)
(defvar *karo-composer* "Daniel Schell")


(defclass karo ()
  ((karo-index :accessor karo-index :initarg :karo-index)
   (notes :reader notes :initarg :notes)
   (analysis)
   (simple-notes)
   (distances)
   (connections :initform nil)))


(defun karo-p (x)
  (cond ((typep x 'karo) t)
	(t nil)))


(defun today ()
  (multiple-value-bind (s m h d mo y)
      (get-decoded-time)
    (declare (ignore s m h))
    (format nil "~4,,,'0@A-~2,,,'0@A-~2,,,'0@A" y mo d)))


(defmacro alias (alias fn)
  `(setf (symbol-function (quote ,alias)) (symbol-function (quote ,fn))))


(defun split (str &optional (ch #\Space) (quote #\"))
  "Converts STR into a list of strings, splitting it at every CH.
However, substrings limited by a pair of QUOTE characters won't
be split even if they contain an instance of CH."
  (let ((len (length str)))
    (when (> len 0)
      (loop for prev-pos = 0 then (1+ next-pos)
	 for next-pos = (cond ((>= prev-pos len)
			       nil)
			      ;;
			      ((eql (elt str prev-pos) quote)
			       (position ch str :start
					 (1+ (position quote str
						       :start (1+ prev-pos)))))
			      ;;
			      (t
			       (position ch str :start prev-pos)))
	 for result = (list (subseq str 0 next-pos)) then
	   (cons (subseq str prev-pos next-pos) result)
	 while next-pos
	 finally (return (nreverse result))))))


(defun karo-version ()
  (destructuring-bind (date time)
      *karo-version-date*
    (values *karo-version* date time)))


(defun karo-version-string ()
  (multiple-value-bind (version date time)
      (karo-version)
    (format nil "Karo Engine v~A ~A ~A" version date time)))


(defun about (&key (stream t))
  (multiple-value-bind (version date time)
      (karo-version)
    (format stream "~&Karo Engine v~A ~A ~A~%~
\(c) 2001-~A Daniel Schell and Ola Rinta-Koski~%~
http://ola.rinta-koski.net/karo/  for more info~%"
            version date time (first (split date #\-)))
    version))


(defun work-on (&optional (z 12) (n 3) force)
  "Configure Karo Engine to work on a Z group divided in n-ads of size N.

Arguments:
Z - group size (default 12)
N - n-ad size (default 3)

Example:
\(work-on 12 3) =>
configures KE to work on Z12 divided in 3-note chords (triads, the default)"
  (unless (or force
	      (and (integerp z) (integerp n) (> z 0) (> n 0)
		   (>= z n) (= (mod z n) 0)))
    (error "Can't work on Z~A divided into n-ads of ~A" z n))
  (setf *notes-in-group* z
	*n-ad-size* n)
  (princ (format nil "Z~D, chords of ~D~%" z n))
  t)


(defun read-chord (list-or-string)
  "Make sure a chord is given in list form.
If LIST-OR-STRING is a list, return that list. 
If LIST-OR-STRING is a string, return that string converted into a list.
The string is assumed to be of known format.
Examples:
\(read-chord \"1,1(1)\") => (1 2 3)
\(read-chord '(1 2 3)) => (1 2 3)"
  (etypecase list-or-string
    (list list-or-string)
    (string
     (let* ((as-list 
	     (read-from-string (format nil "(~A)" (substitute #\Space #\,
							      list-or-string))))
	    (base (caar (last as-list)))
	    (notes (butlast as-list)))
       (normalize (cons base
			(reverse (loop for a on (reverse notes)
				    collect (+ base (reduce #'+ a))))))))))


(defun read-chords (&rest chord-strings)
  (mapcan #'read-chord chord-strings))


(defun normalize-note (note &optional (group-size *notes-in-group*))
  (mod note group-size))


(defun normalize (chord &optional (group-size *notes-in-group*))
  "Bring all notes into one octave range"
  (mapcar #'(lambda (note)
              (normalize-note note group-size))
          (read-chord chord)))


(defun transpose (chord amount)
  "Transpose all notes of chord by AMOUNT"
  (mapcar #'(lambda (note)
              (+ note amount))
          (read-chord chord)))


(defun groupings (notes &optional (n-ad-size *n-ad-size*))
  "Returns all lexicographic groupings of N-AD-SIZE for NOTES

Example: (with *N-AD-SIZE* 3)
\(groupings '(1 2 3 4 5 6)) =>
\((1 2 3 4 5 6) (1 2 4 3 5 6) (1 2 5 3 4 6) (1 2 6 3 4 5) (1 3 4 2 5 6)
 (1 3 5 2 4 6) (1 3 6 2 4 5) (1 4 5 2 3 6) (1 4 6 2 3 5) (1 5 6 2 3 4))"
  (unless (= (rem (length notes) n-ad-size) 0)
    (error "Length of note list ~A must be multiple of ~A" notes n-ad-size))
  (if (= (length notes) n-ad-size)
      (list notes)
      ;;
      (flet ((difference (set-1 set-2)
	       ;; A version of SET-DIFFERENCE that guarantees
	       ;; original order
	       (loop for c in set-1
		  unless (find c set-2)
		  collect c)))
	(loop for j on (rest notes)
	   nconc (loop for k on (rest j)
		    for first-group = (list (first notes)
					    (first j)
					    (first k))
		    nconc (mapcar #'(lambda (x)
				      (append first-group x))
				  (groupings (difference notes first-group)
					     n-ad-size)))))))


#|
(defparameter *groups*
  (mapcar #'(lambda (x) (cons x (groupings (loop for i below x collect i))))
	  *group-sizes*))
|#

(let (all-groupings)
  (defun groups (group-size)
    (unless (assoc group-size all-groupings)
      (push (cons group-size 
		  (groupings (loop for i below group-size collect i)))
	    all-groupings))
    (assoc group-size all-groupings)))
  


(defun nth-group (index &optional (group-size *default-group-size*))
  "Returns the INDEXth lexicographic grouping of size GROUP-SIZE"
  (unless (find group-size *group-sizes*)
    (error "Group size must be in ~A, not ~A" *group-sizes* group-size))
  (nth index (cdr (groups group-size))))


(defun nth-tile (list nth)
  "Returns the NTH lexicographic grouping of LIST. NTH numbering
starts from 0."
  (let ((group (nth-group nth (length list))))
    (loop for i in group collect (nth i list))))


(defun permute (notes index)
  "Returns the INDEXth lexicographic permutation of NOTES"
  (let ((notes-length (length notes)))
    (unless (find notes-length *group-sizes*)
      (error "Notes list ~A length must be in ~A (not ~D)"
             notes *group-sizes* notes-length))
    (let ((group (nth-group index notes-length)))
      (mapcar #'(lambda (x) (nth x notes)) group))))


(defun list-permutations (list &key (remove-mirror-images nil))
  "All permutations of LIST"
  (let ((permutations
	 (cond ((null list)
		(list nil))
	       (t
		(mapcan #'(lambda (first) 
			    (mapcar #'(lambda (rest)
					(cons first rest))
				    (list-permutations (remove first list
							       :count 1 :test #'eq))))
			list)))))
    (if remove-mirror-images
	;; t
	(remove-duplicates permutations
			   :test #'(lambda (x y)
				     (equal x (reverse y)))
			   :from-end t)
	;; nil
	permutations)))


(defun list-permutations-unique (list)
  (list-permutations list :remove-mirror-images t))


(defun reference (chord &optional (group-size *notes-in-group*))
  "Expand CHORD so all notes are in ascending order
by adding octaves when note n+1 would be lower than note n"
  (let ((chord (read-chord chord)))
    (cons (first chord)
          (loop for note in (rest chord)
	     for previous-note in chord
	     with i = 0
	     collect (progn
		       (when (< note previous-note)
			 (incf i group-size))
		       (+ i note))))))


(defun adjust-range (note &optional (group-size *notes-in-group*))
  "Adjust NOTE to be in the range of [A,B], where A is 0 minus half of
GROUP-SIZE and B is half of GROUP-SIZE minus 1. For the default group
size of 12, this adjusts NOTE to be in the range of [-6,5]. The return
values are the adjusted note and T if the note was adjusted, NIL if
the note was already in the correct range."
  (let* ((range-end (- (1- group-size) (floor (/ group-size 2))))
         (folded-note (mod note group-size))
         (adjusted-note (if (> folded-note range-end)   
                            (- folded-note group-size)
			    folded-note)))
    (values adjusted-note (not (= note adjusted-note)))))


(defun adjust-list (list modifier)
  (mapcar #'(lambda (x) (+ x modifier)) list))


(defun first-octave-reference (chord &optional group-size)
  "Force middle note of triad to be in range 0..GROUP-SIZE.

Note: only works for triads."
  (default group-size *notes-in-group*)
  (let* ((r (reference chord))
	 (n (second r)))
    (cond ((< n 0)
	   (adjust-list r (* (- (floor (/ n group-size)) group-size))))
	  ((> n group-size)
	   (adjust-list r (- (* (floor (/ n group-size)) group-size))))
	  (t
	   r))))


(defun first-reference (chord &optional (group-size *notes-in-group*))
  "Expand CHORD as per 'reference' but ensure that
the middle voice is in the range (- (/ GROUP-SIZE 2)) .. (1- (/ GROUP-SIZE 2))

For example, if GROUP-SIZE is 12, middle voice is adjusted to -6..5"
  (let* ((referenced-chord (reference (read-chord chord)))
         (modifier (- (second referenced-chord)
                      (mod (second referenced-chord) group-size)
                      (if (> (mod (second referenced-chord) group-size)
                             (- (1- group-size) (floor (/ group-size 2))))
                          (- group-size) 0))))
    (mapcar #'(lambda (x) (- x modifier)) referenced-chord)))


(defun ambitus (chord)
  "Get the ambitus, i.e. the reach between the lowest and
the highest note, of the CHORD's expansion (reference)"
  (let ((chord-reference (reference (read-chord chord))))
    (- (first (last chord-reference))
       (first chord-reference))))


(let ((permutations-hash (make-hash-table)))
  (defun permutations-of (how-many)
    (unless (gethash how-many permutations-hash)
      (setf (gethash how-many permutations-hash)
            (list-permutations (loop for i below how-many
				  collect i))))
    (gethash how-many permutations-hash)))


(defun lowest-ambitus (a-chord &optional (notes-in-group *notes-in-group*))
  "Find the permutation with the lowest ambitus for given CHORD"
  (let* ((chord (stable-sort (read-chord a-chord)
			     #'(lambda (x y)
				 (< (mod x notes-in-group)
				    (mod y notes-in-group))))))
    (loop for n below (length chord)
       for candidate = chord then (nconc (copy-list (cdr candidate))
					 (list (car candidate)))
       for ambitus = (ambitus candidate)
       with winner = nil
       with winner-ambitus = most-positive-fixnum
       with all = nil
       do
	 (push (cons candidate ambitus) all)
	 (when (< ambitus winner-ambitus)
	   (setf winner candidate
		 winner-ambitus ambitus))
       finally (return (values winner winner-ambitus)))))


(defun as-chords (notes)
  "Regroup given list of NOTES as list of N-note chords.
If already grouped, do nothing."
  (cond ((typep notes 'karo)
         (as-chords (notes notes)))
        ((every #'(lambda (x) (and (typep x 'list)
                                   (= (length x) *n-ad-size*)))
                notes)
         notes)
        (t
         (unless (= (rem (length notes) *n-ad-size*) 0)
           (error "Length of note list ~A must be multiple of ~D"
		  notes *n-ad-size*))
         (loop while notes
	    collect (loop repeat *n-ad-size* collect (pop notes))))))

(alias as-triads as-chords)
(alias chords as-chords)


(defun list-rotate (list &optional (distance 1))
  (flet ((do-rotate (x)
	   (let ((m (copy-list x)))
	     (dotimes (i distance)
	       (setf m (cons (car (last m)) (butlast m))))
	     m)))
    (cond ((or (not (integerp distance))
	       (= distance 0))
	   list)
	  (t
	   (cond ((listp (car list))
		  (mapcar #'do-rotate list))
		 (t
		  (do-rotate list)))))))


(defun all-list-rotations (list)
  (loop for x below (length list)
     collect (list-rotate list x)))


(defun chord-intervals (chord &optional (reference t))
  (let ((c (funcall (if reference #'reference #'identity) chord)))
    (loop for a in c
       for b in (rest c)
       collect (mod (- b a) *notes-in-group*))))


(defun chord-name-intervals (chord &optional reference)
  "Return given CHORD in its 'name' format.
If REFERENCE is not NIL, return name of CHORD as referenced."
  (loop for c in (all-list-rotations chord)
     for v = (apply #'max (chord-intervals c reference))
     with result
     with resultv
     do
       ;; (princ c)       (princ v)       (terpri)
       (when (or (not result)
		 (< v resultv))
	 (setq result c)
	 (setq resultv v))
     finally (return (values (chord-intervals result reference) (first result)))))


(defun base-chord-name (chord)
  (chord-name-intervals (read-chord chord) nil))


(defun chord-name (chord &optional reference)
  (let ((the-chord (funcall (if reference #'reference #'identity)
			    (read-chord chord))))
    (chord-name-intervals the-chord reference)))


(defun tree< (a b)
  "Compares two lists (of depth 1, doesn't recurse)."
  (loop for i in a
     for j in b
     when (> j i)
     do (return t)
     when (< j i)
     do (return nil)
     finally (return nil)))


(defun basic-form (chord)
  (reference (lowest-ambitus (normalize (read-chord chord)))))


(defun karo-structure-chord (chord &key (reference nil) (basic-form t) (simplify nil))
  (multiple-value-bind (interval base)
      (chord-name (funcall (if basic-form #'basic-form #'identity) chord) reference)
    ;; IF simplify THEN 5,2(i) -> 2,5(i+5) (only in Z12)
    (if (and simplify
	     (z12-p)
	     (= (first interval) 5)
	     (= (second interval) 2))
	(list (list (second interval)
		    (first interval))
	      (mod (+ base 5) *notes-in-group*))
      (list interval base))))


(defun perm-key (karo-structure-part)
  (ambitus (normalize (mapcar #'second karo-structure-part))))


(defun structure-to-chord (structured-chord)
  ;; ((4 4) 3) -> (3 7 11)
  (destructuring-bind ((first-interval second-interval) root)
      structured-chord
    (list root
	  (+ root first-interval)
	  (+ root first-interval second-interval))))


(defun chord< (a b)
  ;; input in ((a b) c) format
  (flet ((sort-key (chord)
	   (destructuring-bind ((interval-1 interval-2) root)
	       chord
	   (+ (* interval-1 100)
	      (* interval-2 10)
	      root))))
    (< (sort-key a) (sort-key b))))


(defun chord-score (chord)
  ;; input in format ((a b) c)
  (destructuring-bind ((interval-1 interval-2) root)
      chord
    (+ (* 100 interval-1)
       (* 10 interval-2)
       root)))


(defun chord-structure-score (chord-structure)
  (loop for c in (nreverse (mapcar #'chord-score chord-structure))
       for i = 1 then (* 10 i)
       sum (* i c)))


(defun transpose-structure (chord-structure transposition-interval &key (simplify nil))
  (mapcar #'(lambda (x)
	      (list (first x)
		    (if (and simplify (equal (first x) '(4 4)))
			(mod (mod (- (second x) transposition-interval)
				  *notes-in-group*)
			     4)
		      (mod (- (second x) transposition-interval)
			   *notes-in-group*))))
	  chord-structure))


(defun normalization-candidates (karo-chord-structure)
  (let ((chord-structure (mapcar #'(lambda (c)
				     (karo-structure-chord (structure-to-chord c) :simplify t))
				 karo-chord-structure)))
    (loop for offset in (mapcar #'second chord-structure)
	  for candidate = (sort (transpose-structure chord-structure offset :simplify t) #'chord<)
	  for score = (chord-structure-score candidate)
	  collecting (cons score candidate))))


(defmethod normalize-chord-structure ((a-chord-structure list))
  (let ((candidates (normalization-candidates a-chord-structure)))
    (rest (first (sort candidates #'< :key #'first)))))
#|
	  collecting (cons score candidate))))
    (second (first (sort (mapcar #'(lambda (x)
				     (let ((candidate (sort (transpose-structure chord-structure x) #'chord<)))
				       (cons (chord-structure-score candidate) candidate)))
				 (mapcar #'second chord-structure))
			 #'<
			 :key #'first)))))


    ;; Special cases:
    ;; structures of form a,b(x) a,b(y) c,d(z) e,f(m)
    ;; and a,b(x) a,b(y) a,b(z) c,d(m)
    ;; have to be "further normalized"
    ;; Examples:
    ;; a)
    ;; 1st: 2,5(i) 2,5(i+4) 3,1(i+5) 3,2(i+10)
    ;; is equal to
    ;; 2nd: 2,5(i) 2,5(i+8) 3,1(i+1) 3,2(i+6)
    ;; (subtract 8 from the roots in 2nd to get 1st)
    ;; and the canonical form is 1st (roots in ascending order)
    ;; b)
    ;; 1st: 1,3(i) 1,3(i+2) 1,3(i+7) 4,1(i+5)
    ;; is equal to
    ;; 2nd: 1,3(i) 1,3(i+5) 1,3(i+2) 4,1(i+10)
    ;; (subtract 5 from the roots in 2nd to get 1st)
    ;; and the canonical form is 1st (roots in ascending order)
    (when (equal (first (first normalized))
		 (first (second normalized)))
      (let ((first-root-interval (- (second (second normalized))
				    (second (first normalized))))
	    (second-root-interval (- (second (third normalized))
				     (second (second normalized)))))
	(cond ((equal (first (first normalized))
		      (first (third normalized)))
	       ;; case b)
	       (let ((candidates (mapcar #'(lambda (n)
					    (sort (transpose-structure normalized n) #'chord<))
					(list (second (second normalized))
					      (second (third normalized))))))
		 (loop for c in candidates
		       for s = (chord-structure-score c)
		       with score = (chord-structure-score normalized)
		       when (< s score)
		       do (setq normalized c) (setq score s))))
	      ;;
	      (t
	       ;; case a)
	       (when (>= first-root-interval (ceiling (/ *notes-in-group* 2)))
		 (let ((candidate (sort (transpose-structure normalized first-root-interval)
					#'chord<)))
		   ;; if root of 3rd "chord" is smaller in candidate,
		   ;; let's pick that
		   (when (< (second (third candidate))
			    (second (third normalized)))
		     (format t "~&Replace ~A with ~A" normalized candidate)
		     (setq normalized candidate))))))))
    ;; Root of first interval has to be i, so let's transpose if needed
    (let ((first-structure-root (second (first normalized))))
      (cond ((> first-structure-root 0)
	     (transpose-structure normalized first-structure-root))
	    (t
	     normalized)))))
|#		    

(defmethod normalize-chord-structure ((karo karo))
  (normalize-chord-structure (karo-structure karo)))


(defun show-chord-name (chord &optional reference)
  "Show name of CHORD."
  (multiple-value-bind (intervals base)
      (chord-name chord reference)
    (format nil "~{~A~^,~}(~A)" intervals base)))


(defun name-chord (chord)
  "Show name of lowest ambitus equivalent for given CHORD"
  (show-chord-name (basic-form chord)))

(alias name-triad name-chord)


(defun as-notes (notes-or-karo)
  (etypecase notes-or-karo
    (list notes-or-karo)
    (karo (notes notes-or-karo))))


(defmethod name-intervals ((karo karo) &optional reference)
  (declare (ignore reference))
  (mapcar #'chord-name (as-chords karo)))


(let (karos)
  (defun all-karos (&optional (notes '(0 1 2 3 4 5 6 7 8 9 10 11)))
    "Generate array of all karos"
    (or (cdr (assoc *notes-in-group* karos))
	(let* ((notes (subseq notes 0 *notes-in-group*))
	       (all-karos-in-z
		(loop for i below (max-karo)
		   with karo-array = (make-array (max-karo))
		   do (setf (aref karo-array i) 
			    (make-instance 'karo
					   :karo-index (1+ i) ;; OFF-BY-ONE!
					   :notes notes))
		   finally (return karo-array))))
	  (setf karos (cons (cons *notes-in-group* all-karos-in-z) karos))
	  all-karos-in-z))))


(defun find-karo (notes)
  "Get the karo matching NOTES"
  (let ((notes (apply #'nconc
		      (sort (mapcar #'(lambda (x) (sort x #'<))
				    (as-chords notes))
			    #'< :key #'first))))
    (find-if #'(lambda (x)
		 (equal notes (notes x)))
	     (all-karos))))


(defmethod rotate ((karo karo) &optional (rotation 1))
  "Rotate KARO by ROTATION.

E.g. rotating by 1 (the default):
CL-USER> #[3967]
#<Karo 3967 (0 2 7 1 3 9 4 6 11 5 8 10) 2,5(0) 4,2(9) 2,5(4) 3,2(5)>
CL-USER> (rotate #[3967])
#<Karo 9893 (0 5 7 1 3 8 2 4 10 6 9 11) 5,2(0) 2,5(1) 4,2(10) 3,2(6)>

In the example above, (0 2 7) becomes (1 3 8), (1 3 9) becomes (2 4 10) and so on.
"
  (unless (integerp rotation)
    (error "ROTATION has to be an integer -- ROTATE"))
  (find-karo (mapcar #'(lambda (x) (mod (+ x rotation) *notes-in-group*)) (notes karo))))


(defmethod all-rotations ((karo karo))
  (loop for i below *notes-in-group*
     collect (rotate karo i)))


;; a Karo prints as follows:
;; #<Karo 1 (0 1 2 3 4 5 6 7 8 9 10 11) 1,1(0) 1,1(3) 1,1(6) 1,1(9)>
(defmethod print-object ((object karo) stream)
  (format stream "#<Karo ~A ~A~{ ~A~}>"
	  (karo-index object)
	  (notes object)
	  (mapcar #'name-chord (as-chords (notes object)))))


(defmacro karo-loop ((karo &optional (index (gensym))) &body body)
  `(loop for ,index from 1 upto (array-dimension (all-karos) 0)
      for ,karo = (get-karo ,index)
	,@body))


;; #[300] == (get-karo 300)
;; => #<Karo 300 (0 1 3 2 4 7 5 6 8 9 10 11) 1,2(0) 2,3(2) 1,2(5) 1,1(9)>
;; #[0 1 2 3 4 5 6 7 8 9 10 11] == (find-karo '(0 1 2 3 4 5 6 7 8 9 10 11))
;; => #<Karo 0 (0 1 2 3 4 5 6 7 8 9 10 11) 1,1(0) 1,1(3) 1,1(6) 1,1(9)>
(set-dispatch-macro-character #\# #\[
			      (lambda (stream subchar arg)
				(declare (ignore subchar arg))
				(let* ((notes (read-delimited-list #\] stream t)))
				  (if (= (length notes) 1)
				      `(get-karo ,@notes)
				      `(find-karo (quote ,notes))))))
(set-macro-character #\] (get-macro-character #\) nil))


(defmethod initialize-instance :after ((this karo) &key notes
				       &allow-other-keys)
  (setf (slot-value this 'notes) (permute notes (1- (karo-index this)))))


(defmacro cache (object slot-name &body body)
  (let ((o (gensym))
	(s (gensym)))
    `(let ((,o ,object)
	   (,s ,slot-name))
       (unless (slot-boundp ,o ,s)
	 (setf (slot-value ,o ,s)
	       ,@body))
       (slot-value ,o ,s))))


(defmethod simple-notes ((this karo))
  "Chords of this karo restated as lowest ambitus equivalents"
  (cache this 'simple-notes
    (mapcar #'lowest-ambitus (as-chords this))))


(defmethod distances ((notes list) &key (lowest-ambitus t))
  (mapcan #'(lambda (chord)
	      (let ((ref (if lowest-ambitus
			     (reference (lowest-ambitus chord))
			     (reference chord))))
		(list (- (second ref) (first ref))
		      (- (third ref) (second ref)))))
	  (if (listp (first notes))
	      notes
	      (chords notes))))


(defmethod distances ((this karo) &key ignore)
  (declare (ignore ignore))
  "Distance vector of chords in this karo"
  (cache this 'distances (distances (simple-notes this))))


(defun pairwise-list (obj)
  (etypecase obj
    (list
     (loop for item on obj by #'cddr
	collect (cons (first item) (second item))))
    (vector
     (loop for i below (array-dimension obj 0) by 2
	collect (cons (aref obj i) (aref obj (1+ i)))))))


(defun karo-distance-match (distances model-permutations)
  ;; (format t "~&KARO-DISTANCE-MATCH ~A ~A" distances model-permutations)
  (let ((distances (pairwise-list distances)))
    (some #'(lambda (model)
	      (tree-equal distances model))
	  model-permutations)))


(defmethod distance-match ((this karo) model-permutations)
  (karo-distance-match (distances this) model-permutations))


(defun name-chords (ANY)
  "Name chords of ANY"
  (mapcar #'name-chord (as-chords any)))

(alias name-triads name-chords)


(defun name-chord-form (any)
  "Name chords of ANY"
  (format t "~{~A~^ ~}" (name-chords any))
  any)

(alias name-triad-form name-chord-form)


;; CL-USER> (show t #[300])
;; 300    (0 1 3)   (2 4 7)   (5 6 8)   (9 10 11) 
;;         1,2(0)    2,3(2)    1,2(5)    1,1(9)    
;; ; No value
(defmethod show (stream (this karo))
  "Print a karo"
  (format stream "~&~7A~{~10A~}~%~8T~{~10A~}~%"
	  (karo-index this)
	  (as-chords this)
	  (name-chords this))
  (values))


(defun get-karo (index)
  "Get the INDEXth karo from KAROS"
  (aref (all-karos) (1- index))) ;; OFF-BY-ONE


(defun model-permutations (pairwise-model)
  (remove-duplicates (mapcar #'(lambda (permutation)
				 (mapcar #'(lambda (x)
					     (nth x pairwise-model))
					 permutation))
			     (permutations-of (length pairwise-model)))
		     :test #'equal))


(defun find-karos (&rest model)
  "Find all karos that have similar structure to given model.
Model should be a sequence (list, vector) of 8 elements, where
each successive pair gives the distances within a chord."
  (if (= (length model) 1)
      (setq model (pop model)))
  (unless (and (or (listp model) (arrayp model))
	       (= (length model) 8))
    (error "Model must be a sequence of 8 elements, ~
~A is not -- FIND-KAROS" model))
  ;; (if absolute
  ;;     (loop for karo across karos when (equal model (distances karo)) collect karo)
  (let ((matching-karos
	 (loop for karo across (all-karos)
	    with pairwise-model = (pairwise-list model)
	    when (distance-match karo (model-permutations pairwise-model))
	    collect karo)))
    (if (= (length matching-karos) 1)
	(sort (remove-duplicates (all-rotations (pop matching-karos))) #'< :key #'karo-index)
	matching-karos)))


(defun show-karos (&rest args)
  "Print a list of karos

ARGS can be given as a list of N elements, or as N separate arguments.
Currently N has to be 8."
  (let ((template (cond ((and (= (length args) 1)
			      (listp (first args)))
			 (first args))
			((= (length args) 8)
			 args)
			(t
			 (error "SHOW-KAROS -- Can't interpret ~A as a template" args)))))
    (dolist (karo (find-karos template))
      (show t karo))))


(defun map-note (note axis)
  "Map NOTE into its mirror note around AXIS."
  (mod (floor (+ *notes-in-group* axis (- axis note)))
       *notes-in-group*))


(defun opposite (chord)
  "For a given CHORD, return the chord that lies on the opposite side
of the note circle."
  (mapcar #'(lambda (x) (mod (+ (floor (/ *notes-in-group* 2)) x)
			     *notes-in-group*))
	  chord))


(defun chord= (a b)
  (not (mismatch (sort (copy-list a) #'<) (sort (copy-list b) #'<))))


(defun mirror-image (chord axis &optional opposite)
  "For a given CHORD, return its mirror image around the axis of
symmetry specified by AXIS. If CHORD is its own mirror image and OPPOSITE is T,
return its opposite."
  (let ((c (mapcar #'(lambda (x) (map-note x axis)) chord)))
    (if (and (chord= c chord)
	     opposite)
	(opposite c)
	c)))


(defun chord-set-difference (a b)
  (set-difference a b :test #'chord=))


(defun find-mirror-image (chord chords axis)
  (unless (numberp axis)
    (error "AXIS must be a number instead of ~A -- FIND-MIRROR-IMAGE" axis))
  (loop for c2 in chords
     when (chord= chord (mirror-image c2 axis))
     do (return-from find-mirror-image
	  (values c2 axis (chord-set-difference chords (list chord c2))))
     finally (return nil)))


(defmethod symmetric ((chords list) &optional axis)
  (cond ((not chords)
	 axis)
	(t
	 (multiple-value-bind (chord x rest)
	     (find-mirror-image (first chords) chords axis)
	   (if chord
	       (symmetric rest x)
	       nil)))))


(defmethod symmetric ((karo karo) &optional axis)
  (declare (ignore axis))
  (symmetric (as-triads karo)))


(defmethod symmetries ((chords list))
  (remove nil (loop for i below (floor (/ *notes-in-group* 2)) by 0.5
		 collect (symmetric chords i))))


(defmethod symmetries ((karo karo))
  (symmetries (chords karo)))


(defun analyze-notes (notes)
  (flet ((order-intervals (x)
	   (sort x #'< :key #'(lambda (y) (+ (* 10 (first y))
					     (second y))))))
    (let ((intervals (order-intervals (mapcar #'(lambda (x)
						  (chord-intervals
						   (basic-form x)))
					      (as-chords notes)))))
      (cond ((apply #'= (apply #'append intervals))
	     :rotational)
	    ((equal intervals (order-intervals (mapcar #'reverse intervals)))
	     :symmetric)
	    (t
	     :common)))))


(defmethod analyze ((karo karo))
  (cache karo 'analysis
    (analyze-notes (notes karo))))


(defun classify-karos ()
  (karo-loop (i k)
    for a = (analyze k)
    with classes = (make-hash-table)
    do
    (let ((h (gethash a classes)))
      (cond ((and h
		  (not (cdr (first h)))
		  (= i (1+ (car (first h)))))
	     (setf (cdr (first h)) (list i)))
	    ((and h
		  (cdr (first h))
		  (= i (1+ (second (first h)))))
	     (incf (second (first h))))
	    (t
	     (push (list i)
		   (gethash a classes)))))
    finally
    (return (sort (loop for k being each hash-key of classes
		     collect (cons k (nreverse (gethash k classes))))
		  #'string< :key #'first))))


(defun validate-class (class-id)
  "Return T if CLASS-ID is a keyword or a symbol and its SYMBOL-NAME
is equal to one of the Karo classes (common, rotational, symmetric),
otherwise signal an error."
  (let* ((valid-classes (list :common :rotational :symmetric))
	 (match (find (if (stringp class-id)
			  class-id
			  (symbol-name class-id))
		      valid-classes :key #'symbol-name :test #'string=)))
    (if match
	match
	(error "Class has to be one of ~A, not ~A -- VALIDATE-CLASS"
	       valid-classes class-id))))


(defmethod karo-class ((karo karo))
  (analyze karo))


(defmethod karo-class ((class-id t))
  (let ((id (validate-class class-id)))
    (karo-loop (k)
      when (eq id (analyze k))
      collect k)))


(defun show-karo-classes (&optional (stream t))
  (dolist (c (classify-karos))
    (format stream "~&~:(~A~): ~{~{~#[none~;~S~;~S..~:;XXX~]~}~^, ~}"
	    (first c) (rest c))))


(defun salto (1st-chord 2nd-chord &key adjust absolute)
  (mapcar (if absolute #'abs #'identity)
	  (mapcar (if adjust #'adjust-range #'identity)
		  (mapcar #'-
			  (reference (read-chord 2nd-chord))
			  (first-reference (read-chord 1st-chord))))))


(defun total-salto (&rest parameters)
  (reduce #'+ (apply #'salto parameters)))


(defun total-salto-karo (karo &rest parameters)
  (let ((chords (as-chords karo)))
    (loop for a in chords
       for b in (rest chords)
       sum (apply #'total-salto a b parameters))))


(defun total-absolute-salto (1st-chord 2nd-chord
			     &key reject-translation adjust)
  (let ((salto (salto 1st-chord 2nd-chord
		      :absolute t :adjust adjust)))
    (if (and reject-translation
	     (apply #'= salto))
	most-positive-fixnum
	(reduce #'+ salto))))


(defun tas (x) (fifth x))


(defun voice-decision-number (salto)
  "Calculates the Voice Decision Number of a chord salto."
  (loop for x in (nreverse (sort (mapcar #'abs salto) #'<))
     for i = 1 then (* i 10)
     sum (* i x)))


(defun salto-values-chord (a b
			   &key
			   adjust
			   reject-translation)
  ;; (karo-debug "SALTO-VALUES-CHORD ~A ~A" a b)
  (let ((total-salto (total-salto a b :adjust adjust)))
    (if (> (abs total-salto) (* 6 (length a)))
	;;
	(let ((b2 (transpose b (if (> total-salto 0) -12 12))))
	  ;; (karo-debug "SALTO-VALUES-CHORD ~A ~A total salto ~A, trying ~A ~A"
	  ;;             a b total-salto a b2)
	  (salto-values-chord a b2
			      :adjust adjust
			      :reject-translation reject-translation))
	;;
	(let ((salto (salto a b :adjust adjust)))
	  (list salto
		total-salto
		(total-absolute-salto a b
				      :adjust adjust
				      :reject-translation
				      reject-translation)
		(voice-decision-number salto))))))

(alias salto-s first)
(alias salto-ts second)
(alias salto-tas third)
(alias salto-vdv fourth)


(defun total-tas (saltos)
  (reduce #'+ saltos :key #'salto-tas))


(defun salto-values-karo (karo-list &key reject-translation)
  (let ((chords (as-chords karo-list)))
    (loop for a in chords
       for b in (rest chords)
       collect (salto-values-chord a b
				   :adjust t
				   :reject-translation reject-translation))))


(defun salto-values (a &optional b &rest arguments)
  (karo-debug "SALTO-VALUES ~A ~A" a b)
  (cond ((or (typep a 'karo)
	     (not b))
	 (apply #'salto-values-karo a arguments))
	(t
	 (apply #'salto-values-chord a b arguments))))


(defun transition-matrix (a b &key (format :latex) (stream t) caption)
  (default caption (format nil "Transition matrix for (~{~A~^, ~}) and (~{~A~^, ~})." a b))
  (ecase format
    (:latex
     (format stream "~&\\begin{table}
\\centering
  \\begin{tabular}{|r|~{r~*|~}}~:*
    \\hline
    A/B ~{& ~A ~}\\\\ \\hline
~{~{~A~^ & ~} \\\\ \\hline~^~%~}
  \\end{tabular}
  \\caption{~A}
\\end{table}~%"
	     b
	     (mapcar #'(lambda (x)
			 (cons x
			       (mapcar #'(lambda (y)
					   (let ((salto (- y x)))
					     (if (> salto (1- (floor *notes-in-group* 2)))
						 (- salto *notes-in-group*)
						 (if (< salto (- (floor *notes-in-group* 2)))
						     (+ salto *notes-in-group*)
						     salto))))
				       b)))
		     a)
	     caption))))


(defun karo-debug (fmt &rest arguments)
  (when *debug*
    (apply #'format t (concatenate 'string "~&" fmt "~%")
	   arguments)
    (finish-output t)))


(defun connect-two-sort (first-chord second-chords &key reject-translation (adjust t))
  (sort (mapcar #'(lambda (c)
		    (karo-debug "1st ~A 2nd ~A" first-chord c)
		    (append (list (normalize c)
				  (normalize first-chord))
			    (salto-values first-chord c
					  :adjust adjust
					  :reject-translation reject-translation)))
		second-chords)
	#'< :key #'sixth)) ; SORT BY voice-decision-number


(defmethod karo-structure ((karo karo) &optional reference (basic-form t))
  (let ((intervals-and-bases
	 (sort (mapcar #'(lambda (x) (karo-structure-chord x 
							   :reference reference
							   :basic-form basic-form))
		       (as-chords karo))
	       #'(lambda (a b)
		   (tree< (first a) (first b)))))
	(ib-hash (make-hash-table :test 'equal)))
    (dolist (x intervals-and-bases)
      (push x (gethash (first x) ib-hash)))
    (let ((chord-structures
	   (loop for k being each hash-key of ib-hash
	      append (first (sort (list-permutations (gethash k ib-hash))
				  #'(lambda (a b)
				      (< (perm-key a) (perm-key b))))))))
      chord-structures)))


(defun karo-structures (&optional karo-list)
  (default karo-list (all-karos))
  (let ((structure-hash (make-hash-table :test #'equal)))
    (loop for karo across karo-list
       for key = (normalize-chord-structure karo) ;; (karo-structure karo)
       do (push karo (gethash key structure-hash)))
    structure-hash))


(defun ends-with (string suffix)
  (eql (mismatch string suffix :from-end t)
       (- (length string) (length suffix))))


(defun structure-formatted (karo-structure)
  (format nil "~{~{~{~D~^,~}(i~[~:;~:*+~D~])~}~^ ~}"
	  karo-structure))


(defun compare-lists (a b)
  "Compare two lists of integers, returning -1 if A < B, 1 if A > B and 0 if A = B.
Comparisons are made one element at a time from left."
  (loop for x in a
     for y in b
     when (< x y)
     do (return -1)
     when (> x y)
     do (return 1)
     finally (return 0)))


(defun karo-structure< (a b)
  (loop for x in a
     for y in b
     for c = (compare-lists (first x) (first y))
     do
       (ecase c
	 (1 (return-from karo-structure< nil))
	 (-1 (return-from karo-structure< t))
	 (0 nil)))
  (loop for x in a
     for y in b
     when (< (second x) (second y))
     do (return-from karo-structure< t)
     when (> (second x) (second y))
     do (return-from karo-structure< nil)
     finally (return t)))


(defun karo-connections (&key structures (reject-translation t))
  (default structures (karo-structures))
  (let ((keys (sort (loop for k being each hash-key of structures collect k)
		    #'karo-structure<))
	(the-connections (make-hash-table :test #'equal)))
    (loop for k in keys
       do (let* ((v (gethash k structures))
		 (connections (mapcar #'(lambda (x) 
					  (connect x :reject-translation reject-translation)) 
				      v))
		 (tas-vector (mapcar #'first connections))
		 (tas (extremum connections #'< :key #'first :key-value-only t))
		 (karo-indexes (sort (mapcar #'karo-index v) #'<))
		 (equal-tas (apply #'= tas-vector)))
	    (setf (gethash k the-connections)
		  (list v connections tas-vector tas karo-indexes equal-tas))))
    (setf (gethash :keys the-connections) keys)
    (values the-connections)))


(defun structures-file (&key (filename "/tmp/structures.csv") structures connections (full nil))
  (default structures (karo-structures))
  (default connections (karo-connections :structures structures))
  (let ((suffix ".csv"))
    (unless (ends-with filename suffix)
      (setq filename (concatenate 'string filename suffix))))
  (with-open-file (s filename
		     :direction :output
		     :if-exists :supersede)
    (dolist (row (list (append (list (format nil "Karo Engine v~A" *karo-version*))
			       *karo-version-date*)
		       '("Structure"
			 "n of Karos"
			 "TAS"
			 "Karo indexes")))
      (format s "~&~{\"~A\"~^;~}~%" row))
    (loop for k in (gethash :keys connections)
       do (destructuring-bind (v best-connections tas-vector tas karo-indexes equal-tas)
	      (gethash k connections)
	    (if full
		(format s "~&\"~A\";~D;~D;~A;~:[\"N\"~;~];\"~A\";\"~A\"~%"
			(structure-formatted k)
			(length v)
			tas
			(format nil "\"~{~D~^,~}\"" karo-indexes)
			equal-tas
			(format nil "~A" tas-vector)
			(format nil "~A" (second (first best-connections))))
	      (format s "~&\"~A\";~D;~D;~A~%"
		      (structure-formatted k)
		      (length v)
		      tas
		      (format nil "\"~{~D~^,~}\"" karo-indexes))))))
  filename)


(defun stats (&key (filename "/tmp/stats.txt") structures connections)
  (default structures (karo-structures))
  (default connections (karo-connections structures))
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (loop for key in (gethash :keys connections)
       with stats = (make-hash-table)
       do
	 (destructuring-bind (v best-connections tas-vector tas karo-indexes equal-tas)
	     (gethash key connections)
	   (declare (ignore v best-connections tas-vector karo-indexes equal-tas))
	   (push key (gethash tas stats)))
       finally
	 (let ((stat-keys (loop for k being each hash-key of stats
			     collect k into n
			     finally (return (sort n #'<)))))
	   (loop for k in stat-keys
	      do
		(format s "~&~D ~D~%" k (length (gethash k stats))))))))


(defun connect-two (1st-chord 2nd-chord
		    &key
		    (all t)
		    (adjust t)
		    permute-first
		    reject-translation)
  (karo-debug "Foo")
  (let ((chord-1 (normalize 1st-chord))
	(chord-2 (normalize 2nd-chord)))
    (karo-debug "Connecting ~A and ~A => ~A {~A} and ~A {~A}"
		1st-chord 2nd-chord
		chord-1 (ambitus chord-1)
		chord-2 (ambitus chord-2))
    (remove nil
	    (loop for first-chord in (mapcar #'first-reference
					     (if permute-first
						 (list-permutations chord-1)
						 (list chord-1)))
	       with second-chords = (mapcar #'reference (list-permutations chord-2))
	       append
		 (progn
		   (karo-debug "Foo ~A" first-chord)
		   (let ((connections
			  (stable-sort
			   (connect-two-sort first-chord second-chords 
					     :reject-translation reject-translation
					     :adjust adjust)
			   #'< :key #'tas))) ; SORT BY total-absolute-salto
		     (if all
			 (let ((tas (tas (first connections))))
			   (karo-debug "Connections ~A" connections)
			   (mapcar #'(lambda (x)
				       (cond ((< (tas x) tas)
					      ;; something weird has
					      ;; happened as TAS of
					      ;; first in the list
					      ;; should also be
					      ;; smallest
					      (error "TAS of ~A is ~A but should not be smaller than ~A" 
						     x (tas x) tas))
					     ((= (tas x) tas)
					      x)
					     (t nil)))
				   connections))
			 (first connections))))))))


(defun best-match (chord-1 chord-2 &rest parameters)
  (caar (apply #'connect-two chord-1 chord-2 parameters)))


(defun chord-permutations (list-of-chords &optional result)
  (cond ((null list-of-chords)
	 result)
	((null result)
	 (chord-permutations (rest list-of-chords) 
			     (list-permutations-unique (first list-of-chords))))
	(t
	 (chord-permutations (rest list-of-chords)
			     (loop for r in result
				with p = (list-permutations (first list-of-chords))
				append (mapcar #'(lambda (y)
						   (if (listp (caar result))
						       (append r (list y))
						       (cons (copy-list r) 
							     (list (copy-list y)))))
						p))))))


(defun all-permutations (list-of-chords)
  "Gives all relevant permutations of LIST-OF-CHORDS.
Mirror images are not considered."
  (mapcan #'list-permutations-unique (chord-permutations list-of-chords)))


(defmethod all-chord-permutations ((karo karo))
  (maplist #'(lambda (y)
	       (mapcan #'(lambda (z)
			   (when (rest y)
			     (list :foo z (rest y))))
		       (first y)))
	   (mapcar #'list-permutations-unique (as-chords karo))))


(defun best-connections (candidates)
  (let ((best-connection-hash (make-hash-table :test #'equal))
	(smallest-tas most-positive-fixnum)
	best-connection)
    (dolist (candidate candidates)
      (let* ((salto-values (salto-values candidate))
	     (best-connection-key (mapcan #'rest salto-values))
	     (tas (total-tas salto-values)))
	(cond ((< tas smallest-tas)
	       ;; This permutation is better than any of the ones
	       ;; found so far
	       (clrhash best-connection-hash)
	       (push candidate (gethash best-connection-key
					best-connection-hash))
	       (setf best-connection candidate
		     smallest-tas tas))
	      ;;
	      ((= tas smallest-tas)
	       ;; This permutation is equally good as one or more
	       ;; permutations found earlier (has equal TAS)
	       ;;
	       ;; Permutations are divided into classes by a list
	       ;; stored in BEST-CONNECTION-KEY.  This list
	       ;; consists of 9 numbers that are the total salto,
	       ;; total absolute salto and voice decision number
	       ;; for each successive pair of chords in the
	       ;; solution.
	       (push candidate (gethash best-connection-key
					best-connection-hash)))
	      ;;
	      (t
	       ;; This permutation is not a winner, do nothing
	       nil))))
    (values best-connection
	    (salto-values best-connection)
	    smallest-tas
	    (loop for k being each hash-key of best-connection-hash
	       collect (list k (gethash k best-connection-hash))))))


(defun tas (i j)
  (reduce #'+
	  (mapcar #'(lambda (n m)
		      (let ((x (- n m))
			    (halfway (floor (/ *notes-in-group* 2))))
			(abs (cond ((< x (- halfway))
				    (+ x *notes-in-group*))
				   ((> x halfway)
				    (- x *notes-in-group*))
				   (t
				    x)))))
		  (basic-form i) (basic-form j))))


(defun connect-chords (a b)
  (loop for y in (list-permutations a)
     append
       (loop for x in (list-permutations b)
	  collect (list y x (tas y x)))))


(defun all-perms (list-of-chords)
  (loop for x in (rest list-of-chords)
     with a = (list (list (first list-of-chords)))
     do
       (setq a
	     (mapcan #'(lambda (n)
			 (mapcar #'(lambda (m)
				     (append n (list m)))
				 (list-permutations x)))
		     a))
     finally (return a)))


(defun candidates (list-of-chords)
  (loop for p in (list-permutations-unique list-of-chords)
     append (all-perms p)))
	 

(defun salto-list (list-of-chords)
   (loop for c on list-of-chords
      when (rest c)
      collect (mapcar #'(lambda (a b)
			  (- b a))
		      (first c)
		      (second c))))


(defun tas-list (salto-list)
  (let ((result (mapcar #'(lambda (x)
			    (reduce #'(lambda (n m)
					(+ (abs n) (abs m))) x))
			salto-list)))
    (values result (reduce #'+ result))))


(defun has-translation (salto-list)
  "If SALTO-LIST has a sublist where all elements are equal, return T,
otherwise return NIL."
  (when (remove nil (mapcar #'(lambda (x)
				(apply #'= x))
			    salto-list))
    t))
	  

(defun connect-chord-list (list-of-chords &key reject-translation)
  (let* ((c (mapcar #'first-octave-reference list-of-chords))
	 (salto-list (salto-list c))
	 (tas-list (tas-list salto-list))
	 (tas (if (and reject-translation
		       (has-translation salto-list))
		  most-positive-fixnum
		  (reduce #'+ tas-list))))
    (list tas c list-of-chords tas-list salto-list)))


(defun extremum (list predicate &key (key #'identity) key-value-only)
  (loop for x in list
       for v = (funcall key x)
       with result
       with value
       when (or (not result)
		(funcall predicate v value))
       do
       (setf result x
	     value v)
       finally (return (if key-value-only
			   value
			   (values result value)))))


(defmethod connect ((karo-list list) &key (reject-translation t) (fixed-order nil))
  "Find optimal connection of chords within a list of notes."
  (loop for c in (if fixed-order
		     (chord-permutations (as-chords karo-list))
		     (all-permutations (as-chords karo-list)))
     collect (connect-chord-list c :reject-translation reject-translation) into result
     finally (return (values (extremum result #'< :key #'first)))))


(defmethod connect ((karo karo) &key (reject-translation t) (fixed-order nil))
  (connect (notes karo) 
	   :reject-translation reject-translation 
	   :fixed-order fixed-order))


(defmethod all-connections (&key (clobber t) (file-name "/tmp/all_connections.txt"))
  (with-open-file (s file-name :direction :output
		     :if-exists (when clobber :supersede))
    (karo-loop (karo)
      with tas-minimum = most-positive-fixnum
      with tas-minimum-karo
      with tas-maximum = 0
      with tas-maximum-karo
      do 
      (when (= (mod (karo-index karo) 500) 0)
	(print karo)
	(force-output))
      (destructuring-bind (tas best-connection original-chords tas-vector salto-values)
	  (connect karo)
	(declare (ignore original-chords tas-vector))
	(when (> tas tas-maximum)
	  (setf tas-maximum tas
		tas-maximum-karo nil))
	(when (= tas tas-maximum)
	  (push karo tas-maximum-karo))
	(when (< tas tas-minimum) 
	  (setf tas-minimum tas
		tas-minimum-karo nil))
	(when (= tas tas-minimum)
	  (push karo tas-minimum-karo))
	(format s "~&~D: ~A TAS ~A best connection ~S saltos ~S~%"
		(karo-index karo)
		karo tas
		(format nil "~A" best-connection) 
		(format nil "~A" salto-values)))
      finally
      (format s "~&~%Minimum TAS ~D~%~A~%Maximum TAS ~D~%~A~%"
	      tas-minimum tas-minimum-karo tas-maximum tas-maximum-karo))))


(defun best-connected (class-id &key reject-translation)
  (let (best-connected-karos
	(best-tas most-positive-fixnum))
    (dolist (karo (karo-class class-id))
      (multiple-value-bind (best-solution salto-vector tas all-solutions)
	  (connect karo :reject-translation reject-translation)
	(declare (ignore best-solution salto-vector all-solutions))
	(cond ((< tas best-tas)
	       (setf best-tas tas
		     best-connected-karos (list karo)))
	      ((= tas best-tas)
	       (push karo best-connected-karos))
	      (t
	       nil))))
    (setf best-connected-karos (sort best-connected-karos
				     #'< :key #'karo-index))
    (values best-connected-karos
	    (mapcar #'karo-index best-connected-karos))))


(defun complete-list (group-size &optional (stream t) 
		      force
		      (notes '(0 1 2 3 4 5 6 7 8 9 10 11)))
  (unless (find group-size *group-sizes*)
    (error "Group size must be in ~A (not ~D)"
	   *group-sizes* group-size))
  (loop for i below (max-karo group-size)
     for karo = (permute (subseq notes 0 group-size) (1- i))
     do
       (format stream "~&~A~6T~{~A~^ ~}~%~6T~{~A~^ ~}~%" 
	       i (as-chords karo) (name-chords karo))
       (when force
	 (force-output stream))))


(defun save-complete-list (group-size file-name &key clobber force)
  (with-open-file (s file-name :direction :output
		     :if-exists (when clobber :supersede))
    (complete-list group-size s force)))


(defun solve (stream &rest parameters)
  (multiple-value-bind (best-solution salto-vector tas other-solutions)
      (apply #'connect parameters)
    (format stream "~&Best connection: ~A~%TAS: ~A~%Salto: ~A"
	    best-solution tas salto-vector)
    (loop for (class solutions) in other-solutions
       for i from 1
       do
	 (format stream "~&Class ~@R: ~A" i class)
	 (loop for karo in solutions
	    for j from 1
	    do (format stream "~&Connection ~@R.~D: ~A" i j karo))))
  (values))


;;;;;;;;;;;;;;;;
;;;; Diagrams
;;;;;;;;;;;;;;;;


(defun timestamp ()
  (multiple-value-bind (second minute hour date month year day dst-p tz)
      (get-decoded-time)
    (declare (ignore second day dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d "
	    year month date hour minute)))

(defmethod prolog (stream (format (eql :eps)))
  (format stream "%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 640 336
%%HiResBoundingBox: 0.000 0.000 640.000 336.000
%%Creator: Karo Engine ~A
%%Pages: 1
%%Orientation: Portrait
%%CreationDate: ~A
%%DocumentFonts: Helvetica
%%EndComments
%%Page: 1 1
" *karo-version* (timestamp))
  (format stream "/x 160 def
/y 160 def
/r 140 def
/notes ~D def
/nad ~D def
" *notes-in-group* *n-ad-size*)
  (format stream "
/xy {
15 exch sub notes div 360 mul /tmp2 exch def
x r tmp2 sin mul add y r tmp2 cos mul add exch
} def

/segment {
newpath
1 setlinewidth
0 0 0 setrgbcolor
[] 0 setdash
exch dup dup
/tmp r def
/r tmp 15 add def xy
moveto =string cvs dup stringwidth pop 2 div neg 0 rmoveto show
/r tmp def xy moveto
xy lineto stroke
} def

/triad {
/nth exch def
/c exch def
/b exch def
/a exch def
/dx x r 40 add add nth 30 mul add def
/dy y 70 sub def
% dx dy moveto a =string cvs show
% dx dy 20 add moveto b =string cvs show 
% dx dy 40 add moveto c =string cvs show 
a b segment b c segment c a segment
} def

/axis {
/a exch def
newpath
[3 3] 0 setdash
1 0 0 setrgbcolor
1 setlinewidth
/tmp r def
/r tmp 5 add def
a xy moveto
notes 2 div a add xy lineto stroke
/r tmp def
} def

/karo {
x y moveto
3 triad 2 triad 1 triad 0 triad
} def

%%EndProlog~%"))


(defmacro xml-document (stream &body body)
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (format ,s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
       ,@body)))


(defun doctype (stream name public id dtd)
  (format stream "~&<!DOCTYPE ~A ~A \"~A\" \"~A\">~%"
	  name public id dtd))


(defun tag-name (x)
  (etypecase x
    (string x)
    (symbol (string-downcase (symbol-name x)))))


(defun print-tag (stream tag &optional args (type :opening) (close nil))
  (ecase type
    (:opening
     (format stream "<~A~@[ ~{~A~^ ~}~]~@[/~]>"
	     (tag-name tag)
	     (when args
	       (mapcar #'(lambda (x)
			   (format nil "~A=\"~A\"" (tag-name (car x)) (cdr x)))
		       args))
	     close))
    (:closing
     (format stream "</~A>~%" (tag-name tag)))))


(defmacro tag (stream the-tag args &body body)
  (let ((s (gensym))
	(tag (gensym))
	(a (gensym)))
    `(let* ((,s ,stream)
	    (,tag ,the-tag)
	    (,a ,args))
       ;; (declare (special ,a))
       (print-tag ,s ,tag ,a)
       ,@body
       (print-tag ,s ,tag nil :closing))))


(defun tag1 (stream the-tag content &key allow-nil)
  (unless (or allow-nil content)
    (error "Tag content can not be NIL"))
  (tag stream the-tag nil (format stream "~A" content)))


(defun short-tag (stream the-tag &optional args)
  (print-tag stream the-tag args :opening t))


(defmethod musicxml ((karo karo) &key (directory "/tmp") (arpeggio t))
  (let ((filename (format nil "~A/karo-~A.xml" directory (karo-index karo))))
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (print-musicxml s karo :arpeggio arpeggio))
    filename))


(defmethod musicxml ((karo t) &key (directory "/tmp") (arpeggio t))
  (let ((filename (format nil "~A/karo.xml" directory)))
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (print-musicxml s karo :arpeggio arpeggio))
    filename))


(defmethod print-musicxml (s (karo karo) &key arpeggio)
  (print-musicxml-from-chords s (chords karo) :arpeggio arpeggio :karo-index (karo-index karo)))


(defmethod print-musicxml (s (notes t) &key arpeggio)
  (print-musicxml-from-chords s (as-chords notes) :arpeggio arpeggio))


(defun print-musicxml-from-chords (s chords &key arpeggio (karo-index #\Space))
  (xml-document s
		(doctype s "score-partwise" "PUBLIC"
			 "-//Recordare//DTD MusicXML 2.0 Partwise//EN"
			 "http://www.musicxml.org/dtds/partwise.dtd")
		(tag s 'score-partwise '((version . "2.0"))
		     (tag s 'movement-title nil
			  (format s "Karo ~A" karo-index))
		     (tag s 'identification nil
			  (tag s 'creator '((type . "composer"))
			       (format s "~A" *karo-composer*))
			  (tag s 'encoding nil
			       (tag s 'encoding-date nil
				    (format s "~A" (today)))
			       (tag s 'software nil
				    (format s "~A (~A ~A)" ; on ~A ~A"
					    (karo-version-string)
					    (lisp-implementation-type)
					    (lisp-implementation-version)
					    ;(software-type)
					    ;(software-version)
					    ))
			       (tag s 'encoding-description nil
				    (format s "Representation of Karo as chords"))))
		     (tag s 'part-list nil
			  (tag s 'score-part '((id . "P1"))
			       (tag s 'part-name nil
				    (princ "Music" s))))
		     (tag s 'part '((id . "P1"))
			  (loop for c in chords
			     for measure = 1 then (1+ measure)
			     do 
			       (tag s 'measure (list (cons 'number measure))
				    (tag s 'attributes nil
					 (tag1 s 'divisions 1)
					 (when (= measure 1)
					   (tag s 'key nil
						(tag1 s 'fifths 0))
					   (tag s 'time nil ; '((symbol . "common"))
						(tag1 s 'beats 4)
						(tag1 s 'beat-type 4))
					   (tag s 'clef nil
						(tag1 s 'sign 'g)
						(tag1 s 'line 2))))
				    (loop for n in c
				       for in-chord = nil then t
				       do
					 (tag s 'note nil
					      (unless arpeggio
						(when in-chord
						  (short-tag s 'chord)))
					      (tag s 'pitch nil
						   (ecase *notes-in-group*
						     (6
						      ;; C D E F# G# A#
						      (tag1 s 'step (nth n '(C D E F G A)))
						      (when (member n '(3 4 5))
							(tag1 s 'alter 1)))
						     (12
						      ;; C C# D D# E F F# G G# A A# B
						      (tag1 s 'step (nth n '(C C D D E F F G G A A B)))
						      (when (member n '(1 3 6 8 10))
							(tag1 s 'alter 1)))
						     (24
						      ;; C C+ C# C++
						      ;; D D+ D# D++
						      ;; E E+ 
						      ;; F F+ F# F++
						      ;; G G+ G# G++
						      ;; A A+ A# A++
						      ;; B B+
						      (tag1 s 'step (nth n '(C C C C
									     D D D D
									     E E
									     F F F F
									     G G G G
									     A A A A
									     B B)))
						      (cond ((member n '(1 5 9 11 15 19 23))
							     (tag1 s 'alter .5))
							    ((member n '(2 6 12 16 20))
							     (tag1 s 'alter 1))
							    ((member n '(3 7 13 17 21))
							     (tag1 s 'alter 1.5)))))
						   (tag1 s 'octave 4))
					      (tag1 s 'duration (if arpeggio 1 4))
					      (tag1 s 'voice 1)
					      (tag1 s 'type (if arpeggio "quarter" "whole"))
					      (tag1 s 'staff 1)))
				    (when arpeggio
				      (tag s 'note nil
					   (short-tag s 'rest)
					   (tag1 s 'duration 1)
					   (tag1 s 'voice 1)))))))))


(defmethod prolog (stream (format (eql :svg)))
  (multiple-value-bind (version date time)
      (karo-version)
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<svg width=\"800px\" height=\"420px\"  xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" baseProfile=\"tiny\">
<!-- Karo Engine ~A ~A ~A -->
" version date time)))


(defun do-karo-diagram (stream karo-index name-chords as-notes &optional axis)
  (format stream "/Helvetica findfont 14 scalefont setfont
x y r 0 360 arc closepath stroke

x r 40 add add y 20 add moveto
\(Karo ~D) show
x r 40 add add y moveto
\(~{~A~^ ~}) show~%"
	  karo-index name-chords)
  (when axis
    (dolist (axis-of-symmetry (symmetries (as-chords as-notes)))
      (format stream "~&~D axis~%" axis-of-symmetry)))
  (format stream "~{~D ~}karo~%" as-notes))


(defun svg-circle (cx cy r &key (fill "white") (stroke "black") (stroke-width 1))
  (format nil "~&<circle cx=\"~A\" cy=\"~A\" r=\"~A\" fill=\"~A\" stroke=\"~A\" stroke-width=\"~D\" />~%"
	  cx cy r fill stroke stroke-width))


(defun svg-text (text &key x y dx dy fill font-size point)
  (when point
    (setq x (first point))
    (setq y (second point)))
  (format nil "~&<text x=\"~A\" y=\"~A\"~@[ dx=\"~A\"~]~@[ dy=\"~A\"~]~@[ fill=\"~A\"~]~@[ font-size=\"~A\"~]>~A</text>~%"
	  x y dx dy fill font-size 
	  text))


(defun svg-path (points &key (colour "black"))
  (let ((start (pop points))
	(s (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (stream s)
      (format stream "~&<path style=\"fill: none; stroke: ~A\" d=\"M ~,3F ~,3F"
	      colour (first start) (second start))
      (loop for p in points
	 do (format stream " L ~,3F ~,3F" (first p) (second p)))
      (format stream "\" />~%"))
    s))


(defun svg-line (start end &key (colour "black") (width 1) dash)
  (format nil "~&<line stroke=\"~A\" x1=\"~,3F\" y1=\"~,3F\" x2=\"~,3F\" y2=\"~,3F\" stroke-width=\"~A\" ~@[stroke-dasharray=\"~{~D~^,~}\" ~]/>~%"
	  colour
	  (first start) (second start)
	  (first end) (second end)
	  width
	  dash))


(defun circumference-point (a divisor x y r)
  (let* ((my-pi 3.141592653)
	 (angle (* (/ (- (+ a (/ divisor 2))) divisor) 2 my-pi)))
    (list (+ x (* r (sin angle)))
	  (+ y (* r (cos angle))))))


(defun svg-axis (nth-axis n-notes x y r)
  (svg-line (circumference-point nth-axis n-notes x y r)
	    (circumference-point (+ nth-axis (floor (/ n-notes 2)))
				 n-notes x y r)
	    :dash '(5 3 2)
	    :colour "red"))


(defun svg-karo-diagram (stream karo-index as-chords
			 &optional (show-axis-of-symmetry t) (text nil))
  (let ((x 210) (y 210) (r 150) (n-notes *notes-in-group*)
	;; (name-chords (mapcar #'(lambda (x) (show-chord-name x t)) as-chords))
	(name-chords (mapcar #'name-chord as-chords)))
    (princ (svg-circle x y r) stream)
    (when show-axis-of-symmetry
      (dolist (axis-of-symmetry (symmetries as-chords))
	(princ (svg-axis axis-of-symmetry n-notes x y (* r 1.1)) stream)))
    (when text
      (when karo-index
	(princ (svg-text (format nil "Karo ~A" karo-index)
			 :x (+ x r 40) :y (+ y 20)) stream))
      (princ (svg-text
	      (format nil "~{~A~^ ~}" name-chords)
	      :x (+ x r 40) :y y) stream))
    (loop for n below n-notes
       for point = (circumference-point n n-notes (- x 5) (+ y 5) (* 1.1 r))
       do (princ (svg-text (format nil "~D" n)
			   :x (first point) :y (second point))
		 stream))
    (dolist (chord as-chords)
      (princ (svg-path (mapcar #'(lambda (a)
				   (circumference-point a n-notes x y r))
			       (append chord
				       (list (first chord)))))
	     stream))))


(defun svg-connection-diagram (stream karo-index list-of-chords)
  (declare (ignore karo-index))
  (let* ((scale-x 80)
	 (scale-y 14)
	 (max-y 24)
	 (text-y (1+ max-y)))
    (flet ((point (x y)
	     (list (* x scale-x)
		   (* y scale-y))))
      (loop for c on list-of-chords
	 for i = 1 then (1+ i)
	 when (rest c)
	 do
	   (progn
	     (mapc #'(lambda (a b)
		       (let ((start (point i (- max-y a)))
			     (stop (point (1+ i) (- max-y b))))
		       (princ (svg-line start stop) stream)
		       (princ (svg-circle (first start) (second start) 3) stream)
		       (princ (svg-circle (first stop) (second stop) 3) stream)		       
		       (princ (svg-text (format nil "~A" a) :point (point .5 (- max-y a))) stream)
		       (princ (svg-text (format nil "~A" b) :point (point .5 (- max-y b))) stream)))
		   (first c) (second c))
	     (princ (svg-text (format nil "~A" (first c)) :point (point i text-y)) stream)
	     (princ (svg-text (format nil "~A" (second c)) :point (point (1+ i) text-y)) stream))))))


(defun chord-connection-diagram (chords &key directory karo-index)
  (default directory "/tmp")
  (default karo-index 0)
  (let ((filename (format nil "~A/connections-~D.svg" directory karo-index)))
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (prolog s :svg)
      (svg-connection-diagram s karo-index chords)
      (trailer s :svg))
    filename))


(defmethod connection-diagram ((karo karo) &key directory &allow-other-keys)
  (chord-connection-diagram (as-chords karo) :directory directory :karo-index (karo-index karo)))


(defmethod connection-diagram ((list list) &key directory karo-index)
  (chord-connection-diagram list :directory directory :karo-index karo-index))


(defmethod karo-diagram (stream (karo karo) (format (eql :eps)) &optional (axis t))
  (do-karo-diagram stream
    (karo-index karo)
    (name-chords karo)
    (as-notes karo)
    axis))


(defmethod karo-diagram (stream (karo karo) (format (eql :svg)) &optional (axis t))
  (svg-karo-diagram stream (karo-index karo) (as-chords karo) axis))


(defmethod trailer (stream (format (eql :eps)))
  (format stream "%%Trailer
showpage
%% EOF~%"))


(defmethod trailer (stream (format (eql :svg)))
  (format stream "</svg>~%"))


(defmethod diagram ((karo karo) &optional directory (format :svg) (axis nil))
  (let ((filename (format nil "~A/karo-~D.~A"
			  (if directory directory "/tmp")
			  (karo-index karo)
			  (ecase format
			    (:eps "eps")
			    (:svg "svg")))))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists :supersede)
      (prolog stream format)
      (karo-diagram stream karo format axis)
      (trailer stream format))
    filename))


(defun svg-diagram (notes &optional directory)
  (default directory "/tmp")
  (let ((filename (format nil "~A/karo.svg" directory)))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (prolog stream :svg)
      (svg-karo-diagram stream nil (as-chords notes))
      (trailer stream :svg))
    filename))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:karo-midi
(defmethod midi-output ((notes list) &key directory duration instrument octave karo-index)
  (unless (integerp karo-index)
    (setf karo-index 0))
  (midi::with-midi-output (midi (format nil "~A/karo-~A.mid" 
					(or directory "/tmp") 
					karo-index)
				:title (format nil "Karo ~D" karo-index)
				:instrument instrument)
    (dolist (note notes)
      (cond ((listp note)
	     (midi::add-chord midi note :duration duration
			      :octave octave :notes-per-octave *notes-in-group*))
	    (t
	     (midi::add-note midi note :duration duration
			     :octave octave :notes-per-octave *notes-in-group*))))))


#+:karo-midi
(defmethod midi-output ((karo karo) &key directory duration instrument octave (arpeggio nil))
  (midi-output (if arpeggio 
		   (notes karo)
		   (as-chords karo))
	       :directory directory
	       :karo-index (karo-index karo)
	       :duration duration :instrument instrument
	       :octave octave))


;;;; (midi-output '(0 6 16 1 9 17 2 15 18 7 13 19 4 12 22 5 11 21 8 14 20 3 10 23))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant +default-split+ 5000)


(defun split-directory-name (x &optional (split +default-split+))
  (unless (and (integerp x) (> x 0) (<= x (max-karo)))
    (error "X has to be an integer between ~D and ~D, was ~A -- SPLIT-DIRECTORY-NAME" 1 (max-karo) x))
  (unless (and (integerp split) (> split 0) (<= split (max-karo)))
    (error "SPLIT has to be an integer between ~D and ~D, was ~A -- SPLIT-DIRECTORY-NAME" 1 (max-karo) split))
  (let ((start (1+ (* split (floor (/ (1- x) split)))))
	(end (min (max-karo) (* split (floor (/ (+ (1- x) split) split))))))
    (format nil "~D-~D" start end)))


(defun split-directory-list (&optional (split +default-split+))
  (loop with dirs
     with start = 1
     with end = split
     while (<= end (max-karo))
     do 
       (push (split-directory-name start split) dirs)
       (if (= end (max-karo))
	   (setf end most-positive-fixnum)
	   (setf start (1+ end)
		 end (min (+ end split) (max-karo))))
     finally
       (return (nreverse dirs))))


(defmethod split-directory ((karo karo) &optional split)
  (split-directory-name (karo-index karo) (if (integerp split) split +default-split+)))


(defmethod draw ((karo karo)
		 &key (directory "/tmp") 
		 (eps t) (svg t) (xml t) (midi t) (split nil) (silent nil))
  (macrolet ((progress (&body body)
	       `(if silent
		    (progn ,@body)
		    (print ,@body))))
    (when split
      (setq directory (format nil "~A/~A/" directory (split-directory karo split))))
    (ensure-directories-exist directory)
    (when (and midi (find-package "MIDI"))
      (progress (midi-output karo :directory directory)))
    (when xml
      (progress (musicxml karo :directory directory)))
    (let (diagram-formats)
      (when svg (push :svg diagram-formats))
      (when eps (push :eps diagram-formats))
      (dolist (format diagram-formats)
	(progress (diagram karo directory format t))))))


(defun draw-all (&key (directory "/tmp") (eps t) (svg t) (xml nil) (midi nil) (split t) (silent t))
  (karo-loop (k) 
    do (draw k :directory directory :eps eps :svg svg :xml xml :midi midi :split split :silent silent)))
    

(defun ways (m n)
  "Number of ways a subset of length N can be picked without replacement from a set of length M"
  (/ (factorial m) (* (factorial n) (factorial (- m n)))))


(defun two-ways (m n)
  "Number of ways two subsets of length N can be picked without replacement from a set of length M"
  (ways m (* 2 n)))


(defun arrangements (m n)
  "Number of ways two subsets of length N can be picked without replacement from a set of length M and arranged pairwise"
  (* (two-ways m n) (factorial n)))


(defun lilypond-note (note)
  ;; Only implemented for Z12 so far
  (string-downcase (format nil "~A" (nth note '(c cis d dis e f fis g gis a ais b)))))


(defun lilypond-chord (c)
  (typecase c
    (karo (mapcar #'lilypond-chord (as-chords c)))
    (t
     (list (format nil "<~{~A~^ ~}>4" (mapcar #'lilypond-note (rest c)))
	   (format nil "~A4" (lilypond-note (first c)))))))


(defmethod lilypond-chords ((list list))
  (mapcar #'lilypond-chord list))


(defmethod lilypond-chords ((karo karo))
  (mapcar #'lilypond-chord (as-chords karo)))


(defun first-notes (chords)
  (mapcar #'(lambda (y)
	      (mapcar #'first y))
	  chords))


(defun last-notes (chords)
  (mapcar #'(lambda (y)
	      (mapcar #'second y))
	  chords))


(defun lilypond-output-chords (chord-list &key directory karo-index titles filename)
  (declare (ignore titles))
  (let ((suffix "ly")
	(chords (lilypond-chords chord-list)))
    (default directory "/tmp")
    (cond (karo-index
	   (default filename (format nil "~A/karo-~D-notation.~A" directory karo-index suffix)))
	  (t
	   (default filename (format nil "~A/karo-notation.~A" directory suffix))))
    (unless (ends-with filename suffix)
      ;; to ensure we don't clobber an unrelated file
      (error "Output file name has to end in ~A, is ~A -- LILYPOND-OUTPUT" suffix filename))
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (format s "~&\\paper{
indent=0\\mm
line-width=120\\mm
oddFooterMarkup=##f
oddHeaderMarkup=##f
bookTitleMarkup=##f
scoreTitleMarkup=##f
}

upper = \\relative c' {
  \\clef treble
  \\key c \\major
  \\time 4/4

  ~{~{~A~^ ~}~^~%   |~}
}

lower = \\relative c' {
  \\clef bass
  \\key c \\major
  \\time 4/4
  
  ~{~{~A~^ ~}~^~%   |~}
}

nameChords = \\lyricmode {
 ~{~{~S~^ ~} ~}
}

\\score {
  \\new GrandStaff <<
    \\new Staff = upper { \\new Voice = \"singer\" \\upper }
    \\new Lyrics \\lyricsto \"singer\" \\nameChords
    \\new Staff = lower { \\lower }
  >>
  \\layout {
    \\context {
      \\GrandStaff
      \\accepts \"Lyrics\"
    }
    \\context {
      \\Lyrics
      \\consists \"Bar_engraver\"
    }
  }
  \\midi { }
}~%"
	      (first-notes chords)
	      (last-notes chords)
	      (mapcar #'name-chords chord-list))))
  filename)


;;
;; lilypond -dbackend=eps -dno-gs-load-fonts -dinclude-eps-fonts /tmp/karo-8316-notation.ly
;;
(defmethod lilypond-output ((karo karo) &key directory filename)
  (let ((f (lilypond-output-chords (list karo)
				   :directory directory
				   :filename filename
				   :karo-index (karo-index karo)))) 
    (format t "~&lilypond -dbackend=eps -dno-gs-load-fonts -dinclude-eps-fonts ~A~%" f)
    f))
			    

(defmethod lilypond-output ((list list) &key directory filename titles)
  (let ((f
	 (lilypond-output-chords list
				 :directory directory
				 :filename filename
				 :titles titles)))
    (format t "lilypond -dbackend=eps -dno-gs-load-fonts -dinclude-eps-fonts ~A~%" f)
    f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; csound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csound-default-instruments()
  "sr     = 44100
kr     = 4410
nchnls = 1

instr 1
  idur   = p3
  iamp   = p4
  ifenv  = 51                    ; clarinet settings:
  ifdyn  = 52                    ; amp and index envelope see flow chart
  ;ifq1   = cpspch(p5)*3          ; N1:N2 is 3:2, imax=5
  ifq1   = p5*3
  if1    = 1                     ; duration ca. .5 sec
  ;  ifq2   = cpspch(p5)*2
  ifq2   = p5*2
  if2    = 1
  imax   = p6
  imin   = 2
  
     aenv  oscili   iamp, 1/idur, ifenv                ; envelope
  
     adyn  oscili   ifq2*(imax-imin), 1/idur, ifdyn    ; index
     adyn  =        (ifq2*imin)+adyn                   ; add minimum value
     amod  oscili   adyn, ifq2, if2                    ; modulator
  
     a1    oscili   aenv, ifq1+amod, if1               ; carrier
           out      a1
endin
")


(defun csound-output (stream karo &key arpeggio single-voice output-file &allow-other-keys)
  (let ((instruments (csound-default-instruments)))
    ;; create csound source file from karo
    (format stream "<CsoundSynthesizer>
;;;; ~A ~A
;;;; Karo Engine ~A ~A
<CsOptions>
csound -R -W -f -d -o ~A temp.orc temp.sco
</CsOptions>
<CsInstruments>
~A
</CsInstruments>~%"
	    (today)
	    *karo-composer*
	    *karo-version*
	    *karo-version-date*
	    (if output-file
		output-file
		(format nil "karo-~D.wav" (if (karo-p karo) (karo-index karo) 0)))
	    instruments)
    (format stream "<CsScore>
f1 0 512 10 1 ; sine
; envelopes
f51 0 1024  5  .0001 200 1 674 1 150 .0001       ; amplitude envelope
f52 0 1024  5  1 1024 .0001                      ; index envelope
;ins strt dur amp freq(p5)
~{~A~%~}
</CsScore>
</CsoundSynthesizer>"
	    (csound-notes karo :arpeggio arpeggio :single-voice single-voice))))


(defun csound-file (filename karo
		    &key arpeggio single-voice output-file
		    &allow-other-keys)
  (default output-file (format nil "~A.wav" (pathname-name filename)))
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (csound-output s karo 
		   :arpeggio arpeggio
		   :single-voice single-voice
		   :output-file output-file))
  (values filename))


(defvar *tuning-a* 440)
;(defun base-tone () (/ *tuning-a* (expt 2 (/ 9 *notes-in-group*))))
(defun base-tone () 
  ;; middle C
  (/ *tuning-a* (expt 2 (/ 9 12))))

(defun note-to-pitch (note)
  (* (base-tone) (expt 2 (/ note *notes-in-group*))))


(defun csound-notes (karo &key arpeggio single-voice)
  (loop for ch in (if arpeggio karo (as-chords karo))
     for start = 1 then (+ start (* (if arpeggio 
					(if (eq single-voice t)
					    1
					    *n-ad-size*)
					1) duration))
     with duration = 1
     with amp = 5000
     with note-format = "i~D ~D ~D ~D ~D 4"
     append (cond (single-voice
		   (list
		    (format nil note-format
			    1
			    start
			    duration
			    amp
			    (note-to-pitch (if (integerp single-voice)
					       (nth single-voice ch)
					       ch)))))
		  (t
		   (loop for note in ch
		      for n = 1 ; then (1+ n)
		      collect (format nil note-format
				      n
				      (+ start (* duration 
						  (if arpeggio 
						      (- n 1) 
						      0)))
				      duration
				      amp
				      (note-to-pitch note)))))))


(defun csound-files (file-name-trunk 
		     notes ; as chords
		     &key
		     (directory "/tmp")
		     (chords t)
		     (arpeggio t))
  (flet ((output-file-name (suffix)
	   (format nil "~A/~A_~A.csd" directory file-name-trunk suffix)))
    (let (r)
      (when chords
	(push (csound-file (output-file-name "chords") notes) r))
      (when arpeggio
	(push (csound-file (output-file-name "arp") 
			   (apply #'mapcan #'list notes) 
			   :arpeggio t :single-voice t)
	      r))
      (values r))))
    
		     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ALL DONE!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(about)

;; (in-package "CL-USER")

;; EOF
