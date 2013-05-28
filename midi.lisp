;;;;
;;;; midi.lisp
;;;; 
;;; Copyright (c) 2013 Ola Rinta-Koski
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; http://ola.rinta-koski.net/

(defpackage :midi
  (:use :common-lisp))

(in-package :midi)

(defconstant +version+ "1.2 2010-12-29 08:35:00")
(defun version () (values +version+))


(defun sequence-to-string (sequence)
  (let ((string (make-string (length sequence))))
    (loop for i below (length sequence)
	 for c = (elt sequence i)
       do (setf (elt string i)
		(etypecase c
		  (character c)
		  (number (code-char c)))))
    string))


(defun string-to-sequence (string)
  (let ((sequence (make-sequence '(vector unsigned-byte) (length string))))
    (loop for i below (length string)
       do (setf (elt sequence i) (char-code (elt string i))))
    sequence))


(unless (boundp '+valid-formats+)
  (defconstant +valid-formats+ '((single-track  . 0)
				 (multi-track   . 1)  ;; multiple tracks, synchronous
				 (multi-pattern . 2))) ;; multiple tracks, asynchronous
    
  (defconstant +header-chunk-id+ (string-to-sequence "MThd"))
  (defconstant +track-chunk-id+ (string-to-sequence "MTrk"))
  
  (defconstant +valid-time-codes+ '(24 25 29 30))
  
  (defconstant +general-midi-instrument-patch-map+
    '(nil acoustic-grand-piano bright-acoustic-piano electric-grand-piano
    honky-tonk-piano electric-piano-1 electric-piano-2 harpsichord
    clavi celesta glockenspiel music-box vibraphone marimba xylophone
    tubular-bells dulcimer drawbar-organ percussive-organ rock-organ
    church-organ reed-organ accordion harmonica tango-accordion
    acoustic-guitar-nylon acoustic-guitar-steel electric-guitar-jazz
    electric-guitar-clean electric-guitar-muted overdriven-guitar
    distortion-guitar guitar-harmonics acoustic-bass
    electric-bass-finger electric-bass-pick fretless-bass slap-bass-1
    slap-bass-2 synth-bass-1 synth-bass-2 violin viola cello
    contrabass tremolo-strings pizzicato-strings orchestral-harp
    timpani string-ensemble-1 string-ensemble-2 synthstrings-1
    synthstrings-2 choir-aahs voice-oohs synth-voice orchestra-hit
    trumpet trombone tuba muted-trumpet french-horn brass-section
    synthbrass-1 synthbrass-2 soprano-sax alto-sax tenor-sax
    baritone-sax oboe english-horn bassoon clarinet piccolo flute
    recorder pan-flute blown-bottle shakuhachi whistle ocarina
    lead-1-square lead-2-sawtooth lead-3-calliope lead-4-chiff
    lead-5-charang lead-6-voice lead-7-fifths lead-8-bass+lead
    pad-1-new-age pad-2-warm pad-3-polysynth pad-4-choir pad-5-bowed
    pad-6-metallic pad-7-halo pad-8-sweep fx-1-rain fx-2-soundtrack
    fx-3-crystal fx-4-atmosphere fx-5-brightness fx-6-goblins
    fx-7-echoes fx-8-sci-fi sitar banjo shamisen koto kalimba bag-pipe
    fiddle shanai tinkle-bell agogo steel-drums woodblock taiko-drum
    melodic-tom synth-drum reverse-cymbal guitar-fret-noise
    breath-noise seashore bird-tweet telephone-ring helicopter
    applause gunshot)))

(defun seq= (seq1 seq2 &key end1)
  (not (mismatch seq1 seq2 :end1 end1)))

(defun header-chunk? (sequence)
  (seq= sequence +header-chunk-id+ :end1 4))

(defun track-chunk? (sequence)
  (seq= sequence +track-chunk-id+ :end1 4))

(defun valid-chunk-id? (sequence)
  (or (header-chunk? sequence)
      (track-chunk? sequence)))

(defun byte-to-number (sequence index)
  (char-code (elt sequence index)))


(defun read-number (sequence &key (length 4))
  (declare (fixnum length))
  (flet ((getnum (sequence nth)
	   (if (stringp sequence)
	       (byte-to-number sequence nth)
	       (elt sequence nth))))
    (cond ((= length 4) (+ (* (getnum sequence 0) #.(* #x100 #x100 #x100))
			   (* (getnum sequence 1) #.(* #x100 #x100))
			   (* (getnum sequence 2) #x100)
			   (getnum sequence 3)))
	  ((= length 2)
	   (+ (* (getnum sequence 0) #x100)
	      (getnum sequence 1)))
	  ((= length 1)
	   (getnum sequence 0))
	  (t
	   (error "Number length (in bytes) should be 1, 2 or 4 instead of ~A -- READ-NUMBER"
		  length)))))


(defun get-number (sequence &key (length 4))
  (let ((temp-seq (make-sequence '(vector unsigned-byte) length :initial-element 0)))
    (read-sequence temp-seq sequence :end length)
    (read-number temp-seq :length length)))


(defun number-to-variable-length (number)
  (declare (type (integer 0 #x0FFFFFFF) number))
  (loop
     with variable-length = (make-array 4 :element-type 'unsigned-byte
					:initial-element 0 :fill-pointer 0)
     with initial = t
     do
       (if (not initial)
	   (vector-push (logior (logand number #x7F) #x80) variable-length)
	   (vector-push (logand number #x7F) variable-length))
       (when (= (logandc2 number #x7F) 0)
	 (return-from number-to-variable-length (nreverse variable-length)))
       (setf initial nil
	     number (ash number -7))))


(defun variable-length-to-number (variable-length)
  (let ((number 0))
    (loop for i from 0 to (1- (length variable-length))
       do (setf number (logior (logand (elt variable-length i) #x7F) number))
	 (unless (> (logand (elt variable-length i) #x80) 0)
	   (return-from variable-length-to-number number))
	 (setf number (ash number 7)))))


(defun read-variable-length (stream)
  (let ((variable-length (make-array 4 :element-type 'unsigned-byte
				     :initial-element 0 :fill-pointer 0))
	byte)
    (loop do
	 (setf byte (read-byte stream))
	 (vector-push byte variable-length)
       while (> (logand byte #x80) 0))
    (variable-length-to-number variable-length)))


(defun write-variable-length (number stream)
  (let ((variable-length (number-to-variable-length number)))
    (write-sequence variable-length stream)
    (values (length variable-length))))


(defun print-as-hex (stream sequence)
  (dotimes (i (length sequence))
    (format stream "~2,'0X " (elt sequence i))))


(defun midi-format (format)
  (unless (member format +valid-formats+ :test #'(lambda (x y)
						   (eql x (car y))))
    (error "~A is not a valid MIDI file format -- MIDI-FORMAT" format))
  (cdr (assoc format +valid-formats+)))


(defun write-word (word stream)
  (declare (type (integer 0 #xFFFF) word))
  (write-byte (ash word -8) stream)
  (write-byte (logand word #xFF) stream))


(defun write-long-word (word stream)
  (declare (type (integer 0 #xFFFF) word))
  (write-byte (ash word -24) stream)
  (write-byte (ash word -16) stream)
  (write-byte (ash word -8) stream)
  (write-byte (logand word #xFF) stream))


(defun header-chunk (stream format tracks &key time-code ticks-per-frame division)
  (declare (symbol format)
	   (fixnum tracks division))
  (write-sequence +header-chunk-id+ stream)
  (write-sequence (vector #x00 #x00 #x00 #x06) stream)
  (write-word (midi-format format) stream)
  (write-word tracks stream)
  (if time-code
      (progn
	(write-byte time-code stream)
	(write-byte ticks-per-frame stream))
      (write-word division stream)))


(defclass midi-event ()
  ((delta-time :accessor delta-time :initarg :delta-time :initform 0)
   (event-type :accessor event-type :initarg :event-type :initform #x01)))


(defmethod write-event :around ((event midi-event) stream)
  (let ((event-size (+ (write-variable-length (delta-time event) stream)
		       (call-next-method))))
    ;; (format t "Event size ~D~%" event-size)
    (values event-size)))

(defmethod channel ((event midi-event))
  0)

(defmethod output-midi (stream (event midi-event))
  (write-event event stream))


(defclass midi-track ()
  ((events :accessor events :initarg :events :initform nil)))

(defmethod enqueue ((track midi-track) (event midi-event))
  ;; (format t "~&Adding ~A to track~%" event)
  ;; (describe event)
  (setf (events track) (nconc (events track) (list event))))

(defclass midi-file ()
  ((format :accessor file-format
	   :initarg :format
	   :initform 'single-track)
   (tracks :accessor tracks
	   :initarg :tracks
	   :initform nil)
   (timecode :accessor timecode
	     :initarg :timecode
	     :initform nil)
   (ticks-per-quarter-note :accessor ticks-per-quarter-note
			   :initarg :ticks-per-quarter-note
			   :initform 96)
   (ticks-per-frame :accessor ticks-per-frame
		    :initarg :ticks-per-frame
		    :initform nil)))


(defmethod write-file ((midi-file midi-file) name &key supersede)
  (with-open-file (file name
			:direction :output
			:element-type 'unsigned-byte
			:if-exists (if supersede :supersede :error))
    (output-midi file midi-file)))


(defmethod output-midi (stream (midi-file midi-file))
  (header-chunk stream (file-format midi-file) (length (tracks midi-file))
		:time-code (timecode midi-file)
		:ticks-per-frame (ticks-per-frame midi-file)
		:division (ticks-per-quarter-note midi-file))
  (dolist (track (tracks midi-file))
    (output-midi stream track)))


(defmethod output-midi (stream (midi-track midi-track))
  (write-sequence +track-chunk-id+ stream)
  (let ((chunk-length-position (file-position stream)))
    (write-sequence (vector #x01 #x02 #x03 #x04) stream) ;; Placeholder for chunk length
    ;; (format t "~D events in track~%" (length (events midi-track)))
    (let* ((chunk-length (reduce #'+ (mapcar #'(lambda (event)
						 (output-midi stream event))
					     (append (events midi-track)
						     (list (make-instance 'end-of-track))))))
	   (final-position (file-position stream)))
      (unless (file-position stream chunk-length-position)
	(error "Cannot seek back to write chunk length"))
      (write-long-word chunk-length stream)
      (unless (file-position stream final-position)
	(error "Cannot seek forward to end of chunk"))
      ;; (format t "chunk length ~D~%" chunk-length)
      (values (+ chunk-length 4 4)))))


(defun skip-over (stream how-many)
  (let ((dummy-sequence (make-sequence '(vector unsigned-byte) how-many)))
    (read-sequence dummy-sequence stream :end how-many)))


(defun read-midi-stream (stream)
  (let ((chunk-id (make-sequence '(vector unsigned-byte) 4 :initial-element 0))
	format tracks timecode-format ticks)
    (loop
       ;; 
       while (> (read-sequence chunk-id stream :end 4) 0)
       do
	 (unless (valid-chunk-id? chunk-id)
	   (error "Unknown chunk designator ~A -- READ-MIDI-FILE" chunk-id))
	 ;; (format t "Found chunk ~A~%" (sequence-to-string chunk-id))
	 (let ((chunk-length (get-number stream)) tmp)
	   (cond ((header-chunk? chunk-id)
		  (setf format (get-number stream :length 2)
			tracks (get-number stream :length 2)
			tmp (get-number stream :length 2))
		  (if (> (logand tmp #x8000) 0)
		      (setf timecode-format (logand tmp #x7F00)
			    ticks (logand tmp #x7F))
		      (setf ticks (logand tmp #x7FFF)))
		  
		  (when (> chunk-length 6)
		    ;; Discard rest of header chunk, if it is longer than 6 bytes
		    ;; (SMF 1.0 specifies only the first 6 bytes)
		    (skip-over stream (- chunk-length 6))))
		 ;;
		 ((track-chunk? chunk-id)
		  ;; (format t "Skipping over ~A chars~%" chunk-length)
		  (skip-over stream chunk-length))
		 ;;
		 (t
		  (error "Cannot identify known chunk -- READ-MIDI-FILE")))))
    (values format tracks timecode-format ticks)))

(defun read-midi-file (file)
  (with-open-file (stream file :element-type 'unsigned-byte)
    (read-midi-stream stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun data-to-msb-lsb (value)
  (values (logand (ash value -7) #x7f)
	  (logand value #x7f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Track Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass track-event (midi-event)
  ((event-data :accessor event-data :initarg nil)))

(defmacro create-event (name event-code &body body)
  (let ((code (gensym)))
    `(let ((,code ,event-code))
       (defclass ,name (track-event)
	 ())
       (defmethod initialize-instance :after ((this ,name) &key note velocity controller new-value channel &allow-other-keys)
	 (setf (event-type this) ,code)
	 (setf (event-data this) ,@body)))))

(defmethod write-event ((event track-event) stream)
  (let ((event-sequence (concatenate '(vector unsigned-byte)
				     (vector (logior (event-type event) (channel event)))
				     (event-data event))))
    ;; (format t "event type ~A channel ~A event data ~A sequence ~A~%" (event-type event) (channel event) (event-data event) event-sequence)
    (write-sequence event-sequence stream)
    (values (length event-sequence))))

(create-event note-off #x80 (vector note velocity))
(create-event note-on #x90 (vector note velocity))
(create-event key-aftertouch #xa0 (vector note velocity))
(create-event control-change #xb0 (vector controller new-value))
(create-event program-change #xc0 (vector new-value))
(create-event channel-aftertouch #xd0 (vector channel))
(create-event pitch-wheel #xe0 (multiple-value-bind (msb lsb)
				   (data-to-msb-lsb new-value)
				 (vector lsb msb)))

;;;;;;;;;;;;;;;;

(defclass rpn-event (track-event)
  ((msb :accessor msb :initarg :msb)
   (lsb :accessor lsb :initarg :lsb)))

(defmethod write-event ((event rpn-event) stream)
  (let* ((cmd-byte (logior #xb0 (channel event)))
	 (event-sequence (vector cmd-byte #x65 (msb event)
				 0 cmd-byte #x64 (lsb event)
				 0 cmd-byte #x06 (first (event-data event))
				 0 cmd-byte #x26 (second (event-data event))
				 0 cmd-byte #x65 #x7f
				 0 cmd-byte #x64 #x7f)))
    ;; (format t "~&~A~%" event-sequence)
    (write-sequence event-sequence stream)
    (values (length event-sequence))))

(defmacro create-rpn-event (name lsb msb)
  `(progn
     (defclass ,name (rpn-event)
       ())
     (defmethod initialize-instance :after ((this ,name) &key new-value &allow-other-keys)
       (multiple-value-bind (msb-data lsb-data)
	   (data-to-msb-lsb new-value)
	 (setf (msb this) ,msb
	       (lsb this) ,lsb
	       (event-data this) (list msb-data lsb-data))))))

(create-rpn-event pitch-wheel-range #x00 #x00)
(create-rpn-event channel-fine-tuning #x00 #x01)
(create-rpn-event channel-coarse-tuning #x00 #x02)
(create-rpn-event tuning-program-change #x00 #x03)
(create-rpn-event tuning-bank-select #x00 #x04)
(create-rpn-event modulation-depth-range #x00 #x05)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Meta-Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass meta-event (midi-event)
  ((metadata :accessor metadata :initarg :metadata)))

(defmethod write-event ((event meta-event) stream)
  (let* ((metadata (etypecase (metadata event)
		     (sequence (metadata event))
		     (number (vector (metadata event)))))
	 ;;(metadata-length (number-to-variable-length (length metadata)))
	 (output-sequence  (concatenate '(vector unsigned-byte)
				 (vector #xFF (event-type event))
				 ;; metadata-length
				 metadata)))
    (write-sequence output-sequence stream)
    (values (length output-sequence))))


(defclass midi-text-event (meta-event)
  ())

(defmethod write-event ((text-event midi-text-event) stream)
  (let* ((text (metadata text-event))
	 (text-length (number-to-variable-length (length text))))
    (write-sequence (concatenate '(vector unsigned-byte)
				 (vector #xFF (event-type text-event))
				 text-length
				 (string-to-sequence text))
		    stream)
    (values (+ 2 (length text-length) (length text)))))




(defclass sequence-number (midi-event)
  ((id :accessor id :initarg :id)))

(defmethod write-event ((sequence-number sequence-number) stream)
  ;; (declare (type (integer 0 #xFFFF) id))
  (write-sequence (concatenate '(vector unsigned-byte)
			       (vector #xFF #x00 #x02)
			       (vector (logand (ash (id sequence-number) -8) 
					       #xFF))
			       (vector (logand (id sequence-number) 
					       #xFF)))
		  stream)
  (values 5))

(defmacro create-meta-event (name code &body body)
  `(progn
     (defclass ,name (meta-event)
       ())
     (defmethod initialize-instance :after ((this ,name) &rest initargs)
       (declare (ignore initargs))
       (setf (event-type this) ,code
	     (metadata this) (progn ,@body)))))

(defmacro create-text-event (name code)
  `(progn
     (defclass ,name (midi-text-event)
       ())
     (defmethod initialize-instance :after ((this ,name) &key metadata &allow-other-keys)
       (setf (event-type this) ,code
	     (metadata this) metadata))))

;; (create-meta-event sequence-number #x00)
(create-text-event text-event #x01)
(create-meta-event copyright-notice #x02)
(create-text-event sequence-or-track-name #x03)
(create-meta-event instrument-name #x04)
(create-meta-event lyric #x05)
(create-meta-event marker #x06)
(create-meta-event cue-point #x07)
;;(create-meta-event channel-prefix-assignment #x20)
(create-meta-event end-of-track #x2f #x00)
;;(create-meta-event tempo-setting #x51)
;;(create-meta-event smpte-offset #x54)
;;(create-meta-event time-signature #x58)
;;(create-meta-event key-signature #x59)
					;(create-meta-event sequencer-specific-event #x7f)

;;(defun end-of-track ()
;;  (vector #xFF #x2F #x00))

(defun set-tempo (microseconds-per-midi-quarter-note)
  (declare (type (integer 0 #xFFFFFF) microseconds-per-midi-quarter-note))
  (vector #xFF #x51 #x03
	  (logand (ash microseconds-per-midi-quarter-note -16) #xFF)
	  (logand (ash microseconds-per-midi-quarter-note -8) #xFF)
	  (logand microseconds-per-midi-quarter-note #xFF)))

(defun smpte-offset (hour minute second frame frame-fraction)
  (declare (type (integer 0 #xFF) hour)
	   (type (integer 0 59) minute second)
	   (type (integer 0 30) frame)
	   (type (integer 0 99) frame-fraction))
  (vector #xFF #x51 #x03 hour minute second frame frame-fraction))

(defun time-signature (numerator
		       denominator
		       midi-clocks-per-metronome-click
		       notated-32nd-notes-in-midi-quarter-note)
  (declare (type (integer 1 *) numerator)
	   (type (integer 0 *) denominator);; really 2^-denominator, so 0 is possible
	   (type (integer 0 #xFF)
		 midi-clocks-per-metronome-click
		 notated-32nd-notes-in-midi-quarter-note))
  (vector #xFF #x58 #x04
	  numerator denominator
	  midi-clocks-per-metronome-click
	  notated-32nd-notes-in-midi-quarter-note))

(defun key-signature (flats-or-sharps is-major)
  (declare (type (integer -7 7) flats-or-sharps))
  (vector #xFF #x59 #x02
	  flats-or-sharps
	  (if is-major 0 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +pitch-bend-range+ 8192) ;; range a = 1/6, max pb msg = 8192

(defun note-to-midi-note (note-name-or-number &key (octave 4) (z 12))
  (unless (and (>= octave 0) (<= octave 10))
    (error "OCTAVE must be between 0 and 10, not ~A -- NOTE-TO-MIDI-NOTE" octave))
  (flet ((round-fn (x) (floor x)))
    (let (note pitch-bend)
      (cond ((= z 12)
	     (setf pitch-bend 0
		   note (+ (* octave 12)
			   (etypecase note-name-or-number
			     (number note-name-or-number)
			     (symbol (position (string-upcase (symbol-name note-name-or-number))
					       '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B")
					       :test #'string=))))))
	    (t
	     (unless (numberp note-name-or-number)
	       (error "Only note numbers accepted when octave has ~A notes, not ~A -- NOTE-TO-MIDI-NOTE"
		      z note-name-or-number))
	     (let* ((r (the float (/ (float note-name-or-number) z)))
		    (n (round-fn (* 12 r)))
		    (p (round-fn (* +pitch-bend-range+ (* 12 (- r (/ n 12.0)))))))
	       (setf note (+ (* octave 12) n)
		     pitch-bend (+ +pitch-bend-range+ p)))))
      (values note pitch-bend))))

(defmethod add-event ((midi midi-file) event-type &key note velocity new-value (delta-time 0) (track 0))
  (let ((event (make-instance event-type :note note :velocity velocity :new-value new-value)))
    (setf (delta-time event) delta-time)
    (enqueue (nth track (tracks midi)) event)
    event))

(defmacro default-value (name default)
  ;; NOTE: for obvious reasons only usable for simple SETF-able places
  `(unless ,name (setf ,name ,default)))

(defmethod add-note ((midi midi-file) note
		     &key velocity duration off-velocity
		     octave (notes-per-octave 12) (on t) (off t))
  (default-value velocity 127)
  (default-value duration 96)
  (default-value octave 4)
  (default-value off-velocity velocity)
  (unless (and velocity duration octave notes-per-octave)
    (error "Problem with VELOCITY, DURATION, OCTAVE or NOTES-PER-OCTAVE: ~A ~A ~A ~A"
	   velocity duration octave notes-per-octave))
  (multiple-value-bind (note pitch-bend)
   (note-to-midi-note note :octave (or octave 4) :z (or notes-per-octave 12))
    (unless (and note pitch-bend)
      (error "Problem with NOTE or PITCH-BEND: ~A ~A" note pitch-bend))
    (unless (= notes-per-octave 12)
      (add-event midi 'pitch-wheel :new-value pitch-bend :delta-time 0))
    (unless (or on off)
      (error "Neither NOTE-ON nor NOTE-OFF has been specified"))
    (when on
      (add-event midi 'note-on :note note :velocity velocity :delta-time 0))
    (when off
      (add-event midi 'note-off :note note :velocity (or off-velocity velocity) :delta-time duration))))

(defmethod add-chord ((midi midi-file) notes
		      &key velocity duration off-velocity octave notes-per-octave)
  (dolist (note notes)
    (add-note midi note :on t :off nil
	      :velocity velocity :duration 0 :off-velocity off-velocity
	      :octave octave :notes-per-octave notes-per-octave))
  (add-note midi (first notes) :on nil :off t
	    :velocity velocity :duration duration :off-velocity off-velocity
	    :octave octave :notes-per-octave notes-per-octave)
  (dolist (note (rest notes))
    (add-note midi note :on nil :off t
	      :velocity velocity :duration 0 :off-velocity off-velocity
	      :octave octave :notes-per-octave notes-per-octave)))

(defmethod add-text-event ((midi midi-file) event-type text &key (track 0))
  (let ((event (make-instance event-type :metadata text)))
    (enqueue (nth track (tracks midi)) event)
    event))

(defmethod add-text-event ((midi-track midi-track) event-type text &key track)
  (declare (ignore track))
  (let ((event (make-instance event-type :metadata text)))
    (enqueue midi-track event)
    event))

(defmethod new-track ((file midi-file) &key name)
  (let ((track (make-instance 'midi-track)))
    (when name
      (add-text-event track 'sequence-or-track-name name))
    (setf (tracks file) (nconc (tracks file) (list track)))
    track))

(defun find-instrument (instrument)
  (when instrument
    (position (intern (symbol-name instrument) "MIDI") +general-midi-instrument-patch-map+)))

(defmethod use-instrument ((file midi-file) &optional new-instrument)
  (let ((instrument (or (find-instrument new-instrument) 1)))
    ;; (format t "~&Changing instrument to ~A~%" instrument)
    (add-event file 'program-change :new-value instrument)))

(defmethod set-pitch-bend ((file midi-file) &optional new-range)
  (unless (and (integerp new-range) (>= new-range 0) (<= new-range 24))
    (setq new-range 1))
  ;; (format t "~&Changing pitch bend-range to ~A~%" new-range)
  (add-event file 'pitch-wheel-range :new-value (ash new-range 7)))

(defmacro with-midi-output ((var file-name
				 &key title instrument pitch-bend-range)
			    &body body)
  (let ((fname (gensym)))
    `(let ((,fname ,file-name)
	   (,var (make-instance 'midi-file)))
       (new-track ,var :name ,title)
       (use-instrument ,var ,instrument)
       (set-pitch-bend ,var ,pitch-bend-range)
       ,@body
       (write-file ,var ,fname :supersede t)
       ,fname)))

#|
(progn
  (setf user::*foo* (make-instance 'midi::midi-file))
  (push (make-instance 'midi::midi-track) (midi::tracks user::*foo*))
  (push (make-instance 'midi::cue-point :text "foo bar") (midi::events (first (midi::tracks user::*foo*))))
  (push (make-instance 'midi::lyric :text "baz bat") (midi::events (first (midi::tracks user::*foo*)))))

(midi::write-file user::*foo* #p"/tmp/foo.mid" :supersede t)
|#
