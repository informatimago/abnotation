;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Reads and writes MIDI files into an ABNotation partition.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-10-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "ABNOTATION.CORE")


;;;
;;; PROCESS-EVENT
;;;

(defgeneric process-event (state event)
  (:method (state event)
    (declare (ignore state event))
    :ignored))

;;;
;;; TIMINGS
;;;

(defclass timings ()
  ((numerator                  :initform  4
                               :initarg :numerator
                               :reader timings-numerator)
   (denominator                :initform  4
                               :initarg :denominator
                               :reader timings-denominator)
   (midi-clock/metronome-click :initform 24
                               :initarg :midi-clock/metronome-click
                               :reader timings-midi-clock/metronome-click)
   (32ths/24-midi-clock        :initform 8
                               :initarg :32ths/24-midi-clock
                               :reader timings-32ths/24-midi-clock
                               :documentation "this gives the length of the quater-note:
24 * midi-clock = 32ths/24-midi-clock * 32ths
1 * quater-note = 8 * 32ths
1 * quater-note = 8 * 24 * midi-clock / 32ths/24-midi-clock

32ths/24-midi-clock * quater-note / 192 =  1 * midi-clock
")
   (division                   :initform 240
                               :initarg :division
                               :reader timings-division
                               :documentation "delta-time per quarter-note")
   (tempo-µs                   :initform 500000 ; = 120 BPM
                               :initarg :tempo-µs
                               :reader timings-tempo-µs
                               :documentation "µs per quarter-note")
   (frame/second               :initform 25
                               :initarg frame/second
                               :reader timings-frame/second)
   (drop-frame                 :initform nil
                               :initarg :drop-frame
                               :reader timings-drop-frame)
   (dirty                      :initform t)
   tempo ; tempo in second
   unit ; the time-length of a delta-time = 1.
   measure ; the duration of a measure.
   midi-clock ; the duration of a MIDI-clock.
   metronome-click ; the period of the metronome
   maxima long breve whole half filled 8th 16th 32th 64th 128th)
  (:documentation "

The TIMINGS instances keep track of the timing of the MIDI track.  It
takes from midi events the timing information and maintains the
current values, notably TIMINGS-UNIT which gives the time of one
delta-time.

"))

(defmethod initialize-instance ((self timings) &key &allow-other-keys)
  (call-next-method)
  (compute-timings self)
  self)

(defmethod print-object ((self timings) stream)
  (print-parseable-object (self stream :type t :identity nil)
                          numerator denominator midi-clock/metronome-click
                          32ths/24-midi-clock division tempo-µs
                          frame/second drop-frame
                          dirty
                          tempo unit measure midi-clock metronome-click maxima
                          long breve whole half filled 8th 16th 32th 64th 128th))


(defmacro define-timings-setters (&rest names)
  `(progn
     ,@(mapcar (lambda (name)
                 `(defgeneric (setf ,(intern (concatenate 'string
                                               (string 'timings-)
                                               (string name))))
                      (value timings)
                    (:method (value (timings timings))
                      (setf (slot-value timings ',name) value
                            (slot-value timings 'dirty) t))))
               names)))

(define-timings-setters numerator denominator
  midi-clock/metronome-click 32ths/24-midi-clock division tempo-µs
  frame/second drop-frame)

(defgeneric compute-timings (timings))

(defmacro define-timings-results (&rest names)
  `(progn
     ,@(mapcar (lambda (name)
                 `(defgeneric ,(intern (concatenate 'string
                                         (string 'timings-)
                                         (string name)))
                      (timings)
                    (:method ((timings timings))
                      (when (slot-value timings 'dirty)
                        (compute-timings timings))
                      (slot-value timings ',name)))) 
               names)))

(define-timings-results tempo unit measure midi-clock metronome-click
                        maxima long breve whole half filled 8th 16th 32th 64th 128th)

;; http://cs.fit.edu/~ryan/cse4051/projects/midi/midi.html
;; http://fr.wikipedia.org/wiki/Mesure_%28solf%C3%A8ge%29

(defmethod compute-timings ((timings timings))
  "
RETURN: an a-list giving durations in second, with the following keys:
        :unit       the time-length of a delta-time = 1.
        :measure    the duration of a measure.
        :midi-clock the duration of a MIDI-clock.
        :metronome  the period of the metronome
        :maxima :long  :breve :whole  :half   :filled :8th    :16th   :32th   :64th   :128th
                       carrée ronde   blanche noire   croche  2croche 3croche 4croche 5croche
        the durations of the various notes.
"
  (with-slots (numerator denominator midi-clock/metronome-click
                         32ths/24-midi-clock division tempo-µs
                         ;; frame/second drop-frame

                         dirty
                         
                         tempo unit measure midi-clock metronome-click maxima
                         long breve whole half filled 8th 16th 32th 64th 128th) timings
    (when dirty
      ;; divisions: 240
      ;; This may be in either of two formats, depending on the value of MS bit:
      ;; 
      ;; Bit:	15	14 ... 8	7 ... 0
      ;; <division>	0	ticks per quarter note
      ;; 1	-frames/second	ticks / frame
      (setf tempo            (/ tempo-µs 1000000)
            unit             (if (plusp division)
                                 (/ tempo division)
                                 (error "smpte not implemented yet")
                                 #+not-implemented-yet
                                 (let ((frames/second (truncate division 256))
                                       (ticks/frame   (ldb (byte 8 0) division)))
                                   (error "smpte not implemented yet")))

            measure          (* tempo numerator)
            maxima           (* tempo 32)
            long             (* tempo 16)
            breve            (* tempo  8)
            whole            (* tempo  4)
            half             (* tempo  2)
            filled              tempo
            8th              (/ tempo  2)
            16th             (/ tempo  4)
            32th             (/ tempo  8)
            64th             (/ tempo 16)
            128th            (/ tempo 32)
            
            midi-clock       (/ (* 32ths/24-midi-clock filled) 192) ; second/midi-clock
            metronome-click  (* midi-clock midi-clock/metronome-click)
            dirty            nil))))



(defmethod process-event ((timings timings) (file midifile))
  (setf (timings-division timings) (midifile-division file)))

(defmethod process-event ((timings timings) (message time-signature-message))
  (setf (timings-numerator                  timings) (message-numerator message)
        (timings-denominator                timings) (expt 2 (message-denominator message))
        (timings-midi-clock/metronome-click timings) (slot-value message 'midi::cc)
        (timings-32ths/24-midi-clock        timings) (slot-value message 'midi::bb)))

(defmethod process-event ((timings timings) (message tempo-message))
  (setf (timings-tempo-µs timings)   (message-tempo message)))

(defmethod process-event ((timings timings) (message smpte-offset-message))
  (case (ldb (byte 2 6) (slot-value message 'midi::hr))
    ((0)  (setf (timings-frame/second timings) 24
                (timings-drop-frame timings) nil))
    ((1)  (setf (timings-frame/second timings) 25
                (timings-drop-frame timings) nil))
    ((2)  (setf (timings-frame/second timings) 30
                (timings-drop-frame timings) t))
    ((3)  (setf (timings-frame/second timings) 30
                (timings-drop-frame timings) nil))))






;; (timings 2000000/1000000 240 4 2 24 8)
;; ((:unit . 1/120)
;;  (:midi-clock . 1/12)
;;  (:measure . 8)
;;  (:metronome . 2)
;;  (:maxima . 64)
;;  (:long . 32)
;;  (:breve . 16)
;;  (:whole . 8)
;;  (:half . 4)
;;  (:filled . 2)
;;  (:8th . 1)
;;  (:16th . 1/2)
;;  (:32th . 1/4)
;;  (:64th . 1/8)
;;  (:128th . 1/16))




;;;
;;; MIDI-NOTE stores the instantaneous state of a MIDI note. 
;;; 

(defgeneric duration (note))
(defgeneric note-on  (note event time))
(defgeneric note-off (note event time))
 ;; (defgeneric quantized-duration (note &optional timings))


(defclass midi-note ()
  ((key                 :initarg :key :reader key)
   (initial-velocity    :initarg :initial-velocity :initform 64   :accessor initial-velocity)
   (current-velocity    :initform 0   :accessor current-velocity)
   (note-is-on          :initform nil :accessor note-is-on)
   (note-on-time        :initarg :note-on-time     :initform 0   :accessor note-on-time)
   (note-off-time       :initarg :note-off-time    :initform 0   :accessor note-off-time)
   (timings             :initarg :timings :reader timings)))

(defmethod print-object ((self midi-note) stream)
  (print-parseable-object (self stream :type t :identity nil)
                          key
                          (:note-on-time (coerce (note-on-time self) 'single-float))
                          (:duration (coerce (duration self) 'single-float))
                          initial-velocity
                          current-velocity))

(defmethod duration ((note midi-note))
  (if (note-is-on note)
      0
      (- (note-off-time note) (note-on-time note))))

;; (defmethod accidentals ((note midi-note))
;;   ;; TODO.   Is it what's displayed? Because the pitch is alread 0..127 so it should cover the sharps and flats…
;;   ;;; :natural :flat :double-flat :sharp or :double-sharp.
;;   :natural)
;; 
;; (defmethod quantized-duration ((duration real) &optional timings)
;;   (with-slots (maxima long breve whole half filled 8th 16th 32th 64th 128th) timings
;;     (loop
;;       :for head  :in  '(:filled :filled :filled :filled :filled :filled :half :whole :breve :long :maxima)
;;       :for beams :in  '(5       4       3       2       1       0       0     0      0      0     0)
;;       :for head-duration :in  (list 128th 64th 32th 16th 8th filled half whole breve long maxima)
;;       :while (< head-duration duration)
;;       :finally (loop
;;                  :with dots = 0
;;                  :for over-duration =  head-duration :then (/ over-duration 2)
;;                  :for over =  (-  duration over-duration) :then (- over over-duration)
;;                  :while (and (< dots 3) (plusp over))
;;                  :do (incf dots)
;;                  :finally (return-from quantized-duration (values head beams dots))))))
;; 
;; (defmethod quantized-duration ((note midi-note) &optional (timings (timings note)))
;;   (quantized-duration (duration note) timings))


(defmethod note-on ((note midi-note) event time)
  (assert (= (key note) (message-key event)))
  (if (note-is-on note)
      (setf (current-velocity note) (message-velocity event))
      (setf (initial-velocity note) (message-velocity event)
            (current-velocity note) (message-velocity event)
            (note-is-on note) t
            (note-on-time note) time)))

(defmethod note-off ((note midi-note) event time)
  (assert (= (key note) (message-key event)))
  (when (note-is-on note)
    (setf (note-is-on note) nil
          (current-velocity note) (message-velocity event)
          (note-off-time note) time)))



;;;
;;; output protocol
;;; 

(defgeneric output-note    (output note))
(defgeneric output-silence (output duration))
(defgeneric output-measure (output when measure))


;;;
;;; MIDI-STATE stores the instantaneous state of all MIDI notes, and
;;; process the MIDI events. 
;;;


(defclass midi-state ()
  ((timings      :initform (make-instance 'timings)
                 :initarg :timings
                 :reader timings)
   (notes        :initform (make-array '(16 128) :initial-element nil)
                 :reader  notes)
   (output       :initform t
                 :initarg :output
                 :reader output)))


(defgeneric get-note (state event)
  (:documentation "
Returns the MIDI-NOTE of the STATE for the pitch and channel
specified by the midi EVENT.
")
  (:method ((state midi-state) (event midi::voice-message))
    (or (aref (notes state) (message-channel event) (message-key event))
        (setf (aref (notes state) (message-channel event) (message-key event))
              (make-instance 'midi-note
                  :key (message-key event)
                  :timings (timings state))))))

(defgeneric reset-note (state event)
  (:method ((state midi-state) (event midi::voice-message))
    (setf (aref (notes state) (message-channel event) (message-key event))
          (make-instance 'midi-note
              :key (message-key event)
              :timings (timings state)))))




(defmethod process-event ((state midi-state) event)
  (call-next-method)
  (let* ((timings     (timings state))
         (old-measure (timings-measure timings)))
    (process-event timings event)
    (let ((new-measure  (timings-measure timings)))
      (when (/= old-measure new-measure)
        (output-measure (output state) (message-time event) new-measure)))))


(defun new-note (state note event time)
  (note-off note event time)
  (output-note (output state) note)
  (let ((new-note (reset-note state event)))
    (setf (note-off-time new-note) time)))


(defmethod process-event ((state midi-state) (event note-on-message))
  (let ((note  (get-note state event))
        (time  (* (message-time event) (timings-unit (timings state)))))
    (cond
      ((zerop (message-velocity event))
       (new-note state note event time))
      ((note-is-on note)
       (note-on note event time))
      (t
       (let ((duration (- time (note-off-time note))))
         (when (plusp duration)
           (output-silence (output state) duration))
         (note-on note event time))))))


(defmethod process-event ((state midi-state) (event note-off-message))
  (let ((note (get-note state event))
        (time (* (message-time event) (timings-unit (timings state)))))
    (when (note-is-on note)
      (new-note state note event time))))



;;;
;;; Test output
;;;

(defmethod output-note ((stream t) (note midi-note))
  (print note stream))

(defmethod output-silence ((stream t) duration)
  (format stream "~%(~A ~S ~,9F)" 'silence :duration-s duration))

(defmethod output-measure ((stream t) when measure)
  (format stream "~%(~A ~S ~,9F ~S ~,9F)" 'measure :when when :measure measure))


;;;
;;; abnotation
;;;

(defclass abnotation-sequence ()
  ((notes   :initform (make-queue) :reader notes)
   (measures :initform (make-queue) :reader measures)))

(defmethod print-object ((self abnotation-sequence) stream)
  (print-parseable-object (self stream :type t :identity t)
                          measures notes))

(defmethod output-note ((output abnotation-sequence) (note midi-note))
  (queue-enqueue (notes output) note))

(defmethod output-silence ((output abnotation-sequence) duration)
  (declare (ignore duration))
  (values))

(defmethod output-measure ((output abnotation-sequence) when measure)
  (queue-enqueue (measures output) (cons when measure)))


;;;
;;; load midi files into a partition.
;;;


(defun read-midi-track (track output)
  (let ((state (make-instance 'midi-state :output output)))
    (dolist (event track)
      (check-type event message)
      (process-event state event))
    state))


(defun abnotation-read-midi-file (path)
  (let ((absequence (make-instance 'abnotation-sequence)))
    (read-midi-track (first (midifile-tracks (read-midi-file path)))
                     absequence)
    absequence))

(defun test/abnotation-read-midi-file (path)
  (read-midi-track (first (midifile-tracks (read-midi-file path)))
                   *standard-output*))



;;;
;;; Tests
;;;


(defun test/read-midi ()
  (let ((buffer (make-instance 'abnotation-sequence)))
    (let ((timings (make-instance 'timings))
          (time 0.0))
      (flet ((note (key duration &optional (velocity 64))
               (let ((note  (make-instance 'midi-note :key key :timings timings)))
                 (setf (initial-velocity note) velocity
                       (current-velocity note) velocity
                       (note-on-time note) time
                       (note-off-time note) (incf time duration))
                 note))
             (silence (duration)
               (incf time duration)
               duration))
        (output-measure buffer 0.0 1.75)
        (output-note buffer (note 29 0.25))
        (output-note buffer (note 35 0.25))
        (output-note buffer (note 33 0.25))
        (output-note buffer (note 37 0.50))

        (output-silence buffer (silence 0.50))
        
        (output-note buffer (note 63 0.25))
        (output-note buffer (note 65 0.25))
        (output-note buffer (note 69 0.25))
        (output-note buffer (note 67 0.50))

        (output-silence buffer (silence 0.50))

        (output-note buffer (note 77 0.25))
        (output-note buffer (note 81 0.25))
        (output-note buffer (note 79 0.25))
        (output-note buffer (note 83 0.50))

        (output-silence buffer (silence 0.50))

        (output-note buffer (note 101 0.25))
        (output-note buffer (note 105 0.25))
        (output-note buffer (note 103 0.25))
        (output-note buffer (note 107 0.50))
        
        ))
    buffer))





(defun create-measures-for-notes (measure-durations notes total-duration)
  (let ((measures                 (make-empty-span))
        (min-time                 (note-on-time (first notes)))
        (start-time               (car (first measure-durations)))
        (current-measure-duration (cdr (first measure-durations))))
    (pop measure-durations)
    (when (< min-time start-time)
      (decf start-time (* (ceiling (- start-time min-time) current-measure-duration)
                          current-measure-duration)))
    (flet ((make-measure (duration mnumber start-time)
             (let ((measure (make-instance 'measure :number mnumber :start-time start-time :duration duration)))
               (print (list 'measure mnumber start-time))
               (span-append-node measures measure)
               measure))
           (make-note (midi-note)
             (make-instance 'note
                            :start-time (note-on-time midi-note)
                            :duration (duration midi-note)
                            :dynamic (initial-velocity midi-note)
                            :pitch (key midi-note))))
      (loop ; make each measure
            ;; TODO: if a tempo change occurs in the middle of a measure, we should make it shorter and start over from the tempo change.
            :with next-tempo-change = (car (first measure-durations))
            :for mnumber :from 1
            :for measure = (make-measure current-measure-duration  mnumber start-time)
            :for end-time = (end-time measure)
            :while (< end-time total-duration)
            :do (progn 
                  (loop ; fill a measure
                        :for note = (first notes)
                        :while (and note (< (note-on-time note) end-time))
                        :do (span-append-node measure (make-note (pop notes))))
                  (setf start-time end-time)
                  (when (and next-tempo-change (<= next-tempo-change start-time))
                    (setf current-measure-duration (cdr (pop measure-durations)))))))
    measures))

;; (let ((measures (make-empty-span)))
;;   (span-append-node measures (make-instance 'measure :number 1 :start-time 0 :duration 1))
;;   (span-append-node measures (make-instance 'measure :number 2 :start-time 1 :duration 1))
;;   (span-contents measures))



;; (create-tempos-and-measures-for-notes '((10.0 . 2.0))
;;                                       (list (make-instance 'midi-note
;;                                                 :key 64 :note-on-time 8.5 :note-off-time 15.7))
;;                                       15.7)
;; ((tempo :measure-duration 2.0
;;         :measures (#4=(measure :number 4 :sounds nil "#x302004FBDCCD")
;;                       #3=(measure :number 3 :sounds nil "#x302004FBDD4D")
;;                       #2=(measure :number 2 :sounds nil "#x302004FBDDCD")
;;                       #1=(measure :number 1 :sounds ((note :start-time 8.5 :duration 7.2 :dynamic 64 :pitch 64 "#x302004FBDE3D")) "#x302004FBDECD")) "#x302004FBDF3D"))
;; (#1# #2# #3# #4#)





(defmethod append-midi-sequence ((partition partition) (sequence abnotation-sequence))
  (let* ((measure-durations (sort (queue-elements (measures sequence))
                                  (function <)
                                  :key (function car)))
         (notes             (sort (queue-elements (notes sequence))
                                  (function <)
                                  :key (function note-on-time)))
         (last-note         (car (last notes)))
         (total-duration    (coerce (note-off-time last-note) 'double-float))
         (page (tail partition))
         (line (tail page)))
    (forward-slurp-span (join-spans line (create-measures-for-notes measure-durations notes total-duration)))
    (extract-node (next (tail line)))
    partition))



;; (defparameter *p*  (create-partition *staves/bass15mb-trebble15ma*))
;; (append-midi-sequence *p* (abnotation-read-midi-file  #P"~/works/abnotation/abnotation/files/1-canal.mid"))
;; 
;;  (span-contents (tail (tail *p*)))


;; (abnotation-read-midi-file  #P"~/works/abnotation/abnotation/files/1-canal.mid")
;; (test/abnotation-read-midi-file  #P"~/works/abnotation/abnotation/files/1-canal.mid")



;;;; THE END ;;;;
