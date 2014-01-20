;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               model.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;     The model of the ABNotation.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-11 <PJB> Created.
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

(defvar *partition* nil
  "The current partition.")

;; (NOTE pitch start-time duration intensity)
;; 
;; 
;; 
;; 
;; 
;; 
;;    brace ---- 1..4 staff




;;                  element 1------------------o annotation
;;                     ^                             ^                             
;;                     |                             |
;;                     |                     +-------+--------+
;;  +------------------|----* tempo 1----+   |                |
;;  |                  |                 | image             text
;;  |  +------------+--+-----+---------+ |
;;  |  |            |        |         | *
;; partition 1--* page 1--* line 1--* measure 1---* sound
;;                           | \                      ^
;;                           *  *                     |
;;                       staff  ledger        +-------+-------+
;;                       |   o    o           |               |
;;                       |   |    |           |               |
;;                     clef  +----========* note *-------o cluster



(defclass element ()
  ())



(defclass graphic-element (element)
  ((box :initarg :box :initform (rect 0 0 0 0) :accessor box :type rect)))

(defgeneric (setf box-size)   (new-size graphic-element)
  (:method (new-size (element graphic-element))
    (setf (rect-size (box element)) new-size)))

(defgeneric (setf box-origin) (new-position graphic-element)
  (:method (new-position (element graphic-element))
    (setf (rect-origin (box element)) new-position)))


(defclass offsetable-element (graphic-element)
  ((offset :initarg :offset :initform (size 0 0) :accessor offset :type size)))


(defclass annotation (offsetable-element)
  ())

(defclass image (annotation)
  ((filename :initarg :filename :accessor filename
             :type string
             :documentation "The filename of the file in the partition
                             package file where the image is saved in.")))

(defclass text (annotation)
  ((rtf :initarg :rtf :accessor rtf :type string :initform ""
        :documentation "RTF string.")))


(define-association annotate
  ((element :type element
            :multiplicity #|1|# 0-1
            :kind :aggregation)
   (annotation :type annotation
               :multiplicity 0-1)))


(defclass sound (element)
  ((start-time :initarg :start-time :accessor start-time)
   (duration  :initarg :duration :accessor duration)
   (dynamic :initarg :dynamic :accessor dynamic :initform :mf)))

(defmethod end-time ((sound sound))
  (+ (start-time sound) (duration sound)))

(defclass note (sound)
  ((pitch :initarg :pitch :accessor pitch)))


(defclass cluster (sound)
  ())

(define-association groups
  ((cluster :type cluster
            :multiplicity #|1|# 0-1
            :kind :aggregation)
   (notes :type note
          :multiplicity 0-*)))


(defclass head (offsetable-element)
  ())

(defclass accidental (offsetable-element)
  ()
  (:documentation "Dieze, bemol ou becare devant la head."))

(define-association note-head
  ((head :type head
         :multiplicity #|1|# 0-1)
   (note :type note
         :multiplicity #|1|# 0-1)))

(define-association note-accidental
  ((accidental :type accidental
               :multiplicity 0-1)
   (note :type note
         :multiplicity #|1|# 0-1)))

;; beam dynamic<> and tenue, and annotation, could span several measures/lines/pages.


(defclass segment (offsetable-element)
  ())

(defclass beam-segment (segment)
  ())
(defclass dynamic-segment (segment)
  ())
(defclass tenue-segment (segment)
  ())

(define-association sound-beams
  ((beam-segments :type beam-segment
                  :multiplicity 0-*
                  :ordered t)
   (sound :type sound
          :multiplicity #|1|# 0-1)))

(define-association sound-dynamics
  ((dynamic-segments :type dynamic-segment
                     :multiplicity 0-*
                     :ordered t)
   (sound :type sound
          :multiplicity #|1|# 0-1)))

(define-association sound-tenues
  ((tenue-segments :type tenue-segment
                   :multiplicity 0-*
                   :ordered t)
   (sound :type sound
          :multiplicity #|1-*|# 0-*))
  (:documentation "When a tenue has several sounds, then it's a ------ tenue.
Otherwise it's a - - - - tenue."))





(defclass numbered ()
  ((number :initarg :number :initform 0 :reader number)
   (number-annotation :initform nil)))


(defparameter *number-annotation-rtf-format*
  "{\\rtf1\\ansi\\ansicpg1252\\cocoartf1187\\cocoasubrtf400
{\\fonttbl\\f0\\froman\\fcharset0 Times-Roman;}
{\\colortbl;\\red255\\green255\\blue255;}
\\paperw11900\\paperh16840\\margl1440\\margr1440\\vieww10800\\viewh8400\\viewkind0
\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardirnatural

\\f0\\b\\fs24 \\cf0 ~D}")

(defparameter *title-annotation-rtf-format*
  "{\\rtf1\\ansi\\ansicpg1252\\cocoartf1187\\cocoasubrtf400
{\\fonttbl\\f0\\froman\\fcharset0 Times-Roman;}
{\\colortbl;\\red255\\green255\\blue255;}
\\paperw11900\\paperh16840\\margl1440\\margr1440\\vieww10800\\viewh8400\\viewkind0
\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardirnatural\\qc

\\f0\\b\\fs28 \\cf0 \\
~A
\\fs24 \\

\\b0 \\
~A\\
}")



(defgeneric number-annotation (element)
  (:method ((element numbered))
           (or (slot-value element 'number-annotation)
               (setf  (slot-value element 'number-annotation)
                      (let ((number-text (make-instance 'text :rtf (format nil *number-annotation-rtf-format*
                                                                           (number element)))))
                        (compute-box-size number-text)
                        number-text)))))

(defgeneric renumber (element)
  (:method :before ((element numbered))
           (setf (slot-value element 'number-annotation) nil)))


(defclass measure (graphic-element numbered)
  ((stat-time      :initarg :start-time     :accessor start-time     :initform 0.0)
   (adjusted-width :initarg :adjusted-width :accessor adjusted-width :initform 0.0 :type coordinate)
   (front-kerning  :initarg :front-kerning  :accessor front-kerning  :initform 0.0 :type coordinate)))



(defmethod end-time ((measure measure))
  (+ (start-time measure) (measure-duration (tempo measure))))

(defmethod time-in-measure-p (time (measure measure))
  (and (<= (start-time measure) time (end-time measure))))

(defun find-measure-containing-time (time measure)
  (cond
    ((< time (start-time measure))
     (let ((previous-measure (previous measure)))
       (and previous-measure (find-measure-containing-time time previous-measure))))
    ((<= (end-time measure) time)
     (let ((next-measure (next measure)))
       (and next-measure (find-measure-containing-time time next-measure))))
    (t measure)))

(defmethod (setf box-size) (new-size (measure measure))
  (let ((width (if (slot-boundp measure 'adjusted-width)
                 (slot-value measure 'adjusted-width)
                 (width new-size)))
        (height (height new-size)))
    (if (slot-boundp measure 'box)
      (setf (extent (slot-value measure 'box)) new-size)
      (setf (slot-value measure 'box) (rect 0 0 width height)))
    new-size))

(define-association measure-contains-sounds
  ((measure :type measure
            :multiplicity #|1|# 0-1)
   (sounds :type sound
           :multiplicity 0-*
           :ordered t))
  (:documentation "A sound can span over several measures.  The head
is on the first one, but the beam, dynamic and tenue can have several
segments, one on each successive measure."))


(define-association measure-contains-segments
    ((measure :type measure
              :multiplicity #|1|# 0-1)
     (segments :type segment
               :multiplicity 0-*)))




(defclass line (offsetable-element numbered)
  ())

(defun lane-height (partition)
  (/ (staff-height partition) 8))

(defmethod (setf box) :after (new-box (line line))
  (let ((y 0)
        (lane-height (lane-height (partition (page line)))))
   (dolist (band (bands line))
     (setf (box band) (rect 0 y (width new-box)
                            (* (- (maximum-lane band) (minimum-lane band) -1)
                               lane-height)))
     (incf y (height (box band))))))

(defmethod (setf box-size) (new-size (line line))
  (if (slot-boundp line 'box)
    (setf (extent (slot-value line 'box)) new-size)
    (setf (box line) (rect 0 0 (width new-size) (height new-size))))
  new-size)

(define-association line-contains-vertically
  ((line :type line
         :multiplicity #|1|# 0-1
         :kind :aggregation)
   (measures :type measure
             :multiplicity 0-*
             :ordered t)))


(defclass band (graphic-element)
  ())

(define-association line-contains-horizontally
  ((line :type line
         :multiplicity #|1|# 0-1
         :kind :aggregation)
   (bands :type band
          :multiplicity #|1-*|# 0-*
          :ordered t)))

(define-association band-contains
  ((band :type band
         :multiplicity #|1|# 0-1)
   (notes :type note
          :multiplicity #|1-*|# 0-*)))


(defun lane (pitch)
  (multiple-value-bind (octave note) (truncate (- pitch 20) 12)
    (+ (aref #(  0   0   1   1   2   2   3   4   4   5   5   6) note)
       (* octave 7))))

;; Those four generic functions have methods for staff, ledger and
;; clef.  The staff methods defer to the (clef staff).

(defgeneric maximum-lane (element) 
  (:documentation "The maximum pitch of a note on the ledger above a staff with the clef."))
(defgeneric top-lane (element)
  (:documentation "The pitch of the top line of a staff with the clef."))
(defgeneric bottom-lane (element)
  (:documentation "The pitch of the bottom line of a staff with the clef."))
(defgeneric minimum-lane (element)
  (:documentation "The minimum pitch of a note on the ledger below a staff with the clef."))


(defclass ledger (band)
  ((minimum-lane :initarg :minimum-lane :reader minimum-lane)
   (bottom-lane  :initarg :bottom-lane  :reader bottom-lane)
   (top-lane     :initarg :top-lane     :reader top-lane)
   (maximum-lane :initarg :maximum-lane :reader maximum-lane))
  (:documentation "The lines between the staves."))

(defclass staff (band)
  ())

(defmethod (setf box) :after (new-box (staff staff))
  (setf (box (clef staff)) (rect (left new-box) (bottom new-box)
                                 10.0 (height new-box))))


(defclass clef (graphic-element)
  ((name :initarg :name :reader name)
   (line :initarg :line :reader line
         :documentation "The line of the staff on which the clef is aligned (1-5, 1=bottom).")
   (pitch :initarg :pitch :reader pitch
          :documentation "The pitch of the clef = the note on (line clef).")
   (minimum-lane :initarg :minimum-lane :reader minimum-lane)
   (bottom-lane  :initarg :bottom-lane  :reader bottom-lane)
   (top-lane     :initarg :top-lane     :reader top-lane)
   (maximum-lane :initarg :maximum-lane :reader maximum-lane)))

(define-association gives-pitch
  ((clef :type clef
         :multiplicity #|1|# 0-1)
   (staff :type staff
          :multiplicity #|1|# 0-1
          :kind :aggregation)))


(defmethod maximum-lane ((element staff)) (maximum-lane (clef element)))
(defmethod top-lane     ((element staff)) (top-lane     (clef element)))
(defmethod bottom-lane  ((element staff)) (bottom-lane  (clef element)))
(defmethod minimum-lane ((element staff)) (minimum-lane (clef element)))


(defclass page (graphic-element numbered)
  ())

(define-association page-contains
  ((page :type page
         :multiplicity #|1|# 0-1
         :kind :aggregation)
   (lines :type line
          :multiplicity 0-*
          :ordered t)))


(defclass partition ()
  ((title                 :initarg :title                 :accessor title
                          :type string                    :initform "untitled")
   (author                :initarg :author                :accessor author
                          :type string                    :initform "anonymous")
   (title-annotation)
   (file                  :initarg :file                  :accessor file
                          :type (or pathname null)        :initform nil)
   (needs-saving          :initarg :needs-saving          :initform nil
                          :type boolean                   :accessor needs-saving)
   (staff-set             :initarg :staff-set             :accessor staff-set)
   (page-number-font      :initarg :page-number-font      :accessor page-number-font
                          :type string)
   (line-number-font      :initarg :line-number-font      :accessor line-number-font
                          :type string)
   (measure-number-font   :initarg :measure-number-font   :accessor measure-number-font
                          :type string)
   (default-measure-speed :initarg :default-measure-speed :accessor default-measure-speed
     :type real :initform 40
     :documentation "The scale of a measure in mm/s.")
   (paper-format          :initarg :paper-format          :accessor paper-format
                          :type string)
   (paper-orientation     :initarg :paper-orientation     :accessor paper-orientation
                          :type (member :portrait :paysage) :initform :portrait)
   (paper-size            :initarg :paper-size            :accessor paper-size
                          :type size
                          :documentation "The (width height) in millimeter of the paper page.")
   (paper-printable-area  :initarg :paper-printable-area  :accessor paper-printable-area
                          :type rect
                          :documentation "The (left bottom width height) in millimeter of the printable area.")
   (staff-height          :initarg :staff-height          :accessor staff-height
                          :type real
                          :documentation "Unit: millimeter, values: 3, 5, 7 mm")
   (interline             :initarg :interline :accessor interline
                          :type real :initform 5.0
                          :documentation "Unit: millimeter, the height between lines on a page.")))

(defparameter *papers*
  '((("A4" :portrait) (210 297) (10 10 190 277))
    (("A4" :paysage)  (297 210) (10 10 277 190))
    (("A3" :portrait) (297 420) (10 10 277 4000))
    (("A3" :paysage)  (420 297) (10 10 400 277))))

(defun paper-size-and-printable-area (format orientation)
  (destructuring-bind ((w h) (px py pw ph)) (cdr (assoc (list format orientation) *papers* :test (function equalp)))
    (values (size w h) (rect px py pw ph))))



(defmethod initialize-instance :after ((partition partition) &rest args &key &allow-other-keys)
  (when args
    (unless (and (slot-boundp partition 'paper-size)
                 (slot-boundp partition 'paper-printable-area))
      (unless (and (slot-boundp partition 'paper-format)
                   (slot-boundp partition 'paper-orientation))
        (error "Either the :paper-format and :paper-orientation must be given, ~
              or :paper-size and :paper-printable-area must be given."))
      (setf (values (slot-value partition 'paper-size)
                    (slot-value partition 'paper-printable-area))
            (paper-size-and-printable-area  (slot-value partition 'paper-format)
                                            (slot-value partition 'paper-orientation)))))
  partition)


(define-association partition-contains
  ((partition :type partition
              :multiplicity #|1|# 0-1
              :kind :aggregation)
   (pages :type page
          :multiplicity 0-*
          :ordered t)))

(defgeneric title-annotation (element)
  (:method ((page page))
           (and (= 1 (number page))
                (partition page)
                (title-annotation (partition page))))
  (:method ((partition partition))
           (if (slot-boundp partition 'title-annotation)
               (slot-value partition 'title-annotation)
               (setf (slot-value partition 'title-annotation)
                     ;; TODO: layout?
                     (let ((title (make-instance 'text
                                      :rtf (format nil *title-annotation-rtf-format*
                                                   (title partition)
                                                   (author partition)))))
                       (attach 'annotate (first (pages partition)) title)
                       (compute-box-size title)
                       title)))))

(defclass tempo (element)
  ((measure-duration :initarg :measure-duration :accessor measure-duration)))

(define-association partition-tempo
  ((partition :type partition
              :multiplicity #|1|# 0-1
              :kind :aggregation)
   (tempos :type tempo
           :multiplicity 0-*
           :ordered t)))

(define-association gives-tempo
  ((tempo :type tempo
          :multiplicity #|1|# 0-1)
   (measures :type measure
             :multiplicity 0-*
             :ordered t)))

(defmethod (setf tempos) :after (new-tempo (partition partition))
  (declare (ignore new-tempo))
  (setf (needs-saving partition) t))


;; TODO: see with ordered associations.

(defgeneric next (element))
(defgeneric previous (element))


(define-association tempo-sequence
  ((previous :type tempo
             :multiplicity 0-1)
   (next     :type tempo
             :multiplicity 0-1))
  (:documentation "The tempos are ordered in a doubly-linked list."))

(define-association measure-sequence
  ((previous :type measure
             :multiplicity 0-1)
   (next     :type measure
             :multiplicity 0-1))
  (:documentation "The measures are ordered in a doubly-linked list."))

(define-association line-sequence
  ((previous :type line
             :multiplicity 0-1)
   (next     :type line
             :multiplicity 0-1))
  (:documentation "The lines are ordered in a doubly-linked list."))

(define-association page-sequence
  ((previous :type page
             :multiplicity 0-1)
   (next     :type page
             :multiplicity 0-1))
  (:documentation "The page are ordered in a doubly-linked list."))

(defmethod next ((sound sound))
  (loop :for sounds :on (sounds (measure sound))
        :until (eql (car sounds) sound)
        :finally (return (or (cadr sounds)
                             (loop
                               :with measure = (next (measure sound))
                               :while (and measure (endp (sounds measure)))
                               :do (setf measure (next measure))
                               :finally (return (and measure (first (sounds measure)))))))))

(defmethod previous ((sound sound))
  (let ((sounds (sounds (measure sound))))
    (if (eql sound (first sounds))
        (loop
          :with measure = (previous (measure sound))
          :while (and measure (endp (sounds measure)))
          :do (setf measure (previous measure))
          :finally (return (and measure (first (sounds measure)))))
        (loop :for sounds :on 
              :until (eql (cadr sounds) sound)
              :finally (return (car sounds))))))




(defgeneric container (element)
  (:method ((nul null))         nil)
  (:method ((segment segment))  (measure segment))
  (:method ((sound sound))      (measure sound))
  (:method ((measure measure))  (line measure))
  (:method ((line line))        (page line))
  (:method ((page page))        (partition page))
  (:method ((tempo tempo))      (partition tempo)))


(defun first-segment-p (segment)
  (let ((measure (measure segment))
        (sound (sound segment)))
    (time-in-measure-p (start-time sound) measure)))

(defun intermediate-segment-p (segment)
  (let ((measure (measure segment))
        (sound (sound segment)))
    (and (not (time-in-measure-p (start-time sound) measure))
         (not (time-in-measure-p (end-time   sound) measure)))))

(defun last-segment-p (segment)
  (let ((measure (measure segment))
        (sound (sound segment)))
    (time-in-measure-p (end-time sound) measure)))


(defun first-element-in-container-p (element)
  (not (eql (container (previous element))
            (container element))))

(defun last-element-in-container-p (element)
  (not (eql (container element)
            (container (next element)))))



(defgeneric head (element)
  (:method (element)
    (loop
      :for current = element :then previous
      :for previous = (previous current)
      :while previous
      :finally (return current))))

(defgeneric tail (element)
  (:method (element)
    (loop
      :for current = element :then next
      :for next = (next current)
      :while next
      :finally (return current))))



;; (define-association annotate
;; (define-association groups
;; (define-association note-head
;; (define-association note-accidental
;; (define-association sound-beams
;; (define-association sound-dynamics
;; (define-association sound-tenues
;; (define-association measure-contains-sounds
;; (define-association measure-contains-segments
;; (define-association line-contains-vertically
;; (define-association line-contains-horizontally
;; (define-association band-contains
;; (define-association gives-pitch
;; (define-association page-contains
;; (define-association partition-contains
;; (define-association partition-tempo
;; (define-association gives-tempo
;;
;; (define-association tempo-sequence
;; (define-association measure-sequence
;; (define-association line-sequence
;; (define-association page-sequence
;; 
;; (:method ((segment segment))  (measure segment))  measure-contains-segments
;; (:method ((sound sound))      (measure sound))    measure-contains-sounds      
;; (:method ((measure measure))  (line measure))     line-contains-vertically     measure-sequence
;; (:method ((line line))        (page line))        page-contains                line-sequence
;; (:method ((page page))        (partition page))   partition-contains           page-sequence
;; (:method ((tempo tempo))      (partition tempo))  gives-tempo                  tempo-sequence


(defmethod did-link ((association (eql 'partition-contains)) (partition partition) (page page))
  (compute-box-size page))

(defmethod did-link ((association (eql 'page-contains)) (page page) (line line))
  (compute-box-size line))



(defmacro define-print-object (class &rest slots)
  `(defmethod print-object ((object ,class) stream)
     (print-parseable-object (object stream :type t :identity t) ,@slots)))

(define-print-object image   box offset filename)
(define-print-object text    box offset rtf)
(define-print-object note    start-time duration dynamic pitch)
(define-print-object cluster start-time duration dynamic notes)
(define-print-object measure box number sounds)
(define-print-object line    box offset number bands measures)
(define-print-object page    box number lines)
(define-print-object ledger  box minimum-lane bottom-lane top-lane maximum-lane)
(define-print-object staff   box clef)
(define-print-object clef    box name line pitch)
(define-print-object tempo   measure-duration measures)
(define-print-object partition
  title author file staff-set pages tempos
  page-number-font line-number-font measure-number-font
  paper-format paper-orientation
  paper-size paper-printable-area
  staff-height interline)


;; (loop for i from 20 to 120
;;       do
;;       (when (zerop (mod (- i 20) 12)) (terpri))
;;       (format t " ~3D" i))
;; 
;; (mod (- i 20) 12)
;; #(A A B B C C D E E F F G)
;; ----------------------------------------------------
;;                            #                   #
;;        b       b                   b
;;       A   A   B   B   C   C   D   E   E   F   F   G
;; ----------------------------------------------------
;;       Ab  A   Bb  B   C   C#  D   Eb  E   F   F#  G
;; ----------------------------------------------------
;; 0     20  21  22  23  24  25  26  27  28  29  30  31
;; 1     32  33  34  35  36  37  38  39  40  41  42  43
;; 2     44  45  46  47  48  49  50  51  52  53  54  55
;; 3     56  57  58  59  60  61  62  63  64  65  66  67
;; 4     68  69  70  71  72  73  74  75  76  77  78  79
;; 5     80  81  82  83  84  85  86  87  88  89  90  91
;; 6     92  93  94  95  96  97  98  99 100 101 102 103
;; 7    104 105 106 107 108 109 110 111 112 113 114 115
;; 8    116 117 118 119 120 121 122
;; ----------------------------------------------------
;;    #(  0   0   1   1   2   2   3   4   4   5   5   6)


;; (loop for i from 20 to 122
;;       do
;;       (when (zerop (mod (- i 20) 12)) (terpri))
;;       (multiple-value-bind (lane octave) (lane i)
;;         (format t " ~3D:~2,'0D" i (+ (* octave 7) lane))))
;; 
;; 
;;   20:00  21:00  22:01  23:01  24:02  25:02  26:03  27:04  28:04  29:05  30:05  31:06
;;   32:07  33:07  34:08  35:08  36:09  37:09  38:10  39:11  40:11  41:12  42:12  43:13
;;   44:14  45:14  46:15  47:15  48:16  49:16  50:17  51:18  52:18  53:19  54:19  55:20
;;   56:21  57:21  58:22  59:22  60:23  61:23  62:24  63:25  64:25  65:26  66:26  67:27
;;   68:28  69:28  70:29  71:29  72:30  73:30  74:31  75:32  76:32  77:33  78:33  79:34
;;   80:35  81:35  82:36  83:36  84:37  85:37  86:38  87:39  88:39  89:40  90:40  91:41
;;   92:42  93:42  94:43  95:43  96:44  97:44  98:45  99:46 100:46 101:47 102:47 103:48
;;  104:49 105:49 106:50 107:50 108:51 109:51 110:52 111:53 112:53 113:54 114:54 115:55
;;  116:56 117:56 118:57 119:57 120:58 121:58 122:59


(defun create-bands (staff-set)
  (let ((clefs (list (make-instance 'clef :name :bass15mb   :line 4 :pitch  41
                                    :minimum-lane 5 :bottom-lane 6
                                    :maximum-lane 15 :top-lane 14)
                     (make-instance 'clef :name :bass       :line 4 :pitch  65
                                    :minimum-lane 19  :bottom-lane 20
                                    :maximum-lane 29  :top-lane 28)
                     (make-instance 'clef :name :treble     :line 2 :pitch  79
                                    :minimum-lane 31  :bottom-lane 32
                                    :maximum-lane 41 :top-lane 40)
                     (make-instance 'clef :name :treble15ma :line 2 :pitch 103
                                    :minimum-lane 45  :bottom-lane 46
                                    :maximum-lane 55 :top-lane 54))))
    (subseq (list (make-instance 'ledger
                      :minimum-lane 1 :bottom-lane 2
                      :maximum-lane 4 :top-lane 4)
                  (let ((staff (make-instance 'staff)))
                    (attach 'gives-pitch (pop clefs) staff)
                    staff)
                  (make-instance 'ledger
                      :minimum-lane 16  :bottom-lane 16
                      :maximum-lane 18  :top-lane 18)
                  (let ((staff (make-instance 'staff)))
                    (attach 'gives-pitch (pop clefs) staff)
                    staff)
                  (make-instance 'ledger
                      :minimum-lane 30  :bottom-lane 30
                      :maximum-lane 30 :top-lane 30)
                  (let ((staff (make-instance 'staff)))
                    (attach 'gives-pitch (pop clefs) staff)
                    staff)
                  (make-instance 'ledger
                      :minimum-lane 42  :bottom-lane 42
                      :maximum-lane 44 :top-lane 44)
                  (let ((staff (make-instance 'staff)))
                    (attach 'gives-pitch (pop clefs) staff)
                    staff)
                  (make-instance 'ledger
                      :minimum-lane 56 :bottom-lane 46
                      :maximum-lane 59 :top-lane 58))
            (first staff-set) (second staff-set))))


(defun create-partition (staff-set &key (title "untitled") (author "anonymous")
                                   (title-number-font "Helvetica-16")
                                   (page-number-font "Helvetica-12")
                                   (line-number-font "Helvetica-10")
                                   (measure-number-font "Helvetica-8")
                                   (paper-format "A4")
                                   (paper-orientation :portrait)
                                   (staff-height 5))
  (let* ((partition  (make-instance 'partition
                         :title title
                         :author author
                         :staff-set staff-set
                         :title-number-font title-number-font
                         :page-number-font page-number-font
                         :line-number-font line-number-font
                         :measure-number-font measure-number-font
                         :paper-format paper-format
                         :paper-orientation paper-orientation
                         :staff-height staff-height))
         (*partition* partition)
         (tempo      (make-instance 'tempo :measure-duration 1))
         (page       (make-instance 'page :number 1))
         (line       (make-instance 'line :number 1))
         (measure    (make-instance 'measure :number 1)))
    ;; (attach 'measure-contains measure sound)
    ;; (attach 'band-contains band note)
    (dolist (band (create-bands staff-set))
      (attach 'line-contains-horizontally line band))
    (attach 'partition-contains partition page)
    (attach 'page-contains page line)
    (attach 'partition-tempo partition tempo)
    (attach 'gives-tempo tempo measure)
    (attach 'line-contains-vertically line measure)
    (let* ((measure-speed (default-measure-speed partition))
           (duration (measure-duration tempo))
           (width (* measure-speed duration))
           (height (* 58/8 (staff-height partition))))
      (setf (box-size measure) (size width height))
      (setf (box-size line) (size (width (box page)) height)))
    (layout-partition-from-tempos partition)
    partition))


(defparameter *staves/bass*                 '(2 5))
(defparameter *staves/trebble*              '(4 7))
(defparameter *staves/bass-trebble*         '(2 7))
(defparameter *staves/bass-trebble15ma*     '(2 nil))
(defparameter *staves/bass15mb-trebble*     '(0 7))
(defparameter *staves/bass15mb-trebble15ma* '(0 nil))


(defclass cursor ()
  ((partition :initarg :partition :initform nil :accessor partition)
   (page      :initarg :page      :initform nil :accessor page)
   (line      :initarg :line      :initform nil :accessor line)
   (measure   :initarg :measure   :initform nil :accessor measure)
   (sound     :initarg :sound     :initform nil :accessor sound)
   (item      :initarg :item      :initform nil :accessor item)))

(defmethod initialize-instance :after ((cursor cursor) &key &allow-other-keys)
  (when (item    cursor) (setf (sound     cursor) (sound     (item    cursor))))
  (when (sound   cursor) (setf (measure   cursor) (measure   (sound   cursor))))
  (when (measure cursor) (setf (line      cursor) (line      (measure cursor))))
  (when (line    cursor) (setf (page      cursor) (page      (line    cursor))))
  (if (page    cursor)
      (setf (partition cursor) (partition (page    cursor)))
      (if (partition cursor)
          (setf (page    cursor) (first (pages    (partition cursor)))
                (line    cursor) (first (lines    (page      cursor)))
                (measure cursor) (first (measures (line      cursor)))
                (sound   cursor) (first (sounds   (measure   cursor)))))))


(defmethod forward-sound ((cursor cursor))
  (if (sound cursor)
      ()
      )
  )





(defmethod add-new-page ((partition partition))
  )

(defmethod add-new-line ((page page))
  )

(defmethod add-new-measure ((line line))
  )



(defmethod insert-sound ((sound sound) (measure measure))
  )

(defmethod add-note ((note note) (sound sound))
  )



(defmethod insert-new-page-before ((page page))
  )

(defmethod insert-new-line-before ((line line))
  )

(defmethod insert-new-measure-before ((measure measure))
  )




;;;; THE END ;;;;
