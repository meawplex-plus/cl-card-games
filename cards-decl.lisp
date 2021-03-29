
					; Poker - cards and decks

;; Define global variables, and shuffle cards

(defparameter *start-cards*
  (loop for rank across #("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "K" "Q")
	append (loop for suit across #("Clubs" "Spades" "Diamonds" "Hearts")
		     collect (list rank suit))))

(alexandria:shuffle *start-cards*)

(defparameter *card-instances*
  (loop for (rank suit) in *start-cards*
	collect (make-instance 'card :rank rank :suit suit))) ;;; <== Defined in the classes section

;; Special card error handler!

(define-condition invalid-property (error)
  ((rank :initarg :rank :reader y)
   (suit :initarg :suit :reader x))
  (:report (lambda (condition stream) ;;; <== CONDITION required for some reason
	     (format stream "Invalid card property/properties in :rank or :suit initargs:~&~
:rank => ~s~&~
:suit => ~s~&~
Fix the issue to continue.~&~
Property requirements:~&~
:rank should be one of ~s;~&~
:suit should be one of ~s."
		     (y condition) ;;; <== This might be confusing, but this is just the syntax of DEFINE-CONDITION.
		     (x condition) ;;; <== Same.
		     '("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "K" "Q")
		     '("Clubs" "Spades" "Diamonds" "Hearts")))))

;; Classes

(defclass deck ()
  ((size :initform 52 :initarg :deck-size :reader :deck-size)
   (deck-type :initform "Poker" :initarg :deck-type)
   (cards-assocs :initform *start-cards* :initarg :cards-list)))

(defclass card ()
  ((suit :initarg :suit :initform (error "Supply a suit."))
   (rank :initarg :rank :initform (error "Supply a rank."))
   (card-type)))

;; All methods

(defmethod initialize-instance :after ((card card) &key)
  (card-check card)
  (if (member card '("J" "K" "Q"))
      (setf (slot-value card 'card-type) 'face-card)
      (setf (slot-value card 'card-type) 'normal-card)))

(defmethod card-check ((card card))
  (if (or
       (not (member (slot-value card 'rank)
		    '("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "K" "Q") :test 'equal)) ;;; <== :test 'equal required for strings!
       (not (member (slot-value card 'suit)
		    '("Clubs" "Spades" "Diamonds" "Hearts") :test 'equal))) ;;; <== Ditto on this one!
      (error 'invalid-property
	     :rank (slot-value card 'rank)
	     :suit (slot-value card 'suit))))

;; ...But this is unfinished.

(defgeneric deal (hands))

(defmethod deal (hands))
