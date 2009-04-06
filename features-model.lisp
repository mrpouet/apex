(in-package :apex-model)

;; classe student
;; un étudiant est une personne ayant un numéros étudiant
;; pouvant être enseignant ou pas (au même titre qu'une person)
;; avec un status d'étudiant durant une période à durée déterminée
;; (génèralement)
;; 

(defclass student (person)
  ((%id :initarg :id :reader id)
   (%registered-years :initarg :registered-years :initform '() :accessor registered-years)
   (%notes :initarg :notes :initform '() :accessor notes)))

(define-save-info student
   (:id id)
   (:registered-years registered-years)
   (:notes notes))
