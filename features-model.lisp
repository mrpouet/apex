(in-package :apex-model) 

;; Sous classe d'un buffer apex
;; @registrations: Liste des inscriptions administratives
(defclass buffer-extended (buffer)
  ((%registrations :initform '() :initarg :registrations 
		   :accessor registrations)))

;; "Prédicat" retournant un objet registration si la person p est enregistré
;; (donc T) , NIL sinon.
(defmethod studentp ((p person) (buffer buffer-extended))
  (let ((registrations (registrations buffer)))
    (find p registrations :key (lambda (r) (person r)) 
	  :test #'eq)))

;; Retourne l'objet registration associé au Numéro d'étudiant "student-id"
(defmethod get-registration-with-id (student-id (buffer buffer-extended))
  (let ((registration (find student-id (registrations buffer)
			    :key (lambda(r) (id r)))))
    (assert (not (eq registration nil)))
    registration))

(define-save-info buffer-extended
    (:registrations registrations))

;; Inscription administrative pour un étudiant :
;; @person : personne à laquelle cette inscription fait réfèrence,
;; ici il s'agit de la personne qui est étudiante
;; @id : Numéro d'étudiant de cette personne (unique !)
;; @registered-years: Liste d'objet "year" contenant les années qui suit
;; ou qu'il a suivit, avec pour chacune de ces années les semestres.
(defclass registration (apex-object)
  ((%person :initarg :person :accessor person)
   (%id     :initarg :id     :reader   id)
   (%registered-years :initform '() :initarg :registered-years 
		      :accessor registered-years)
   (%notes :initform '() :initarg :notes :accessor notes)))

(define-save-info registration
  (:person person)
  (:id id)
  (:registered-years registered-years)
  (:notes notes))

;; Un semestre
(defclass semester (apex-object name-mixin) ())

;; Année administrative pour un étudiant :
;; @academic-year   : année academique
;; @first-semester  : semestre 1
;; @second-semester : semestre 2
(defclass year (apex-object)
  ((%academic-year   :initarg :academic-year   :reader academic-year)
   (%first-semester  :initarg :first-semester  :reader first-semester)
   (%second-semester :initarg :second-semester :reader second-semester)
   (%notes           :initarg :initform '() :notes :accessor notes)))

(define-save-info year
   (:academic-year academic-year)
   (:first-semester first-semester)
   (:second-semester second-semester)
   (:notes :accessor notes))

;; Un objet "note" contient la liste des notes d'un étudiant,
;; notes propres à un semestre d'une année donnée :
;; @year : L'année académique de type "year"
;; @*-semester-notes : Liste associative des notes,
;; les clés sont les noms des UE, les valeurs sont des listes associative d'épreuves.
;; Une épreuve est tout simplement soit un DS, soit un TP, soit un Exam,
;; la valeur d'une pair contenant une épreuve (son cdr) est simplement la liste des notes.
;; exemple:
;;
;;  #1 -> (("DS" . #2#) ("TP" . #3#))
;;  #2 -> (14.5 12.0)
;;  #3 -> (12.0 9.0)
;;  @notes -> (("INF356" . #1#))
;;
(defclass note (apex-object)
  ((%first-semester-notes  :initarg :first-semester-notes  :initform '() 
			   :accessor first-semester-notes) 
   (%second-semester-notes :initarg :second-semester-notes :initform '()
			   :accessor second-semester-notes)))

(define-save-info note
  (:first-semester-notes  first-semester-notes)
  (:second-semester-notes second-semester-notes))
 