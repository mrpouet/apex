(in-package :apex-model) 


(define-condition no-such-student (apex-condition) ()
		  (:report
		   (lambda (condition stream)
		     (declare (ignore condition))
		     (format stream "No such student with the given id"))))

(define-condition no-such-course (apex-condition) ()
		  (:report
		   (lambda (condition stream)
		     (declare (ignore condition))
		     (format stream "No such course with the given code"))))

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
    (if (not registration)
	(error 'no-such-student))
    registration))

(defmethod valid-course (code (buffer buffer-extended))
  (let ((course (find code (courses buffer) :key (lambda(c) (code c))
		      :test #'string=)))
    (if (not course)
	(error 'no-such-course))
    course))

(defmethod get-credits-from-course (code (buffer buffer-extended))
  (credits (valid-course code buffer)))

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
   (%first-semester-notes :initarg :first-semester-notes :initform '() 
			  :accessor first-semester-notes)
   (%second-semester-notes :initarg :second-semester-notes :initform '()
			   :accessor second-semester-notes)))

(defmethod push-note-s1 (type ue note (y year))
  (if (not (assoc ue (first-semester-notes y) :test #'string=))
      (push (cons ue nil) (first-semester-notes y)))
  (let ((p (assoc ue (first-semester-notes y) :test #'string=)))
    (if (not (assoc type (cdr p) :test #'string=))
	(push (cons type nil) (cdr p)))
    (push note (cdr (assoc type (cdr p) :test #'string=)))))

(defmethod push-note-s2 (type ue note (y year))
  (if (not (assoc ue (second-semester-notes y) :test #'string=))
      (push (cons ue nil) (second-semester-notes y)))
  (let ((p (assoc ue (second-semester-notes y) :test #'string=)))
    (if (not (assoc type (cdr p) :test #'string=))
	(push (cons type nil) (cdr p)))
    (push note (cdr (assoc type (cdr p) :test #'string=)))))


(define-save-info year
   (:academic-year academic-year)
   (:first-semester first-semester)
   (:second-semester second-semester)
   (:first-semester-notes first-semester-notes)
   (:second-semester-notes second-semester-notes))
   

;; Un objet "note" contient la liste des notes d'un étudiant,
;; notes propres à un semestre d'une année donnée :
;; @year : L'année académique de type "year"
;; @*-semester-notes : Liste associative des notes pour le semestre correspondant,
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
  ((%first-semester-notes  :initform '() :initarg :first-semester-notes 
			   :accessor first-semester-notes)
   (%second-semester-notes :initform '() :initarg :second-semester-notes 
			   :accessor second-semester-notes)))

(define-save-info note
  (:first-semester-notes  first-semester-notes)
  (:second-semester-notes second-semester-notes))
