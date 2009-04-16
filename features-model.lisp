(in-package :apex-model) 

(defclass buffer-extended (buffer)
  ((%registrations :initform '() :initarg :registrations 
		   :accessor registrations)))

(defmethod studentp ((p person) (buffer buffer-extended))
  (let ((registrations (registrations buffer)))
    (find p registrations :key (lambda (r) (person r)) 
	  :test #'eq)))

(defmethod get-registration-with-id (student-id (buffer buffer-extended))
  (let ((registration (find student-id (registrations buffer)
			    :key (lambda(r) (id r)))))
    (assert (not (eq registration nil)))
    registration))

    
    

(define-save-info buffer-extended
    (:registrations registrations))

(defclass registration (apex-object)
  ((%person :initarg :person :accessor person)
   (%id     :initarg :id     :reader   id)
   (%registered-years :initform '() :initarg :registered-years 
		      :accessor registered-years)
   (%notes  :initform nil :initarg :notes :accessor  notes)))

(define-save-info registration
  (:person person)
  (:id id)
  (:registered-years registered-years))