(in-package :apex-model) 

(defclass buffer-extended (buffer)
  ((%registrations :initform '() :initarg :registrations 
		   :accessor registrations)))

(defmethod studentp ((p person) (buffer buffer-extended))
  (let ((registrations (registrations buffer)))
    (find (name p) registrations :key (lambda (r) (name (person r))) 
	  :test #'string=)))
    
    

(define-save-info buffer-extended
    (:registrations registrations))

(defclass registration (apex-object)
  ((%person :initarg :person :accessor person)
   (%id     :initarg :id     :reader   id)
   (%registered-years :initform '() :initarg :registered-years 
		      :accessor registered-years)))

(define-save-info registration
  (:person person)
  (:id id)
  (:registered-years registered-years))