(in-package :apex-utilities)

(defparameter *this-year* (make-instance 'apex-model:academic-year 
					 :name "2008-2009"))
(defparameter *A22* (make-instance 'apex-model:organization
				   :name "A22 Building"))

(defparameter *student-1* (make-instance 'apex-model:student :remark ""
					 :name "PERIER_Romain"
					 :date-of-birth 0
					 :gender :male
					 :id 601051))
(defparameter *student-2* (make-instance 'apex-model:student :remark ""
					 :name "QUINTAR_Fabien"
					 :date-of-birth 0
					 :gender :male
					 :id 601052))

(defparameter *UE-type* (make-instance 'apex-model:teaching-type 
				       :name "Projet de Programmation 3"))

(defparameter *pp3-charger* (make-instance 'apex-model:person :remark ""
					   :name "DURAND_Irène"
					   :date-of-birth 0
					   :gender :female))
(defparameter *pp3-supplier* (make-instance 'apex-model:organization :name "Informatique"))
(defparameter *dept-license* (make-instance 'apex-model:organization 
					    :name "Département Licence"))


(defparameter *pp3-course-inst* (make-instance 'apex-model:course-instance
					      :person-in-charge *pp3-charger*
					      :remark ""
					      :course nil
					      :academic-year *this-year*
					      :parity-semester 2
					      :parts nil))
(defparameter *pp3-course-part* (make-instance 'apex-model:course-part
					       :remark ""
					       :course-instance *pp3-course-inst*
					       :teaching-type *UE-type*
					       :cost 8
					       :assignments nil))
(defparameter *pp3-assign* (make-instance 'apex-model:assignment
					  :person-in-charge *pp3-charger*
					  :remark ""
					  :course-part *pp3-course-part*
					  :amount 1))
;;(setf (apex-model:assignments *pp3-course-part*) (list *pp3-assign*))
(defparameter *pp3-course* (make-instance 'apex-model:course 
					  :person-in-charge *pp3-charger*
					  :remark ""
					  :name "PP3"
					  :code "INF356"
					  :supplier *pp3-supplier*
					  :credits 3
					  :description ""
					  :instances (list *pp3-course-inst*)))
(defparameter *program* (make-instance 'apex-model:teaching-program
				       :name "LST_Informatique"
				       :organized-by *dept-license*
				       :part-of nil))
(defparameter *batch-1* (make-instance 'apex-model:batch
				       :teaching-program *program*
				       :relative-semester 5
				       :academic-year *this-year*
				       :parity-semester 2
				       :size 2
				       :revenue-per-student 13))
					  
(defparameter *buf* (make-instance 'apex-model:buffer :name "student"
				   :academic-years (list *this-year*)
				   :organizations  (list *A22*)
				   :teaching-types (list *UE-type*)
				   :people (list *student-1* *student-2*)
				   :teaching-programs (list *program*)
				   :courses (list *pp3-course*)
				   :batches (list *batch-1*)))

(defun genbase (stream)
  (setf apex-model:*print-for-file-io* t)
  (format stream "~A~%" (car (car apex-model:*readtables*)))
  (apex-model:print-object *buf* stream)
  (setf apex-model:*print-for-file-io* nil))
