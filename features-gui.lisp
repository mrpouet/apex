(in-package :apex-gui)

(defmethod display-pane-view :before (frame pane buffer (view person-view))
  (let ((registration (studentp (person view) buffer)))
    (if registration
	(progn
	  (format pane "Etudiant n°: ~2D~%" (id registration))
	  (format pane "Année(s) scolarisée(s): ")
	  (dolist (e (registered-years registration))
		  (format pane (name e)))
	  (format pane "~%")))))

(define-command (com-student-view :name "Voir Etudiant" :command-table global-apex-table)
    ((student-id 'integer :prompt "N° Etudiant"))
  (let ((registration (find student-id (registrations (current-buffer)) 
			    :key (lambda(r) (id r)))))    
    (assert (not (eq registration nil)))
    (new-view (make-instance 'person-view
			     :buffer (current-buffer)
			     :person (person registration)))))

(define-command (com-genbase :name "Sauvegarder Base" :command-table global-apex-table)
    ((filename 'string :prompt "Nom fichier"))
  (with-open-file (stream filename :direction :output :if-does-not-exist :create)
    (setf apex-model:*print-for-file-io* t)
    (format stream "~A~%" (car (car apex-model:*readtables*)))
    (apex-model:print-object (current-buffer) stream)
    (setf apex-model:*print-for-file-io* nil)))

(define-command (com-add-student :name "Ajouter Etudiant" :command-table global-apex-table)
    ((name   'string  :prompt "Nom")
     (gender 'gender  :prompt "Sexe")
     (id     'integer :prompt "N° Etudiant"))
  (let ((person (make-instance 'apex-model:person 
		       :name   name
		       :gender gender
		       :date-of-birth 0)))
  (push person (people (current-buffer)))
  (push (make-instance 'apex-model:registration 
		       :person person
		       :id id) (registrations (current-buffer)))))

