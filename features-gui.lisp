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