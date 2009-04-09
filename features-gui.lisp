(in-package :apex-gui)

(defmethod display-pane-view :before (frame pane buffer (view person-view))
  (let ((registration (studentp (person view) buffer)))
    (if registration
	(progn
	  (format pane "Etudiant n°: ~2D~%" (id registration))
	  (format pane "Année(s) scolarisée(s): ")
	  (dolist (e (registered-years registration))
		  (format pane (name e)))
	  (format pane "~%")
	  (let ((notes (notes registration)))
	    (if notes
		(progn
		  (format pane "Notes:~%")
		  (loop for k being the hash-key of notes
		       do(format pane "~A: " k)
		       do(dolist (e (gethash k notes))
			   (format pane "~2D " e))
		       do(format pane "~%")))))))))

(define-command (com-student-view :name "Voir Etudiant" :command-table global-apex-table)
    ((student-id 'integer :prompt "N° Etudiant"))
  (let ((registration (get-registration-with-id student-id (current-buffer))))
    (new-view (make-instance 'person-view
			     :buffer (current-buffer)
			     :person (person registration)))))

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

(define-command (com-add-student-note :name "Ajouter Note" :command-table global-apex-table)
    ((id   'integer :prompt "N° Etudiant")
     (ue   'string  :prompt "Code UE")
     (note 'integer :prompt "Note"))
  (let* ((registration (get-registration-with-id id (current-buffer))))
    (if (not (notes registration))
	(setf (notes registration) (make-hash-table :test #'equal)))
    (push note (gethash ue (notes registration))))) 
    