(in-package :apex-gui)

(defclass registration-set-view (person-set-view)
  ((%set-view :initarg :set-view :reader set-view)))

(defun find-multiple-symbol (item sequence &key (key (lambda(e) e)) (test #'=))
  (let (l '())
    (dolist (e sequence)
      (if (funcall test item (funcall key e))
	  (push e l)))
    l))

(defmethod display-pane-view (frame pane buffer (view registration-set-view))
  (formatting-table (pane)
    (loop for reg in (set-view view)
       do (with-output-as-presentation (pane reg 'registration)
	    (formatting-row (pane)
	      (formatting-cell (pane)
		(format pane "~a " (pretty-name (person reg))))
	      (formatting-cell (pane)
		(format pane "~2D~%" (id reg))))))))
					     

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
    ((student-name 'string :prompt "Nom Etudiant"))
  (let ((registrations (find-multiple-symbol student-name (registrations (current-buffer)) 
					     :key (lambda(r) (name (person r)))
					     :test #'string=)))
    (if (> (length registrations) 1)
	(new-view (make-instance 'registration-set-view
				 :buffer (current-buffer)
				 :set-view registrations)))))
       
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

(define-command (com-reg-view :name "Voir Inscription" :command-table global-apex-table)
    ((registration 'registration :prompt "Numéro"))
  (new-view (make-instance 'person-view
			   :buffer (current-buffer)
			   :person (person registration))))
  

(define-presentation-to-command-translator view-registration
    (registration com-reg-view global-apex-table)
  (object)
  `(,object))
