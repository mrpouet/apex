(in-package :apex-gui)

;; Toute classe de la forme "object-view" doit hériter
;; de la classe apex-view qui hérite elle même de la class view.
;; Sinon l'affichage par un display-pane-view est impossible.

;; Visualiser un étudiant et son inscription, 
;; depuis la liste des étudiant obtenue
;; par "Voir Etudiant" lorsque deux étudiants portent le même nom.
(defclass registration-set-view (person-set-view)
  ((%set-view :initarg :set-view :reader set-view)))

(defclass student-set-view (person-set-view) ())

(defclass year-view (apex-view)
  ((%buffer :initarg :buffer :accessor buffer)
   (%year   :initarg :year   :reader   year)))



(defun find-multiple-symbol (item sequence &key (key (lambda(e) e)) (test #'=))
  (let (l '())
    (dolist (e sequence)
      (if (funcall test item (funcall key e))
	  (push e l)))
    l))

(defun compute-examination (alist symbol)
  (let ((entry (assoc symbol alist :test #'string=))
	(sum 0)
	(count 0))
    (if (not entry)
	'Non
	(progn
	  (dolist (note (cdr entry))
	    (setf sum (+ sum note))
	    (setf count (1+ count)))
	  (/ sum count)))))

(defun compute-cc (DS TP)
  (if (eq DS 'Non)
      TP
      (if (eq TP 'Non)
	  DS
	  (/ (+ DS TP) 2))))
	  
(defmethod display-pane-view (frame pane buffer (view year-view))
  (let* ((person       (person (car (views *application-frame*))))
	 (registration (studentp person buffer)))
    (formatting-table (pane)
      (with-drawing-options (pane :ink +red+)
	(format pane "Année: ~A~%~%" (name (academic-year (year view))))
	(format pane "Semestre 1 -> ~A~%" (name (first-semester (year view))))
	(format pane "Semestre 2 -> ~A~%" (name (second-semester (year view))))))))

(defmethod display-pane-view (frame pane buffer (view registration-set-view))
  (formatting-table (pane)
    (loop for reg in (set-view view)
       do (with-output-as-presentation (pane reg 'registration)
	    (formatting-row (pane)
	      (formatting-cell (pane)
		(format pane "~a " (pretty-name (person reg))))
	      (formatting-cell (pane)
		(format pane "~2D~%" (id reg))))))))

(defmethod display-pane-view (frame pane buffer (view student-set-view))
  (formatting-table (pane)
    (dolist (reg (registrations buffer))
      (let ((person (person reg)))
        (with-output-as-presentation (pane person 'person)
	  (formatting-row (pane)
	    (formatting-cell (pane)
              (format pane "~a" (pretty-name person)))))))))
	


(defmethod display-pane-view :before (frame pane buffer (view person-view))
  (let ((registration (studentp (person view) buffer)))
    (if registration
	(progn
	  (format pane "Etudiant n°: ~2D~%~%" (id registration))
	  (formatting-table (pane)
            (with-drawing-options (pane :ink +red+)
	      (formatting-row (pane)
		(formatting-cell (pane)
	          (format pane "Année(s)"))
		(formatting-cell (pane)
		  (format pane "Semestre 1"))
		(formatting-cell (pane)
		  (format pane "Semestre 2"))))
	    (dolist (year (registered-years registration))
	      (with-output-as-presentation (pane year 'year)
		(formatting-row (pane)
		  (formatting-cell (pane)
		    (format pane "~A" (name (academic-year year))))
		  (formatting-cell (pane)
		    (format pane "~A" (name (first-semester year))))
		  (formatting-cell (pane)
		    (format pane "~A" (name (second-semester year))))))))))))
			


(define-command (com-student-view :name "Voir Etudiant" :command-table global-apex-table)
    ((student-name 'string :prompt "Nom Etudiant"))
  (let ((registrations (find-multiple-symbol student-name (registrations (current-buffer)) 
					     :key (lambda(r) (name (person r)))
					     :test #'string=)))
    (if (> (length registrations) 1)
	(new-view (make-instance 'registration-set-view
				 :buffer (current-buffer)
				 :set-view registrations))
	(new-view (make-instance 'person-view
				 :buffer (current-buffer)
				 :person (person (car registrations)))))))

(define-command (com-view-set-student :name "Voir Ensemble Etudiant" :command-table global-apex-table)
    ()
  (new-view (make-instance 'student-set-view
			   :buffer (current-buffer))))
       
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
    ((id      'integer        :prompt "N° Etudiant")
     (year    'academic-year  :prompt "Année")
     (epreuve 'string         :prompt "Semestre")
     (ue      'string         :prompt "Code UE")
     (type    'string         :prompt "Type (DS/TP/Exam)")
     (note    'float          :prompt "Note"))
  (let ((registration (get-registration-with-id id (current-buffer)))
	)))
	  
;; Commande privée
(define-command (com-reg-view :name "Voir Inscription" :command-table global-apex-table)
    ((registration 'registration :prompt "Numéro"))
  (new-view (make-instance 'person-view
			   :buffer (current-buffer)
			   :person (person registration))))

(define-command (com-student-register :name "Inscrire Etudiant" :command-table global-apex-table)
    ((id        'integer         :prompt "N° Etudiant")
     (year      'academic-year   :prompt "Année Acadèmique")
     (semester1 'string          :prompt "Semestre 1")
     (semester2 'string          :prompt "Semestre 2"))
  (let ((registration  (get-registration-with-id id (current-buffer)))
	(sem1 (make-instance 'semester :name semester1))
	(sem2 (make-instance 'semester :name semester2)))
    (push (make-instance 'year
			 :academic-year   year
			 :first-semester  sem1
			 :second-semester sem2) (registered-years registration))))
;; Commande privée    
(define-command (com-view-year :name "Voir Semestres Etudiant" :command-table global-apex-table)
    ((year 'year   :prompt "Année"))
  (new-view (make-instance 'year-view
			   :buffer (current-buffer)
			   :year year))) 

(define-presentation-to-command-translator view-registration
    (registration com-reg-view global-apex-table)
  (object)
  `(,object))

(define-presentation-to-command-translator view-year
    (year com-view-year global-apex-table)
  (object)
  `(,object))
