(in-package :apex-gui)

;; Toute classe de la forme "object-view" doit hériter
;; de la classe apex-view qui hérite elle même de la class view.
;; Sinon l'affichage par un display-pane-view est impossible.

;; Visualiser un étudiant et son inscription, 
;; depuis la liste des étudiant obtenue
;; par "Voir Etudiant" lorsque deux étudiants portent le même nom.
(defclass registration-set-view (person-set-view)
  ((%set-view :initarg :set-view :reader set-view)))

;; Utilisé pour le "filtre" de la vue de tout les étudiants
;; de la base Apex.
(defclass student-set-view (person-set-view) ())

;; Vue d'une année administrative
(defclass year-view (apex-view)
  ((%buffer :initarg :buffer :accessor buffer)
   (%year   :initarg :year   :reader   year)))


;; Retourne une liste de toute les occurences de @item trouvés dans @sequence
;; utilisée par la commande Voir Etudiant pour gérés les multiples occurences
;; de plusieurs étudiant portant le même nom.
(defun find-multiple-symbol (item sequence &key (key (lambda(e) e)) (test #'=))
  (let (l '())
    (dolist (e sequence)
      (if (funcall test item (funcall key e))
	  (push e l)))
    l))

;; Calcule Note totale par "type" (TP,DS ou Exam)
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

;; Fonction "auxilière"
(defun compute-exam (alist symbol)
  (let* ((entry (assoc "Exam" alist :test #'string=))
	 (notes (cdr entry)))
    (if (not notes)
	'Non
	(progn
	  (if (string= symbol "S1")
	      (progn
		(if (= (length notes) 2)
		    (second notes)
		    (first  notes)))
	      (progn
		(if (= (length notes) 2)
		    (first  notes)
		    'Non)))))))
;; Si deux sessions d'examens sont présentent
;; retourne la session 2, si une seule session est présente,
;; retoure la session 1, sinon retourne le symbole 'Non	    
(defun get-correct-session (alist)
  (let ((session1 (compute-exam alist "S1"))
	(session2 (compute-exam alist "S2")))
    (if (not (eq session1 'Non))
	(progn
	  (if (not (eq session2 'Non))
	      session2
	      session1))
	'Non)))

;; Calcule du CC
(defun compute-cc (DS TP)
  (if (eq DS 'Non)
      TP
      (if (eq TP 'Non)
	  DS
	  (/ (+ DS TP) 2))))

;; Note totale à l'UE (CC + Exam)
(defun compute-total-ue (alist)
  (let* ((TP (compute-examination alist "TP"))
	 (DS (compute-examination alist "DS"))
	 (CC (compute-cc DS TP))
	 (SESSION (get-correct-session alist)))
    (if (or (eq CC 'Non)
	    (eq SESSION 'Non))
	'Non
	(+ (* 0.75 SESSION) (* 0.25 (max CC SESSION))))))	 

;; Note totale du semestre en tenant compte des crédits de chaque UE
;; chaque UE est nécèssairement valide est existante, voir la commande
;; Ajouter Note.
(defun compute-total (list-notes)
  (let ((sum-notes 0)
	(sum-credits 0)
	(tmp-val  nil)
	(tmp-credits nil)) 
  (dolist (cell list-notes)
    (setf tmp-val (compute-total-ue (cdr cell)))
    (if (eq tmp-val 'Non)
	(return)
	(progn
	  (setf tmp-credits (get-credits-from-course (car cell) (current-buffer)))
	  (setf sum-notes   (+ sum-notes (* tmp-val tmp-credits)))
	  (setf sum-credits (+ sum-credits tmp-credits)))))
  (if (eq tmp-val 'Non)
      'Non
      (/ sum-notes sum-credits))))
	  


;; Quelques conditions (assez explicites)
(define-condition no-such-year (apex-condition) ()
		  (:report
		   (lambda (condition stream)
		     (declare (ignore condition))
		     (format stream "Not registered for given year"))))

(define-condition no-such-semester (apex-condition) ()
		  (:report
		   (lambda (condition stream)
		     (declare (ignore condition))
		     (format stream "Semester not found for the given year"))))

(define-condition no-such-type (apex-condition) ()
		  (:report
		   (lambda (condition stream)
		     (declare (ignore condition))
		     (format stream "type must be equal to DS, TP or Exam !"))))

;; Assertion sur le contenu d'un "type"
(defun assert-type (type)
  (if (not (or  (string= type "DS")
		(string= type "TP")
		(string= type "Exam")))
      (error 'no-such-type)))

;; Récapitulatif notes par semestre
(defmethod display-semester (pane buffer semester (view year-view))
  (let ((fn (funcall semester (year view))))
    (if fn
	(progn
	  (format pane "~%")
	  (formatting-table (pane)
	    (with-drawing-options (pane :ink +red+)
	      (formatting-row (pane)
		(formatting-cell (pane)
		  (format pane "UE"))
		(formatting-cell (pane)
		  (format pane "TP"))
		(formatting-cell (pane)
		  (format pane "DS"))
		(formatting-cell (pane)
		  (format pane "Session1"))
		(formatting-cell (pane)
		  (format pane "Session2"))
		(formatting-cell (pane)
		  (format pane "Total"))))
	    (let ((total 0))
	    (dolist (ue fn)
	      (formatting-row (pane)
		(formatting-cell (pane)
		  (format pane "~A" (car ue)))
		(formatting-cell (pane)
		  (format pane "~A" (compute-examination (cdr ue) "TP")))
		(formatting-cell (pane)
		  (format pane "~A" (compute-examination (cdr ue) "DS")))
		(formatting-cell (pane)
		  (format pane "~A" (compute-exam (cdr ue) "S1")))
		(formatting-cell (pane)
		  (format pane "~A" (compute-exam (cdr ue) "S2")))
		(formatting-cell (pane)
		  (format pane "~A" (compute-total-ue (cdr ue))))))
	    (formatting-row (pane)
	      (formatting-cell (pane)
		(format pane "---------------")))
	    (formatting-row (pane)
	      (formatting-cell (pane)
		(with-drawing-options (pane :ink +red+)
		  (format pane "Total Semestre:")))
	      (formatting-cell (pane)
		(setf total (compute-total fn))
		(format pane "~A" total))
	      (formatting-cell (pane)
		(if (eq total 'Non)
		    (format pane "Incomplet")
		    (progn
		      (if (>= total 10.0)
			  (progn
			    (with-drawing-options (pane :ink +green+)
			      (format pane "Validé")))
			  (progn
			    (with-drawing-options (pane :ink +red+)
			      (format pane "Non Validé")))))))))))))
	 
	      
  (format pane "~%"))
		
	  
(defmethod display-pane-view (frame pane buffer (view year-view))
  (let* ((person       (person (car (views *application-frame*))))
	 (registration (studentp person buffer)))
    (formatting-table (pane)
      (with-drawing-options (pane :ink +red+)
	(formatting-row (pane)
	  (formatting-cell (pane)
	    (format pane "Année: ~A" (name (academic-year (year view))))))
	(formatting-row (pane)
	  (formatting-cell (pane)
	    (format pane "Semestre 1 -> ~A" (name (first-semester (year view))))))))
    (display-semester pane buffer #'first-semester-notes view)
    (formatting-table (pane)
      (with-drawing-options (pane :ink +red+)
	(formatting-row (pane)
	  (formatting-cell (pane)
	    (format pane "Semestre 2 -> ~A" (name (second-semester (year view))))))))
    (display-semester pane buffer #'second-semester-notes view))
  
  (format pane "~%"))
      

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
		    (format pane "~A" (name (second-semester year)))))))))))
  (format pane "~%"))
			


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
     (sem     'string         :prompt "Semestre")
     (ue      'string         :prompt "Code UE")
     (type    'string         :prompt "Type (DS/TP/Exam)")
     (note    'float          :prompt "Note"))
  (let* ((registration (get-registration-with-id id (current-buffer)))
	(year (find (name year) (registered-years registration) :key (lambda(e) (name (academic-year e)))
		    :test #'string=)))
    (assert-type type)
    (valid-course ue (current-buffer))
    (if (not year)
	(error 'no-such-year))
    (if (and (not (string= sem (name (first-semester year))))
	     (not (string= sem (name (second-semester year)))))
	(error 'no-such-semester))
    (if (string= sem (name (first-semester year)))
	(push-note-s1 type ue note year)
	(push-note-s2 type ue note year))))
	  
	    
    
    
	  
;; Commande privée
;; La bonne chose à faire en théorie serait de creer une private-apex-table de commandes
;; ainsi les commandes privées le serait vraiment.
;; Les commandes privée ne doivent pas être utilisés, elle sont appelées uniquement
;; lors des clics.
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

;; Fait le lien entre un objet cliquable et une commande
;; (les objets sont dit cliquable grâce à la macro with-output-as-presentation)
(define-presentation-to-command-translator view-registration
    (registration com-reg-view global-apex-table)
  (object)
  `(,object))

(define-presentation-to-command-translator view-year
    (year com-view-year global-apex-table)
  (object)
  `(,object))
