(in-package :apex-gui)

(declaim (optimize (debug 3)))

(defclass apex-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (format pane "Pane name: ~s" (pane-name (master-pane pane))))

(defclass apex-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20))

(defclass apex-pane (esa-pane-mixin application-pane)
  ((contents :initform "hello" :accessor contents)))

(defmethod view ((pane apex-pane))
  (stream-default-view pane))

(defmethod (setf view) (new-view (pane apex-pane))
  (setf (stream-default-view pane) new-view))

(define-application-frame apex (esa-frame-mixin
				   standard-application-frame)
  ((%views :initform nil :initarg :views :accessor views)
   (%years :initform nil :initarg :years :accessor years))
  (:panes
   (window1 (let* ((my-pane 
		    (make-pane 'apex-pane
			       :default-view (pop (views *application-frame*))
			       :width 900 :height 400
			       :display-function 'display-my-pane
			       :command-table 'global-apex-table))
		   (my-info-pane
		    (make-pane 'apex-info-pane
			       :master-pane my-pane
			       :width 900)))
	      (setf (windows *application-frame*) (list my-pane))
	      (vertically ()
		(scrolling ()
		  my-pane)
		my-info-pane)))
   (window2 (let* ((my-pane 
		    (make-pane 'apex-pane
			       :default-view (pop (views *application-frame*))
			       :width 900 :height 400
			       :display-function 'display-my-pane
			       :command-table 'global-apex-table))
		   (my-info-pane
		    (make-pane 'apex-info-pane
			       :master-pane my-pane
			       :width 750)))
	      (push my-pane (windows *application-frame*))
	      (vertically ()
		(scrolling ()
		  my-pane)
		my-info-pane)))
   (minibuffer (make-pane 'apex-minibuffer-pane :width 750)))
  (:layouts
   (default (vertically ()
	      (horizontally () window1 window2)
	      minibuffer))
   (alternative (vertically ()
		  window1
		  minibuffer)))
  (:top-level (esa-top-level)))

(defmethod buffers ((application-frame apex))
  (remove-duplicates (mapcar (lambda (window) (buffer (view window)))
                             (windows application-frame))
                     :test #'eq))

(defmethod frame-current-buffer ((application-frame apex))
  (buffer (view (car (windows application-frame)))))

(defgeneric display-pane-view (frame pane buffer view))

(defun display-my-pane (frame pane)
  (display-pane-view frame pane (buffer (view pane)) (view pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Global Apex command table

(define-command-table global-apex-table
    :inherit-from (global-esa-table esa-io-table keyboard-macro-table help-table))

(defgeneric find-apex-command-table (view)
  (:method ((view view))
	   (find-command-table 'global-apex-table)))

(defmethod find-applicable-command-table ((frame apex))
  (find-apex-command-table (view (current-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; New view

(defun new-view (view)
  (push (view (current-window)) (views *application-frame*))
  (setf (view (current-window)) view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Prettify names

(defun pretty-name (object)
  (substitute #\Space #\_ (name object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Academic year

(define-presentation-method present
    (object (type academic-year) stream view &key)
  (format stream "~a" (pretty-name object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The base view used by all non-textual views

(defclass apex-view (view) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Person

(define-presentation-method present
    (object (type person) stream (view apex-view) &key)
  (format stream "~a" (pretty-name object)))

(define-presentation-method present
    (object (type person) stream (view textual-view) &key)
  (format stream "~a" (name object)))

(define-condition no-such-person (apex-condition) ()
		  (:report
		   (lambda (condition stream)
		     (declare (ignore condition))
		     (format stream "Unknown person"))))

(define-presentation-method accept
    ((type person) stream (view textual-view) &key)
  (multiple-value-bind (person success string)
      (handler-case (complete-input stream
                                    (lambda (so-far mode)
                                      (complete-from-possibilities
                                       so-far
                                       (people (current-buffer))
                                       '()
                                       :action mode
                                       :predicate (constantly t)
                                       :name-key #'name
                                       :value-key #'identity)))
		    (simple-parse-error () (error 'no-such-person)))
    (declare (ignore string))
    (if success person (error 'no-such-person))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Global view

(defclass global-view (apex-view)
  ((%buffer :initarg :buffer :reader buffer)))

(defmethod display-pane-view (frame pane buffer (view global-view))
  (declare (ignore frame))
  (loop for organization in (organizations buffer)
	do (with-output-as-presentation (pane organization 'organization)
	     (format pane "~a~%" (pretty-name organization)))))					

(define-command (com-global-view :name "Vue Globale" :command-table global-apex-table)
    ()
  (new-view (make-instance 'global-view
	      :buffer (current-buffer))))

(set-key 'com-global-view 'global-apex-table '(#\g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Organization view

(defclass organization-view (apex-view)
  ((%buffer :initarg :buffer :reader buffer)
   (%organization :initarg :organization :reader organization)))

(defmethod display-pane-view (frame pane buffer (view organization-view))
  (format pane "~a~%~%" (pretty-name (organization view)))
  (format *trace-output* "years: ~s~%" (years frame))
  (format pane "Filières organisées :~%~%")
  (loop for program in (teaching-programs (current-buffer))
	when (eq (organized-by program) (organization view))
	do (with-output-as-presentation (pane program 'teaching-program)
	     (format pane "    ~a~%" (pretty-name program))))
  (format pane "~%~%")
  (format pane "Unités d'enseignement offertes :~%~%")
  (format pane "    ")			; indent the table a little
  (formatting-table (pane)
    (with-drawing-options (pane :ink +red+)
      (formatting-row (pane)
	(formatting-cell (pane) (format pane "Code"))
	(formatting-cell (pane) (format pane "Crédits"))
	(formatting-cell (pane) (format pane "Intitulé"))))
    (loop for course in (sort (copy-list (courses buffer)) #'string< :key #'code)
	  when (eq (supplier course) (organization view))
	  do (formatting-row (pane)
	       (with-output-as-presentation (pane course 'course)
		 (formatting-cell (pane)
		   (format pane "~a" (code course)))
		 (formatting-cell (pane)
		   (format pane "~a" (credits course)))
		 (formatting-cell (pane)
		   (format pane "~a" (pretty-name course)))))))
  ;; this seems necessary in order to activate scrolling
  (format pane "~%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Person set view

(defclass person-set-view (apex-view)
  ((%buffer :initarg :buffer :reader buffer)))

(defun total-load (person year buffer)
  (loop for course in (courses buffer)
	sum (loop for instance in (instances course)
		  sum (if (eq (academic-year instance) year)
			  (loop for part in (parts instance)
				sum (loop for assignment in (assignments part)
					  sum (if (eq (person-in-charge assignment)
						      person)
						  (* (cost (course-part assignment))
						     (amount assignment))
						  0)))
			  0))))

(defmethod display-pane-view (frame pane buffer (view person-set-view))
  (declare (ignore frame))
  (formatting-table (pane :x-spacing "   " :multiple-columns 3)
    (loop for person in (sort (copy-list (people buffer)) #'string< :key #'name)
	  do (with-output-as-presentation (pane person 'person)
	       (formatting-row (pane) 
		 (formatting-cell (pane)
		   (format pane "~a~%" (pretty-name person)))))))
  ;; this seems necessary in order to activate scrolling
  (format pane "~%"))

(define-command
    (com-view-person-set :name "Voir Ensemble Personnes" :command-table global-apex-table)
    ()
  (new-view (make-instance 'person-set-view
			   :buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Courses view

(defclass course-set-view (apex-view)
  ((%buffer :initarg :buffer :reader buffer)))

(defmethod display-pane-view (frame pane buffer (view course-set-view))
  (declare (ignore frame))
  (format pane "Courses~%~%")
  (formatting-table (pane)
    (with-drawing-options (pane :ink +red+)
      (formatting-row (pane) 
	(formatting-cell (pane)
	  (format pane "Code"))
	(formatting-cell (pane)
	  (format pane "Crédits"))
	(formatting-cell (pane)
	  (format pane "Intitulé"))))
    (loop for course in (sort (copy-list (courses buffer)) #'string< :key #'name)
	  do (with-output-as-presentation (pane course 'course)
	       (formatting-row (pane) 
		 (formatting-cell (pane)
		   (format pane "~a" (code course)))
		 (formatting-cell (pane)
		   (format pane "~a" (credits course)))
		 (formatting-cell (pane)
		   (format pane "~a" (pretty-name course)))))))
  ;; this seems necessary in order to activate scrolling
  (format pane "~%"))

(define-command
    (com-view-course-set :name "Voir Ensemble UE" :command-table global-apex-table)
    ()
  (new-view (make-instance 'course-set-view
			   :buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Course view

(defclass course-view (apex-view)
  ((%buffer :initarg :buffer :reader buffer)
   (%course :initarg :course :reader course)))

(defmethod display-pane-view (frame pane buffer (view course-view))
  (let* ((course (course view))
	 (instances (instances course)))
    (format pane "~a (~a)~%" (pretty-name course) (code course))
    (unless (equal (remark course) "")
      (format pane "(~a)~%" (remark course)))
    (format pane "Fournisseur : ~a~%" (pretty-name (supplier course)))
    (format pane "Responsable : ")
    (present (person-in-charge course)'person :stream pane)
    (format pane "~%~%")
    (unless (null instances)
      (format pane "Instances :~%~%")
      (loop for instance in (sort (copy-seq instances) #'string<
				  :key (lambda (instance)
					 (name (academic-year instance))))
	    do (with-output-as-presentation (pane instance 'course-instance)
		 (format pane
			 "  Année : ~a Semestre ~a~%"
			 (name (academic-year instance))
			 (parity-semester instance)))
	    do (format pane "  Responsable : ")
	    do (present (person-in-charge instance) 'person :stream pane)
	    do (format pane "~%~%")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Course instance view

(defclass course-instance-view (apex-view)
  ((%buffer :initarg :buffer :reader buffer)
   (%instance :initarg :instance :reader instance)))

(define-presentation-type minutes-eqtd ())

(define-command
    (com-add-assign :name "Ajouter Affectation" :command-table global-apex-table)
    ((part 'course-part :prompt "Type"))
  (let ((person (accept 'person :prompt "Personne")))
    (push (make-instance 'assignment
	    :course-part part
	    :person-in-charge person
	    :amount 1)
	  (assignments part))))

(define-presentation-to-command-translator add-assign
    (course-part com-add-assign global-apex-table)
  (object)
  `(,object))

(defmethod display-pane-view (frame pane buffer (view course-instance-view))
  (let* ((instance (instance view))
	 (course (course instance)))
    (format pane "~a (~a)~%" (pretty-name course) (code course))
    (unless (equal (remark course) "")
      (format pane "(~a)~%" (remark course)))
    (format pane "Année : ~a  Semestre : ~a~%"
	    (name (academic-year instance))
	    (parity-semester instance))
    (format pane "Responsable : ")
    (present (person-in-charge instance) 'person :stream pane)
    (format pane "~%~%")
    (format pane "Enseignements :~%~%")
    ;; indent the table a bit
    (format pane "   ")
    (formatting-table (pane) 
      (with-drawing-options (pane :ink +red+)
	(formatting-row (pane)
	  (formatting-cell (pane)
	    (format pane "Type"))
	  (formatting-cell (pane)
	    (format pane "h (EqTD)"))))
      (loop for part in (parts instance)
	    do (formatting-row (pane)
		 (with-output-as-presentation (pane part 'course-part)
		   (formatting-cell (pane)
		     (format pane "~a" (pretty-name (teaching-type part)))))
		 (with-output-as-presentation
		     (pane (cost part) 'minutes-eqtd)
		   (formatting-cell (pane)
		     (format pane "~2dh~2,'0d"
			     (floor (cost part))
			     (mod (* (cost part) 60) 60))))
		 (formatting-cell (pane)
		   (if (null (assignments part))
		       (format pane "[pas d'affectations]")
		       (formatting-table (pane)
			 (loop for assignment in (assignments part)
			       do (formatting-row (pane)
				    (with-output-as-presentation
					(pane (person-in-charge assignment) 'person)
				      (formatting-cell (pane)
					(format pane "~a"
						(pretty-name (person-in-charge assignment)))))
				    (with-output-as-presentation
					(pane assignment 'assignment)
				      (formatting-cell (pane)
					(format pane "~a" (amount assignment))))))))))))
    (terpri pane)))
				  

(define-command-table course-instance-table
    :inherit-from (global-apex-table))

(defmethod find-apex-command-table ((view course-instance-view))
  (find-command-table 'course-instance-table))

(define-command (com-add-course-part :name "Ajouter Partie" :command-table course-instance-table)
    ((teaching-type 'teaching-type :prompt "Type d'enseignement")
     (cost 'number :prompt "Coût (h EqTD)"))
  (let ((instance (instance (view (current-window)))))
    (push (make-instance 'course-part
			 :course-instance instance
			 :teaching-type teaching-type
			 :cost cost)
	  (parts instance))))

(define-command
    (com-delete-course-part :name "Supprimer Partie" :command-table course-instance-table)
    ((part 'course-part :prompt "Type"))
  (setf (parts (course-instance part))
	(remove part (parts (course-instance part)) :test #'eq)))

(define-command
    (com-add-assignment :name "Ajouter Affectation" :command-table global-apex-table)
			;; course-instance-table)
    ((part 'course-part :prompt "Type")
     (person 'person :prompt "Personne")
     (amount 'number :prompt "Quantité"))
  (push (make-instance 'assignment
		       :course-part part
		       :person-in-charge person
		       :amount amount)
	(assignments part)))

(define-command
    (com-delete-assignment :name "Supprimer Affectation" :command-table course-instance-table)
    ((assignment 'assignment :prompt "Affectation"))
  (setf (assignments (course-part assignment))
	(remove assignment (assignments (course-part assignment)) :test #'eq)))

(define-command
    (com-change-assignment :name "Modifier Affectation" :command-table course-instance-table)
    ((assignment 'assignment :prompt "Affectation"))
  (let ((amount (accept 'number
			:prompt "Quantité"
			:default (amount assignment))))
    (setf (amount assignment) amount)))

(define-command (com-duplicate :name "Dupliquer" :command-table course-instance-table)
    ((part 'course-part :prompt "Type"))
  (let* ((assignment (car (assignments part))))
    (push (make-instance 'assignment
			 :course-part part
			 :person-in-charge (person-in-charge assignment)
			 :amount (amount assignment))
	  (assignments part))))

(define-command
    (com-pop-assignment :name "Dépiler Affectation" :command-table course-instance-table)
    ((part 'course-part :prompt "Type"))
  (pop (assignments part)))				    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Person view

(defclass person-view (apex-view)
  ((%buffer :initarg :buffer :reader buffer)
   (%person :initarg :person :reader person)))

;; Une instance person est un teacher ssi il existe
;; au moin un person-in-charge dans les UE (course, course-instance, assignment) 
;; du buffer principal portant le même nom
;; TODO: A finir (vérifier les course-instance et les assignment)
(defmethod teacherp ((view person-view))
  (let ((person (person view))
	(co (courses (buffer view))))
    (find (name person) co :key (lambda (c) (name (person-in-charge c))) :test #'string=)))

(defmethod display-pane-view (frame pane buffer (view person-view))
  (flet ((show-assignment (assignment)
	   (formatting-row (pane)
	     (formatting-cell (pane)
	       (format pane "~a"
		       (pretty-name (course (course-instance (course-part assignment))))))
	     (formatting-cell (pane)
	       (format pane "~a"
		       (pretty-name (teaching-type (course-part assignment)))))
	     (formatting-cell (pane)
	       (format pane "~3d"
		       (cost (course-part assignment))))
	     (formatting-cell (pane)
	       (format pane "~3d" (amount assignment)))
	     (formatting-cell (pane)
	       (format pane "~3d" (* (cost (course-part assignment)) (amount assignment)))))
	   (* (cost (course-part assignment)) (amount assignment))))
    (let ((person (person view)))
      (format pane "~a~%~%" (pretty-name person))
      (loop for year in (sort (copy-seq (years frame)) #'string< :key #'name)
	    do (format pane "Année universitaire : ~a~%" (name year))
	    do (format pane "~%")
	    do (format pane "   Enseignements~%~%")
	    do (format pane "   ")
	    do (formatting-table (pane)
		 (with-drawing-options (pane :ink +red+)
		   (formatting-row (pane)
		     (formatting-cell (pane)
		       (format pane "Unité d'enseignement"))
		     (formatting-cell (pane)
		       (format pane "Type"))
		     (formatting-cell (pane)
		       (format pane "Coût/unité"))
		     (formatting-cell (pane)
		       (format pane "Qté"))
		     (formatting-cell (pane)
		       (format pane "Coût"))))
		 (let ((sum (loop for course in (courses buffer)
				  sum (loop for instance in (instances course)
					    sum (if (eq (academic-year instance) year)
						    (loop for part in (parts instance)
							  sum (loop for assignment in (assignments part)
								    sum (if (eq (person-in-charge assignment)
										person)
									    (show-assignment assignment)
									    0)))
						    0)))))
		   (formatting-row (pane)
		     (formatting-cell (pane)
		       (format pane "----------"))
		     (formatting-cell (pane)
		       (format pane " "))
		     (formatting-cell (pane)
		       (format pane " "))
		     (formatting-cell (pane)
		       (format pane " "))
		     (formatting-cell (pane)
		       (format pane "---")))
		   (formatting-row (pane)
		     (formatting-cell (pane)
		       (format pane "Total"))
		     (formatting-cell (pane)
		       (format pane " "))
		     (formatting-cell (pane)
		       (format pane " "))
		     (formatting-cell (pane)
		       (format pane " "))
		     (formatting-cell (pane)
		       (format pane "~3d" sum)))))
				  
	    do (format pane "~%~%")))))

(define-command (com-view-person :name "Voir Personne" :command-table global-apex-table)
    ((person 'person :prompt "Personne"))
  (new-view (make-instance 'person-view
			   :buffer (current-buffer)
			   :person person)))

(define-presentation-to-command-translator view-person
    (person com-view-person global-apex-table)
  (object)
  `(,object))

(define-command-table person-table
    :inherit-from (global-apex-table))

(defmethod find-apex-command-table ((view person-view))
  (find-command-table 'person-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Teaching program view

(defclass teaching-program-view (apex-view)
  ((%buffer :initarg :buffer :reader buffer)
   (%teaching-program :initarg :teaching-program :reader teaching-program)))

(defun display-one-academic-year (pane buffer program academic-year)
  (format pane "Année Universitaire ~a~%" (name academic-year))
  (let ((batches (loop for batch in (batches buffer)
		       when (and (batch-in-program-p batch program)
				 (eq (academic-year batch) academic-year))
		       collect batch)))
    (format pane "   ") ; indent the table a bit
    (formatting-table (pane)
      (with-drawing-options (pane :ink +red+)
	(formatting-row (pane)
	  (formatting-cell (pane) (format pane "Semestre"))
	  (formatting-cell (pane) (format pane "Semestre"))
	  (formatting-cell (pane) (format pane "Nombre"))
	  (formatting-cell (pane) (format pane "H/E"))
	  (formatting-cell (pane) (format pane "Recettes")))
	(formatting-row (pane)
	  (formatting-cell (pane) (format pane "relatif"))
	  (formatting-cell (pane) (format pane "parité"))
	  (formatting-cell (pane) (format pane "d'inscrits"))
	  (formatting-cell (pane) (format pane ""))
	  (formatting-cell (pane) (format pane "(h EqTD)"))))
      (loop for batch in (sort batches #'< :key #'relative-semester)
	    do (formatting-row (pane)
		 (formatting-cell (pane)
		   (format pane "~a" (relative-semester batch)))
		 (formatting-cell (pane)
		   (format pane "~a" (parity-semester batch)))
		 (formatting-cell (pane)
		   (format pane "~a" (size batch)))
		 (formatting-cell (pane)
		   (format pane "~a" (revenue-per-student batch)))
		 (formatting-cell (pane)
		   (format pane "~6d" (revenue batch))))))))

(defmethod display-pane-view (frame pane buffer (view teaching-program-view))
  (declare (ignore frame))
  (let ((program (teaching-program view)))
    (format pane "~a~%" (pretty-name program))
    (format pane "Organisé par : ")
    (with-output-as-presentation (pane
				  (organized-by program)
				  'organization)
      (format pane "~a~%~%" (pretty-name (organized-by program))))
    (loop for year in (sort (copy-list (academic-years buffer)) #'string< :key #'name)
	  do (display-one-academic-year pane buffer program year)
	  do (format pane "~%~%"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Start the application

(defun apex-common (buffer new-process process-name width height)
  (let* ((view1 (make-instance 'course-set-view
		  :buffer buffer))
	 (view2 (make-instance 'person-set-view
		  :buffer buffer))
	 ;; Be careful about this because if there once year such as "2008-2009"
	 ;; year will be nil and frame will be instance with a nil list of year
	 ;; finally an "exception" is throw because pretty-name is unable to call esa-utils:name
	 ;; method on a nil year object (logic)
	 (year (find "2007-2008" (academic-years buffer) :key #'name :test #'string=)))
    (let ((frame (make-application-frame 'apex
                                         :buffer buffer
					 :views (list view1 view2)
					 :years (list year)
                                         :width width :height height)))
      (flet ((run ()
               (run-frame-top-level frame)))
        (if new-process
            (clim-sys:make-process #'run :name process-name)
            (run))))))    

(defun apex (&key new-process (process-name "Apex")
               (width 900) (height 600))
  "Start an Apex session with a fresh empty buffer" 
  (apex-common (make-instance 'buffer)
		  new-process process-name width height))

(defun edit-file (filename &key new-process (process-name "Apex")
                  (width 900) (height 600))
  "Start an Apex session editing a given file" 
  (apex-common (read-everything filename)
		  new-process process-name width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; I/O

(defmethod frame-make-buffer-from-stream ((frame apex) stream)
  (read-buffer-from-stream stream))

(defmethod frame-find-file :around ((application-frame apex) filepath)
  (declare (ignore filepath))
  (let* ((buffer (call-next-method))
         (view (make-instance 'global-view
                              :buffer buffer)))
    (new-view view)
    (setf (filepath buffer) filepath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Methods for accept and present

(define-condition no-such-organization (apex-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown organization"))))

(define-presentation-method present
    (object (type organization) stream (view textual-view) &key)
  (format stream "~a" (name object)))

(define-presentation-method accept
    ((type organization) stream (view textual-view) &key)
  (multiple-value-bind (organization success string)
      (handler-case (complete-input stream
                                    (lambda (so-far mode)
                                      (complete-from-possibilities
                                       so-far
                                       (organizations (current-buffer))
                                       '()
                                       :action mode
                                       :predicate (constantly t)
                                       :name-key #'name
                                       :value-key #'identity)))
        (simple-parse-error () (error 'no-such-organization)))
    (declare (ignore string))
    (if success organization (error 'no-such-organization))))

(define-condition no-such-teaching-program (apex-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown teaching-program"))))

(define-presentation-method accept
    ((type teaching-program) stream (view textual-view) &key)
  (multiple-value-bind (teaching-program success string)
      (handler-case (complete-input stream
                                    (lambda (so-far mode)
                                      (complete-from-possibilities
                                       so-far
                                       (teaching-programs (current-buffer))
                                       '()
                                       :action mode
                                       :predicate (constantly t)
                                       :name-key #'name
                                       :value-key #'identity)))
        (simple-parse-error () (error 'no-such-teaching-program)))
    (declare (ignore string))
    (if success teaching-program (error 'no-such-person))))

(define-condition no-such-gender (apex-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown gender"))))

(define-presentation-type gender ())

(define-presentation-method accept
    ((type gender) stream (view textual-view) &key)
  (multiple-value-bind (gender success string)
      (handler-case (complete-input stream
                                    (lambda (so-far mode)
                                      (complete-from-possibilities
                                       so-far
                                       '((:male . "male") (:female . "female"))
                                       '()
                                       :action mode
                                       :predicate (constantly t)
                                       :name-key #'cdr
                                       :value-key #'car)))
        (simple-parse-error () (error 'no-such-gender)))
    (declare (ignore string))
    (if success gender (error 'no-such-gender))))

(define-condition no-such-academic-year (apex-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown academic year"))))

(define-presentation-method accept
    ((type academic-year) stream (view textual-view) &key)
  (multiple-value-bind (academic-year success string)
      (handler-case (complete-input stream
                                    (lambda (so-far mode)
                                      (complete-from-possibilities
                                       so-far
                                       (academic-years (current-buffer))
                                       '()
                                       :action mode
                                       :predicate (constantly t)
                                       :name-key #'name
                                       :value-key #'identity)))
        (simple-parse-error () (error 'no-such-academic-year)))
    (declare (ignore string))
    (if success academic-year (error 'no-such-academic-year))))

(define-condition no-such-teaching-type (apex-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown teaching type"))))

(define-presentation-method accept
    ((type teaching-type) stream (view textual-view) &key)
  (multiple-value-bind (teaching-type success string)
      (handler-case (complete-input stream
                                    (lambda (so-far mode)
                                      (complete-from-possibilities
                                       so-far
                                       (teaching-types (current-buffer))
                                       '()
                                       :action mode
                                       :predicate (constantly t)
                                       :name-key #'name
                                       :value-key #'identity)))
        (simple-parse-error () (error 'no-such-teaching-type)))
    (declare (ignore string))
    (if success teaching-type (error 'no-such-teaching-type))))

(define-presentation-method present
    (object (type course) stream (view textual-view) &key)
  (format stream "~a" (name object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands and key bindings

(define-command (com-add-organization :name t :command-table global-apex-table)
    ((name 'string :prompt "Name"))
  (push (make-instance 'organization
		       :name name)
	(organizations (current-buffer))))

(define-command
    (com-add-program :name "Ajouter Filière" :command-table global-apex-table)
    ((name 'string :prompt "Nom")
     (organized-by 'organization :prompt "Organisé par"))
  (push (make-instance 'teaching-program
		       :name name
		       :organized-by organized-by)
	(teaching-programs (current-buffer))))

(define-command
    (com-add-person :name "Ajouter Personne" :command-table global-apex-table)
    ((name 'string :prompt "Nom")
     (gender 'gender :prompt "Sexe"))
  (push (make-instance 'person :name name :gender gender)
	(people (current-buffer))))

(define-command
    (com-add-course :name "Ajouter UE" :command-table global-apex-table)
    ((name 'string :prompt "Intitulé")
     (person-in-charge 'person :prompt "Responsable")
     (code 'string :prompt "Code")
     (supplier 'organization :prompt "Fournisseur")
     (credits 'number :prompt "Crédits"))
  (push (make-instance 'course
	  :name name
	  :person-in-charge person-in-charge
	  :code code
	  :supplier supplier
	  :credits credits)
	(courses (current-buffer))))

(define-command
    (com-change-course :name "Modifier UE" :command-table global-apex-table)
    ((course 'course :prompt "Unité d'enseignement"))
  (let ((name (accept 'string
		      :prompt "Intitulé"
		      :default (name course)
		      :insert-default t))
	(person-in-charge (accept 'person
				  :prompt "Responsable"
				  :default (person-in-charge course)
				  :insert-default t))
	(code (accept 'string
		      :prompt "Code"
		      :default (code course)
		      :insert-default t))
	(supplier (accept 'organization
			  :prompt "Fournisseur"
			  :default (supplier course)
			  :insert-default t))
	(credits (accept 'number
			 :prompt "Credits"
			 :default (credits course)
			 :insert-default t)))
    (setf (name course) name
	  (person-in-charge course) person-in-charge
	  (code course) code
	  (supplier course) supplier
	  (credits course) credits)))

(define-command
    (com-view-organization :name "Voir Organisation" :command-table global-apex-table)
    ((organization 'organization :prompt "Organisation"))
  (let* ((view (make-instance 'organization-view
		 :buffer (buffer (view (current-window)))
		 :organization organization)))
    (new-view view)))

(define-command
    (com-view-program :name "Voir Filière" :command-table global-apex-table)
    ((program 'teaching-program :prompt "Filière"))
  (let* ((view (make-instance 'teaching-program-view
		 :buffer (buffer (view (current-window)))
		 :teaching-program program)))
    (new-view view)))

(define-presentation-to-command-translator view-organization 
    (organization com-view-organization global-apex-table)
  (object)
  `(,object))

(define-presentation-to-command-translator view-program
    (teaching-program com-view-program global-apex-table)
  (object)
  `(,object))

(define-command
    (com-add-academic-year :name "Ajouter Anée Universitaire" :command-table global-apex-table)
    ((name 'string :prompt "Nom année"))
  (push (make-instance 'academic-year :name name)
	(academic-years (current-buffer))))

(define-command
    (com-add-batch :name "Ajouter Promotion" :command-table global-apex-table)
    ((program 'teaching-program :prompt "Filière")
     (relative-semester 'integer :prompt "Semestre relatif")
     (academic-year 'academic-year :prompt "Année Universitaire")
     (parity-semester 'integer :prompt "Semestre parité")
     (size 'integer :prompt "Nombre d'étudiants")
     (revenue-per-student 'real :prompt "H/E"))
  (push (make-instance 'batch
	  :teaching-program program
	  :relative-semester relative-semester
	  :academic-year academic-year
	  :parity-semester parity-semester
	  :size size
	  :revenue-per-student revenue-per-student)
	(batches (current-buffer))))

(define-command
    (com-add-remark :name "Ajouter Remarque" :command-table global-apex-table)
    ((object 'apex-object :prompt "Objet")
     (remark 'string :prompt "Remarque"))
  (setf (remark object) remark))

(define-command 
    (com-view-course :name "Voir UE" :command-table global-apex-table)
    ((course 'course :prompt "Unité d'enseignement"))
  (setf (view (second (windows *application-frame*)))
	(make-instance 'course-view
	  :buffer (current-buffer)
	  :course course)))

(define-presentation-to-command-translator view-course
    (course com-view-course global-apex-table)
  (object)
  `(,object))

(define-command
    (com-view-course-instance :name "Voir instance UE" :command-table global-apex-table)
    ((instance 'course-instance :prompt "Instance"))
  (setf (view (second (windows *application-frame*)))
	(make-instance 'course-instance-view
	  :buffer (current-buffer)
	  :instance instance)))

(define-presentation-to-command-translator view-course-instance
    (course-instance com-view-course-instance global-apex-table)
  (object)
  `(,object))

(define-command
    (com-add-teaching-type :name "Ajouter Type Enseignement" :command-table global-apex-table)
    ((name 'string :prompt "Nom"))
  (push (make-instance 'teaching-type :name name)
	(teaching-types (current-buffer))))

(define-command-table course-table
    :inherit-from (global-apex-table))

(defmethod find-apex-command-table ((view course-view))
  (find-command-table 'course-table))

(define-command
    (com-add-course-instance :name "Ajouter Instance" :command-table course-table)
    ((year 'academic-year :prompt "Année Universitaire")
     (parity-semester 'integer :prompt "Semestre (1 ou 2)")
     (person-in-charge 'person :prompt "Responsable"))
  (let ((course (course (view (current-window)))))
    (push (make-instance 'course-instance
			 :course course
			 :person-in-charge person-in-charge
			 :academic-year year
			 :parity-semester parity-semester)
	  (instances course))))

(define-command (com-return :name "Retour" :command-table global-apex-table)
    ()
  (unless (null (views *application-frame*))
    (setf (views *application-frame*)
	  (append (views *application-frame*)
		  (list (view (current-window)))))
    (setf (view (current-window))
	  (pop (views *application-frame*)))))

(set-key 'com-return 'global-apex-table '(#\r))
