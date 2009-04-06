(in-package :apex-gui)

(defclass student-view (person-view)
  ((%student :initarg :student :reader get-student)))

(defmethod display-pane-view (frame pane buffer (view student-view))
  (format pane "N° Etudiant: ~2D~%~%" (id (get-student view)))
  (call-next-method))

(define-command (com-view-student :name "Voir Etudiant" 
				  :command-table global-apex-table)
    ((person 'person :prompt "Etudiant"))
  (new-view (make-instance 'student-view
			   :buffer  (current-buffer)
			   :person  person
			   :student person)))
