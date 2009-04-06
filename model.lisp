(in-package :apex-model)

(declaim (optimize (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader programming

(defparameter *apex-readtable-v1* (copy-readtable))

(defun read-apex-object-v1 (stream char)
  (declare (ignore char))
  (apply #'make-instance (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-apex-object-v1 nil *apex-readtable-v1*)
(set-syntax-from-char #\] #\) *apex-readtable-v1*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printer programming

(defgeneric save-info (object)
  (:method-combination append :most-specific-last))

(defclass apex-object () ())

;;; should really use *print-readably*
(defparameter *print-for-file-io* nil)

(defmethod print-object ((obj apex-object) stream)
  (if *print-for-file-io*
      (pprint-logical-block (stream nil :prefix "[" :suffix "]")
	(format stream "~s ~2i" (class-name (class-of obj)))
	(loop for info in (save-info obj)
	      do (format stream
			 "~_~s ~W "
			 (car info)
			 (funcall (cadr info) obj))))
      (call-next-method)))

(defmacro define-save-info (type &body save-info)
  `(progn

     (defmethod save-info append ((obj ,type))
       ',save-info)
	  
     ,@(loop for info in save-info
	     collect
	     `(defmethod (setf ,(cadr info)) :after (new-stuff (obj ,type))
		(declare (ignore new-stuff))
		(setf (needs-saving (current-buffer)) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Name

(defclass name-mixin ()
  ((%name :initarg :name :accessor name)))

(define-save-info name-mixin
  (:name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remark

(defclass remark-mixin ()
  ((%remark :initform "" :initarg :remark :accessor remark)))

(define-save-info remark-mixin
  (:remark remark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Academic year

(defclass academic-year (name-mixin apex-object) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer

(defclass buffer (esa-buffer-mixin apex-object)
  ((%academic-years :initform '() :initarg :academic-years :accessor academic-years)
   (%organizations :initform '() :initarg :organizations :accessor organizations)
   (%teaching-types :initform '() :initarg :teaching-types :accessor teaching-types)
   (%people :initform '() :initarg :people :accessor people)
   (%teaching-programs :initform '()
		       :initarg :teaching-programs
		       :accessor teaching-programs)
   (%courses :initform '() :initarg :courses :accessor courses)
   (%batches :initform '() :initarg :batches :accessor batches)
   (%follows :initform '() :initarg :follows :accessor follows)))

(define-save-info buffer
  (:academic-years academic-years)
  (:organizations organizations)
  (:teaching-types teaching-types)
  (:people people)
  (:teaching-programs teaching-programs)
  (:courses courses)
  (:batches batches)
  (:follows follows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Date

(defclass date-mixin ()
  ((%date :initarg :date :reader date)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Address

;;; this class is currently not used. 

(defclass address (apex-object)
  ((%address :initarg :address :reader address)
   (%postal-code :initarg :postal-code :accessor postal-code)
   (%city :initarg :city :accessor city)))

(define-save-info address
  (:address address)
  (:postal-code postal-code)
  (:city city))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Person 

(defclass person
    (name-mixin remark-mixin apex-object)
  ((%date-of-birth :initform 0 :initarg :date-of-birth :reader date-of-birth)
   (%gender :initarg :gender :accessor gender)))

(define-save-info person
  (:date-of-birth date-of-birth)
  (:gender gender))

(defclass person-in-charge-mixin ()
  ((%person-in-charge :initarg :person-in-charge
		      :accessor person-in-charge
		      :initform nil)))

(define-save-info person-in-charge-mixin
  (:person-in-charge person-in-charge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Organization

(defclass organization (name-mixin apex-object) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Teaching-program

(defclass teaching-program (name-mixin apex-object)
  (;; if NIL, inherit the organized-by from the part-of teaching program
   (%organized-by :initform nil :initarg :organized-by :accessor organized-by)
   ;; NIL if not part of any program, otherwise another, more general,
   ;; teaching program
   (%part-of :initform nil :initarg :part-of :accessor part-of)))

(define-save-info teaching-program
  (:organized-by organized-by)
  (:part-of part-of))

(defmethod organized-by :around ((program teaching-program))
  (or (call-next-method)
      (organized-by (part-of program))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Batch.  We define a batch as the set of ALL students 
;;; enrolled at the same time, in the same teaching program,
;;; and following a particular relative semester of that 
;;; teaching program.  This is where we store the relative
;;; cost per student per year, because this number can 
;;; change from one year to another, so we can't associate
;;; it with the teaching program itself. 

(defclass batch (apex-object)
  ((%teaching-program :initarg :teaching-program :reader teaching-program)
   (%relative-semester :initarg :relative-semester :reader relative-semester)
   (%academic-year :initarg :academic-year :reader academic-year)
   (%parity-semester :initarg :parity-semester :reader parity-semester)
   (%size :initarg :size :accessor size)
   (%revenue-per-student :initarg :revenue-per-student :reader revenue-per-student)))

(define-save-info batch
  (:teaching-program teaching-program)
  (:relative-semester relative-semester)
  (:academic-year academic-year)
  (:parity-semester parity-semester)
  (:size size)
  (:revenue-per-student revenue-per-student))
	   
;;; A sub-batch is a subset of a batch or of another sub-batch. 
;;; The size of a sub-batch can be expressed either as a fraction
;;; of its parent (sub-) batch, or as an absolute number of students. 
(defclass sub-batch (name-mixin apex-object)
  ((%batch :initarg :batch :reader batch)
   ;; at least one of the following two fields much have a non-NIL value
   ;; which must then be a number.
   (%absolute-size :initform nil :initarg :absolute-size :accessor absolute-size)
   (%relative-size :initform nil :initarg :relative-size :accessor relative-size)))

(defmethod revenue-per-student ((sub-batch sub-batch))
  (revenue-per-student (batch sub-batch)))

(defmethod size ((sub-batch sub-batch))
  (or (absolute-size sub-batch)
      (* (relative-size sub-batch) (size (batch sub-batch)))))

(defun revenue (thing)
  (* (size thing) (revenue-per-student thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Teaching type

(defclass teaching-type (name-mixin apex-object) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Courses, course instances, parts of courses

;; Instance d'une UE de manière génèrale
(defclass course
    (name-mixin remark-mixin person-in-charge-mixin apex-object)
  ((%code :initarg :code :initform nil :accessor code)
   (%supplier :initarg :supplier :accessor supplier)
   (%credits :initarg :credits :accessor credits)
   (%description :initarg :description :initform "" :accessor description)
   (%instances :initform '() :initarg :instances :accessor instances)))

(define-save-info course
  (:code code)
  (:supplier supplier)
  (:credits credits)
  (:description description)
  (:instances instances))

(defclass assignment (remark-mixin person-in-charge-mixin apex-object)
  ((%course-part :initarg :course-part :accessor course-part)
   (%amount :initarg :amount :accessor amount)))

(define-save-info assignment
  (:course-part course-part)
  (:amount amount))

;; Instance d'une UE pour une année acadèmique donnée
(defclass course-instance
    (remark-mixin person-in-charge-mixin apex-object)
  ((%course :initarg :course :reader course)
   (%academic-year :initarg :academic-year :reader academic-year)
   (%parity-semester :initarg :parity-semester :reader parity-semester)
   (%parts :initform '() :initarg :parts :accessor parts)))

(define-save-info course-instance
  (:course course)
  (:academic-year academic-year)
  (:parity-semester parity-semester)
  (:parts parts))

(defclass course-part (remark-mixin apex-object)
  ((%course-instance :initarg :course-instance :reader course-instance)
   (%teaching-type :initarg :teaching-type :reader teaching-type)
   (%cost :initarg :cost :reader cost)
   (%assignments :initform '() :initarg :assignments :accessor assignments)
   (%instances :initform '() :initarg :instances :accessor instances)))

(define-save-info course-part
  (:course-instance course-instance)
  (:teaching-type teaching-type)
  (:assignments assignments)
  (:instances instances))

(defclass part-instance (remark-mixin person-in-charge-mixin apex-object)
  ((%part :initarg :part :reader part)
   (%follows :initform '() :initarg :follows :accessor follows)))

(define-save-info part-instance
  (:follows follows))

(defun total-number-of-students (course-part)
  (loop for follow in (follows (current-buffer))
	when (eq (part-instance follow) course-part)
	sum (size follow)))

(defclass lecture-stream (course-part) ; "serie"
  ())

(defclass group (course-part) ; "groupe de TD"
  ((%lecture-stream :initarg :lecture-stream :reader lecture-stream)))

(define-save-info group
  (:lecture-stream lecture-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Follow.  A follow object is used to indicate that a certain number
;;; of students of a particular batch are following a particular part
;;; instance.

(defclass follow (apex-object)
  (;; the batch or sub-batch 
   (%batch :initarg :batch :reader batch)
   ;; the part-instance the batch is following
   (%part-instance :initarg :part-instance :reader part-instance)
   ;; NIL means the price is the same as the cost
   (%explicit-price :initform nil :initarg :explicit-price :accessor explicit-price)))

(define-save-info follow
  (:batch batch)
  (:part-instance part-instance)
  (:explicit-price explicit-price))

(defgeneric cost (thing))

(defmethod cost ((part-instance part-instance))
  (cost (part part-instance)))

(defmethod cost ((follow follow))
  (* (cost (part-instance follow))
     (/ (size follow)
	(total-number-of-students (part-instance follow)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Diplomas

(defclass diploma (name-mixin)
  ())

(defclass acquired-diploma (date-mixin apex-object)
  ((%student :initarg :student :reader student)
   (%diploma :initarg :diploma :reader diploma)))

(define-save-info acquired-diploma
  (:student student)
  (:diploma diploma))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; I/O

(define-condition apex-condition (error) ())

(define-condition file-does-not-exist (apex-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "File does not exist"))))

(define-condition unknown-file-version (apex-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown file version"))))

(defparameter *readtables*
  `(("ISV1" . ,*apex-readtable-v1*)))

(defun read-everything (filename)
  (assert (probe-file filename) () 'file-does-not-exist)
  (with-open-file (stream filename :direction :input)
    (let* ((version (read-line stream))
           (readtable (cdr (assoc version *readtables* :test #'string=))))
      (assert readtable () 'unknown-file-version)
      (let ((*read-eval* nil)
            (*readtable* readtable))
        (read stream)))))

(defun read-buffer-from-stream (stream)
  (let* ((version (read-line stream))
         (readtable (cdr (assoc version *readtables* :test #'string=))))
    (assert readtable () 'unknown-file-version)
    (let ((*read-eval* nil)
          (*readtable* readtable))
      (read stream))))

(defmethod frame-save-buffer-to-stream (application-frame (buffer buffer) stream)
  (let ((*print-circle* t)
	(*print-for-file-io* t)
        (*package* (find-package :keyword)))
    (format stream "ISV1~%")
    (pprint buffer stream)
    (terpri stream)
    (finish-output stream)))

