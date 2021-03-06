(defpackage :apex-utilities
  (:use :clim-lisp :clim-mop)
  (:export #:ninsert-element #:define-added-mixin
	   #:unicode-to-char #:char-to-unicode
	   ))

(defpackage :apex-model
    (:use :clim-lisp :apex-utilities :esa :esa-buffer :esa-io)
  (:shadow #:second #:number)
  (:export #:read-everything #:read-buffer-from-stream
	   #:apex-object
	   #:buffer #:buffer-extended #:courses #:people #:organizations 
	   #:teaching-programs #:get-registration-with-id #:notes
	   #:registration #:registrations #:registered-years
	   #:teaching-types
	   #:academic-years #:batches #:course-instances
	   #:organization
	   #:teaching-program #:organized-by #:part-of #:subprogram-p
	   #:academic-year
	   #:course 
	   #:supplier #:person-in-charge #:code #:credits #:instances
	   #:teaching-type #:semester
	   #:course-instance 
	   #:negociated-price #:parts #:part-instances
	   #:course-part #:part-instance #:fraction #:assignments
	   #:batch #:size #:relative-semester #:parity-semester 
	   #:revenue-per-student #:studentp
	   #:revenue #:batch-in-program-p
	   #:follow #:price #:cost
	   #:address #:postal-code #:city #:year #:first-semester 
	   #:second-semester #:note #:first-semester-notes #:push-note-s1
	   #:push-note-s2 #:valid-course #:get-credits-from-course
	   #:second-semester-notes
	   #:name
	   #:remark
	   #:instances
	   #:print-object
	   #:*print-for-file-io*
	   #:*readtables*
	   #:person #:teacher #:secretary #:id
	   #:amount #:assignment
	   #:apex-condition))

(defpackage :apex-gui
  (:use :clim-lisp :apex-utilities :apex-model :clim :esa :esa-buffer :esa-io)
  (:export #:apex #:edit-file))
