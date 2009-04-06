(asdf:defsystem :apex
;; Avoid total recompilations when McCLIM is updated
;;  :depends-on (:mcclim)
  :components
  ((:file "packages" :depends-on ())
   (:file "utilities" :depends-on ("packages"))
   (:file "model" :depends-on ("packages" "utilities"))
   (:file "gui" :depends-on ("packages" "utilities" "model"))
   (:file "features-model" :depends-on ("packages" "model"))
   (:file "features-gui"   :depends-on ("packages" "gui" "model" "features-model"))
   ))
