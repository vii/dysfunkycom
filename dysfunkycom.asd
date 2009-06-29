(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cl-fad))

(loop for addon in (remove-if-not 'cl-fad:directory-pathname-p (cl-fad:list-directory "addons"))
      do
      (pushnew addon asdf:*central-registry* :test #'equalp))

(asdf:defsystem :dysfunkycom
  :name "dysfunkycom"
  :author "dysfunkycom"
  :version "prerelease"
  :description "ICFP 2009"

  :components (
	       (:module :src
			:components (
				     (:file "package")
				     (:file "matrix")
				     (:file "lu" :depends-on ("matrix"))
				     #-darwin (:file "visualise" :depends-on ("physics" "macros" "submission"))
				     (:file "orbit-vm" :depends-on ("package" "macros" "math" "constants" "physics"))
				     (:file "constants" :depends-on ("package"))
				     (:file "math" :depends-on ("package" "lu"))
				     (:file "macros" :depends-on ("package"))
				     (:file "physics" :depends-on ("constants"))
				     (:file "chaser" :depends-on ("physics" "macros" "apogee"))
				     (:file "submission" :depends-on ("constants" "orbit-vm"))
				     (:file "apogee" :depends-on ("orbit-vm"))
				     (:file "control" :depends-on ("physics" "macros" "apogee" "chaser"))
				     (:file "brute" :depends-on ("control"))
				     (:file "control-superburn" :depends-on ("control"))
				     (:file "control-problem-4" :depends-on ("control"))
				     )))

  :depends-on (
	       :iterate
	       :alexandria
	       :cl-fad
	       #-darwin :lispbuilder-sdl
	       :ieee-floats))


