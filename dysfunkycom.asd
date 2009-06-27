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
				     #-darwin (:file "visualise" :depends-on ("physics" "macros"))
				     (:file "orbit-vm" :depends-on ("package"))
				     (:file "constants" :depends-on ("package"))
				     (:file "math" :depends-on ("package"))
				     (:file "macros" :depends-on ("package"))
				     (:file "physics" :depends-on ("constants"))
				     (:file "submission" :depends-on ("constants"))
				     (:file "control" :depends-on ("physics" "macros"))
				     )))

  :depends-on (
	       :iterate
	       :alexandria
	       :cl-fad
	       #-darwin :lispbuilder-sdl
	       :stefil
	       :ieee-floats))


