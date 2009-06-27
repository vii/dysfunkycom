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
				     #-darwin (:file "visualise" :depends-on ("package"))
				     (:file "orbit-vm" :depends-on ("package"))
				     (:file "constants" :depends-on ("package"))
				     (:file "physics" :depends-on ("constants"))
				     )))

  :depends-on (
	       :iterate
	       :alexandria
	       :cl-fad
	       #-darwin :lispbuilder-sdl
	       :stefil
	       :ieee-floats))


